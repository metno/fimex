/*
 * Fimex, GribCDMReader.cc
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "GribCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "GribCDMIndexer.h"
#include "GribFileIndex.h"
#include "GribProtobufIndexReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/FileUtils.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/MutexLock.h"
#include "fimex/RecursiveSliceCopy.h"
#include "fimex/SliceBuilder.h"
#include "fimex/XMLUtils.h"

#include "fimex_grib_config.h"

#include <cassert>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.GribCDMReader");

template <typename T>
class RoundValue
{
private:
    T scale_;
    T scaleInv_;
    T initialMissing_;
    T finalMissing_;

public:
    RoundValue(double scale, double initialMissing, double finalMissing)
        : scale_(scale)
        , scaleInv_(1 / scale)
        , initialMissing_(initialMissing)
        , finalMissing_(finalMissing)
    {
    }
    T operator()(T in)
    {
        if (in == initialMissing_)
            return finalMissing_;
        return scale_ * MetNoFimex::round(scaleInv_ * in);
    }
};

template <typename T>
DataPtr roundData(shared_array<T> array, size_t n, double scale, double initialMissing, double finalMissing)
{
    std::transform(array.get(), array.get() + n, array.get(), RoundValue<T>(scale, initialMissing, finalMissing));
    return createData(n, array);
}

} // namespace

struct GribCDMReader::Impl
{
    Impl();

    ChunkReaderFactory_p ca;

    OmpMutex mutex;

    std::string root_path;
    std::shared_ptr<GribCDMIndexer::grib_indexed> grib_indexed;
};

GribCDMReader::Impl::Impl()
    : ca(createDefaultChunkReaderFactory())
    , grib_indexed(std::make_shared<GribCDMIndexer::grib_indexed>())
{
}

GribCDMReader::GribCDMReader(const std::vector<std::string>& fileNames, const XMLInput& configXML,
                             const std::vector<std::pair<std::string, std::string>>& members)
    : p_(new Impl())
{
    GribCDMIndexer grind(configXML, members, p_->ca);
    grind.load(fileNames);
    grind.build(cdm_, p_->grib_indexed);
}

GribCDMReader::GribCDMReader(const std::string& grbmlFileName, const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members)
    : p_(new Impl())
{
    GribCDMIndexer grind(configXML, members, p_->ca);
    grind.load(grbmlFileName);
    grind.build(cdm_, p_->grib_indexed);
}

GribCDMReader::GribCDMReader(const std::string& fiinFileName, const XMLInput& configXML)
    : p_(new Impl())
{
#ifdef HAVE_PROTOBUF
    readGribProtobufIndex(fiinFileName, *cdm_, *p_->grib_indexed);

    p_->root_path = removeFilename(fiinFileName);
    if (const auto doc = configXML.getXMLDoc()) {
        XPathNodeSet nodes(doc, "/cdm_fimex_index_reader_config/root_path");
        if (nodes.size() == 1) {
            p_->root_path = XmlCharPtr(xmlNodeGetContent(nodes[0])).to_string();
        }
    }
#else
    throw std::runtime_error("compiled without protobuf support");
#endif
}

GribCDMReader::~GribCDMReader() {}

DataPtr GribCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "fetching slicebuilder for variable " << varName);
    const CDMVariable& variable = cdm_->getVariable(varName);

    if (variable.getDataType() == CDM_NAT) {
        return createData(CDM_INT, 0); // empty
    }

    if (DataPtr mem = getDataSliceFromMemory(variable, sb)) {
#if 0
        LOG4FIMEX(logger, Logger::DEBUG, "variable " << varName << " has in-memory data");
#endif
        return mem;
    }

    const auto it_gv = p_->grib_indexed->grib_vars.find(varName);
    if (it_gv == p_->grib_indexed->grib_vars.end()) {
        throw CDMException("no grib message found for variable '" + varName + "'");
    }
    const auto& gv = it_gv->second;

    // shape can be (x, y [,ensemble], level [,time]), i.e. ensemble and time may be absent
    const auto& dimNames = sb.getDimensionNames();
    // FIXME reimplement this check
    // assert(dimNames.at(0) != p_->grib_indexed->timeDimName); // this would indicate shape in the wrong order
    const auto& dimSizes = sb.getDimensionSizes();
    const auto& dimStart = sb.getDimensionStartPositions();
    const auto& maxSizes = sb.getMaxDimensionSizes();

    size_t dim_e_start = 0, dim_e_end = 1, dim_e_len = 1; // ensemble dimension
    size_t dim_l_start = 0, dim_l_end = 1, dim_l_len = 1; // vertical dimension
    size_t dim_t_start = 0, dim_t_end = 1, dim_t_len = 1; // time dimension

    // time is the only possible unlimited dimension
    const bool has_time_dim = cdm_->hasUnlimitedDim(variable);
    // all variables have x+y+level, some have time, length of shape indicates ensemble dim
    const bool has_ensemble_dim = dimNames.size() == (has_time_dim ? 5 : 4);

    size_t shape_idx = 2;
    if (has_ensemble_dim) {
        dim_e_start = dimStart[shape_idx];
        dim_e_end = dimStart[shape_idx] + dimSizes[shape_idx];
        dim_e_len = maxSizes[shape_idx];
        shape_idx += 1;
    }

    // next comes always the vertical dimension
    dim_l_start = dimStart[shape_idx];
    dim_l_end = dimStart[shape_idx] + dimSizes[shape_idx];
    dim_l_len = maxSizes[shape_idx];
    shape_idx += 1;

    if (has_time_dim) {
        dim_t_start = dimStart[shape_idx];
        dim_t_end = dimStart[shape_idx] + dimSizes[shape_idx];
        dim_t_len = maxSizes[shape_idx];
        shape_idx += 1;
    }
    if (shape_idx != dimNames.size())
        throw CDMException("bad shape_idx");

    const size_t xySliceSize = dimSizes[0] * dimSizes[1]; // x and y are always the first two dimensions
    const size_t maxXySize = maxSizes[0] * maxSizes[1];
    const bool xyslice = (maxXySize != xySliceSize);

    const size_t sliceSize = xySliceSize * (dim_t_end - dim_t_start) * (dim_l_end - dim_l_start) * (dim_e_end - dim_e_start);

    // storage for complete data
    auto doubleArray = make_shared_array<double>(sliceSize);

    // prefill with missing values
    double missingValue;
    if (gv.has_precision()) {
        // varPrecision used, use default missing
        missingValue = MIFI_FILL_DOUBLE;
    } else {
        missingValue = cdm_->getFillValue(varName);
    }
    std::fill(&doubleArray[0], &doubleArray[sliceSize], missingValue);

    // storage for one layer, required only if making xy-slice
    shared_array<double> full_data_array;
    std::vector<size_t> orgSizes, orgSliceSize, newStart, newSizes;
    if (xyslice) {
        LOG4FIMEX(logger, Logger::DEBUG, "need xy slicing");
        full_data_array = make_shared_array<double>(maxXySize);

        orgSizes = {maxSizes.at(0), maxSizes.at(1)};
        orgSliceSize = {1, maxSizes.at(0)};
        newStart = {dimStart.at(0), dimStart.at(1)};
        newSizes = {dimSizes.at(0), dimSizes.at(1)};
    }

    // loop over messages selected in SliceBuilder
    for (size_t idx_t = dim_t_start; idx_t < dim_t_end; ++idx_t) {
        for (size_t idx_l = dim_l_start; idx_l < dim_l_end; ++idx_l) {
            for (size_t idx_e = dim_e_start; idx_e < dim_e_end; ++idx_e) {
                const size_t idx_msgs = idx_t * dim_l_len * dim_e_len + idx_l * dim_e_len + idx_e;
                const auto& msg = gv.messages.at(idx_msgs);
                if (!msg.is_valid()) {
                    continue;
                }

                const size_t idx_data =
                    ((idx_t - dim_t_start) * dim_l_len * dim_e_len + (idx_l - dim_l_start) * dim_e_len + (idx_e - dim_e_start)) * xySliceSize;
                double* data_out = &doubleArray[idx_data];
                double* grib_out = xyslice ? full_data_array.get() : data_out;
                size_t dataRead;
                {
#ifndef HAVE_GRIB_THREADSAFE
                    OmpScopedLock lock(p_->mutex);
#endif
                    const auto url = joinFilename(p_->root_path, p_->grib_indexed->grib_files.at(msg.file_index));
                    auto cr = p_->ca->readerFor(url);
                    dataRead = readGribData(cr, msg.message_start, msg.message_size, grib_out, maxXySize, missingValue);
                }
#if 0
                LOG4FIMEX(logger, Logger::DEBUG, "done reading variable");
#endif
                if (dataRead != maxXySize) {
                    LOG4FIMEX(logger, Logger::WARN, "unexpected data size " << dataRead << ", setting to missingValue");
                    std::fill(data_out, data_out + xySliceSize, missingValue);
                } else if (xyslice) { // slicing on xy-data
                    recursiveCopyMultiDimData(&full_data_array[0], data_out, orgSizes, orgSliceSize, newStart, newSizes);
                }
            }
        }
    }

    DataPtr data = createData(sliceSize, doubleArray);
    if (gv.has_precision()) {
        const auto dt = variable.getDataType();
        const auto fv = cdm_->getFillValue(varName);
        if (dt == CDM_FLOAT) {
            data = roundData(data->asFloat(), data->size(), gv.scale_factor, missingValue, fv);
        } else if (dt == CDM_DOUBLE) {
            data = roundData(data->asDouble(), data->size(), gv.scale_factor, missingValue, fv);
        } else {
            data = data->convertDataType(missingValue, 1, 0, dt, fv, gv.scale_factor, gv.add_offset);
        }
    }
    return data;
}

DataPtr GribCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "fetching unlim-slice " << unLimDimPos << " for variable " << varName);
    const CDMVariable& variable = cdm_->getVariable(varName);

    if (variable.getDataType() == CDM_NAT) {
        return createData(CDM_INT, 0); // empty
    }
    if (variable.hasData()) {
#if 0
        LOG4FIMEX(logger, Logger::DEBUG, "variable " << varName << " has in-memory data");
#endif
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    // only time can be unLimDim for grib
    SliceBuilder sb(*cdm_, varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        auto timeDim = cdm_->getUnlimitedDim();
        if (unLimDimPos >= timeDim->getLength()) {
            throw CDMException("requested time outside data-region");
        }
        sb.setStartAndSize(timeDim->getName(), unLimDimPos, 1);
    }
    return getDataSlice(varName, sb);
}

} // namespace MetNoFimex
