/*
 * Fimex, AggregationReader.cc
 *
 * (C) Copyright 2013-2019, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
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
 */

#include "fimex/AggregationReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SliceBuilder.h"
#include "fimex/StringUtils.h"
#include "fimex/Type2String.h"

#include <cassert>
#include <memory>
#include <regex>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.AggregationReader");

} // namespace

AggregationReader::AggType AggregationReader::aggTypeFromText(const std::string& aggType)
{
    if (aggType == "joinExisting") {
        return AGG_JOIN_EXISTING;
    } else if (aggType == "union") {
        return AGG_UNION;
    } else {
        throw CDMException("aggregation type '" + aggType + "' not known");
    }
}

AggregationReader::AggregationReader(const std::string& aggregationType)
    : aggType_(aggTypeFromText(aggregationType))
{
}

AggregationReader::~AggregationReader() {}

void AggregationReader::addReader(CDMReader_p reader, const std::string& id)
{
    if (gDataReader_)
        throw CDMException("reference reader already set");
    if (!id.empty()) {
        readers_.push_back(std::make_pair(id, reader));
    } else {
        const std::string dummy_id = "reader_" + type2string(readers_.size());
        readers_.push_back(std::make_pair(dummy_id, reader));
    }
}

void AggregationReader::initAggregation()
{
    // find the reference-file, choose penultimate, no readers also possible
    if (readers_.size() == 1) {
        gDataReader_ = readers_.at(0).second;
    } else if (readers_.size() > 1) {
        gDataReader_ = readers_.at(readers_.size() - 2).second;
    }
    if (gDataReader_)
        *(this->cdm_) = gDataReader_->getCDM();

    if (readers_.size() <= 1) {
        // aggregation only with more than 1 reader
        return;
    }

    if (aggType_ == AGG_JOIN_EXISTING) {
        // join unlimited from joinExisting, remember unlimdim->datasource map
        const CDMDimension* uDim = cdm_->getUnlimitedDim();
        if (uDim == 0) {
            throw CDMException("cannot aggregate files with joinExisting without unlimited dimension");
        }
        // disable all cached information about variable-data for variables with unlimited dimension
        for (const CDMVariable& var : cdm_->getVariables()) {
            if (cdm_->hasUnlimitedDim(var)) {
                cdm_->getVariable(var.getName()).setData(DataPtr());
            }
        }
        const std::string& uDimName = uDim->getName();
        for (size_t i = 0; i < readers_.size(); ++i) {
            auto& id_rd = readers_[i];
            const CDMDimension* readerUdim = id_rd.second->getCDM().getUnlimitedDim();
            if (readerUdim == 0 || (readerUdim->getName() != uDimName)) {
                LOG4FIMEX(logger, Logger::INFO, "file '" << id_rd.first << "' does not have matching unlimited dimension: " << uDimName);
                id_rd.second.reset(); // no longer needed-
            } else {
                for (size_t j = 0; j < readerUdim->getLength(); ++j) {
                    readerUdimPos_.push_back(std::make_pair(i, j));
                }
            }
        }
        // change size of unlimited dimension
        CDMDimension& ulimDim = cdm_->getDimension(uDim->getName());
        ulimDim.setLength(readerUdimPos_.size());
    } else if (aggType_ == AGG_UNION) {
        // join variables/dimensions from union, remember variable->datasource map
        for (size_t ir = 0; ir < readers_.size(); ++ir) {
            auto& id_rd = readers_[ir];
            const CDM& rCdm = id_rd.second->getCDM();
            for (const CDMVariable& rv : rCdm.getVariables()) {
                const CDM::VarVec& knownVars = cdm_->getVariables();
                if (find_if(knownVars.begin(), knownVars.end(), CDMNameEqual(rv.getName())) == knownVars.end()) {
                    LOG4FIMEX(logger, Logger::INFO, "found new variable '" << rv.getName() << " in " << id_rd.first);
                    // check dimensions
                    const std::vector<std::string>& rshape = rv.getShape();
                    bool dimsOk = true;
                    for (const std::string& rshpdim : rshape) {
                        const CDMDimension& rdim = rCdm.getDimension(rshpdim);
                        if (cdm_->hasDimension(rshpdim)) {
                            CDMDimension& dim = cdm_->getDimension(rshpdim);
                            if (dim.isUnlimited()) {
                                if (!rdim.isUnlimited()) {
                                    dimsOk = false;
                                    LOG4FIMEX(logger, Logger::WARN, rv.getName() << " not unlimited in " << id_rd.first);
                                } else {
                                    if (rdim.getLength() > dim.getLength()) {
                                        dim.setLength(rdim.getLength());
                                    }
                                }
                            } else {
                                if (rdim.isUnlimited()) {
                                    dimsOk = false;
                                    LOG4FIMEX(logger, Logger::WARN, rv.getName() << " unlimited in " << id_rd.first);
                                } else {
                                    if (rdim.getLength() != dim.getLength()) {
                                        dimsOk = false;
                                        LOG4FIMEX(logger, Logger::WARN, rv.getName() << " changes size in dim " << rdim.getName() << " at " << id_rd.first);
                                    }
                                }
                            }
                        } else {
                            if (!rdim.isUnlimited() || !cdm_->getUnlimitedDim()) {
                                cdm_->addDimension(rdim);
                            } else {
                                dimsOk = false;
                                LOG4FIMEX(logger, Logger::WARN, "two unlimited dimensions: " << rdim.getName() << " at " << id_rd.first);
                            }
                        }
                    }
                    if (dimsOk) {
                        varReader_[rv.getName()] = ir;
                        cdm_->addVariable(rv);
                        for (const CDMAttribute& attr : rCdm.getAttributes(rv.getName())) {
                            cdm_->addAttribute(rv.getName(), attr);
                        }
                    }
                }
            }
        }
    } else {
        throw CDMException("bad aggregation type");
    }
}

DataPtr AggregationReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,uDim): (" << varName << "," << unLimDimPos << ")");
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData())
        return getDataSliceFromMemory(variable, unLimDimPos);

    if (!gDataReader_) {
        // no datasource, return empty
        return createData(CDM_NAT, 0);
    }

    if (aggType_ == AGG_JOIN_EXISTING) {
        if (cdm_->hasUnlimitedDim(variable) && (unLimDimPos < readerUdimPos_.size())) {
            LOG4FIMEX(logger, Logger::DEBUG,
                      "fetching data from " << readers_.at(readerUdimPos_.at(unLimDimPos).first).first << " at uDimPos "
                                            << readerUdimPos_.at(unLimDimPos).second);
            return readers_.at(readerUdimPos_.at(unLimDimPos).first).second->getDataSlice(varName, readerUdimPos_.at(unLimDimPos).second);
        }
    } else if (aggType_ == AGG_UNION) {
        const auto it = varReader_.find(varName);
        if (it != varReader_.end()) {
            auto& r = readers_.at(it->second);
            LOG4FIMEX(logger, Logger::DEBUG, "fetching data of " << varName << " from " << r.first);
            return r.second->getDataSlice(varName, unLimDimPos);
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "fetching data from default reader");
    return gDataReader_->getDataSlice(varName, unLimDimPos);
}

DataPtr AggregationReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,sb): (" << varName << ", sb)");
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData())
        return getDataSliceFromMemory(variable, sb);

    if (!gDataReader_) {
        // no datasource, return empty
        return createData(CDM_NAT, 0);
    }

    if (aggType_ == AGG_JOIN_EXISTING) {
        if (cdm_->hasUnlimitedDim(variable)) {
            // merge along the unlim-slices
            // this only works if the unlim dim is the highest (as in netcdf-3)
            const std::string& unLimDim = cdm_->getUnlimitedDim()->getName();
            const std::vector<std::string> dimNames = sb.getDimensionNames();
            const std::vector<size_t>& dimStart = sb.getDimensionStartPositions();
            const std::vector<size_t>& dimSize = sb.getDimensionSizes();
            // get the data along the unlimited dimension and join
            // unlimited dimension must be outer=first dimension!
            size_t unLimDimStart = 0;
            size_t unLimDimSize = 0;
            size_t unLimSliceSize = 1;
            for (size_t i = 0; i < dimNames.size(); ++i) {
                if (dimNames.at(i) == unLimDim) {
                    unLimDimStart = dimStart.at(i);
                    unLimDimSize = dimSize.at(i);
                } else {
                    unLimSliceSize *= dimSize.at(i);
                }
            }
            if (unLimDimSize == 0 || unLimSliceSize == 0) {
                return createData(variable.getDataType(), 0);
            }
            // read now each unlimdim-slice
            // slice that slice according to the other dimensions
            // join those slices
            DataPtr retData = createData(variable.getDataType(), unLimSliceSize * unLimDimSize, cdm_->getFillValue(varName));
            for (size_t i = 0; i < unLimDimSize; ++i) {
                LOG4FIMEX(logger, Logger::DEBUG,
                          "fetching data from " << readers_.at(readerUdimPos_.at(unLimDimStart + i).first).first << " at uDimPos "
                                                << readerUdimPos_.at(unLimDimStart + i).second);
                CDMReader_p reader = readers_.at(readerUdimPos_.at(i + unLimDimStart).first).second;
                SliceBuilder sbi(reader->getCDM(), varName);
                for (size_t j = 0; j < dimNames.size(); ++j) {
                    if (dimNames.at(j) == unLimDim) {
                        sbi.setStartAndSize(unLimDim, readerUdimPos_.at(i + unLimDimStart).second, 1);
                    } else {
                        sbi.setStartAndSize(dimNames.at(j), dimStart.at(j), dimSize.at(j));
                    }
                }
                DataPtr unLimDimData = reader->getDataSlice(varName, sbi);
                // gDataReader_->getDataSlice(varName, sb); //getDataSlice(varName, i+unLimDimStart);
                if (unLimDimData->size() != 0) {
                    assert(unLimDimData->size() == unLimSliceSize);
                    retData->setValues(i * unLimSliceSize, *unLimDimData);
                }
            }
            return retData;
        }
    } else if (aggType_ == AGG_UNION) {
        const auto it = varReader_.find(varName);
        if (it != varReader_.end()) {
            const auto& id_rd = readers_.at(it->second);
            LOG4FIMEX(logger, Logger::DEBUG, "fetching data of " << varName << " from " << id_rd.first);
            return id_rd.second->getDataSlice(varName, sb);
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "fetching data from default reader");
    return gDataReader_->getDataSlice(varName, sb);
}

} // namespace MetNoFimex
