/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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
 */

#include "fimex/Null_CDMWriter.h"

#include "fimex/CDM.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MutexLock.h"
#include "fimex/SharedArray.h"
#include "fimex/Type2String.h"

#include "fimex_config.h"
#ifdef HAVE_MPI
#include "fimex/mifi_mpi.h"
#endif

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.Null_CDMWriter");

bool convertData(CDMDataType dt, DataPtr data)
{
    if (data->size() == 0)
        return true;

    // clang-format off
    switch (dt) {
    case CDM_NAT: return true;
    case CDM_CHAR:   data->asChar().get(); break;
    case CDM_UCHAR:  data->asUChar().get(); break;
    case CDM_SHORT:  data->asShort().get(); break;
    case CDM_USHORT: data->asUShort().get(); break;
    case CDM_INT:    data->asInt().get(); break;
    case CDM_UINT:   data->asUInt().get(); break;
    case CDM_INT64:  data->asInt64().get(); break;
    case CDM_UINT64: data->asUInt64().get(); break;
    case CDM_FLOAT:  data->asFloat().get(); break;
    case CDM_DOUBLE: data->asDouble().get(); break;
    case CDM_STRING: data->asString(); break;
    default: return false;
    }
    // clang-format on
    return true;
}

} // namespace

Null_CDMWriter::Null_CDMWriter(const CDMReader_p cdmReader, const std::string& outputFile)
    : CDMWriter(cdmReader, outputFile)
{
    const CDM& cdm = cdmReader->getCDM();
    const CDM::DimVec& cdmDims = cdm.getDimensions();
    // define dims
    for (CDM::DimVec::const_iterator it = cdmDims.begin(); it != cdmDims.end(); ++it) {
        int length = it->isUnlimited() ? 0 : it->getLength();
        length++;
    }

    // define vars
    const CDM::VarVec& cdmVars = cdm.getVariables();
    for (CDM::VarVec::const_iterator it = cdmVars.begin(); it != cdmVars.end(); ++it) {
        const CDMVariable& var = *it;
        const std::vector<std::string>& shape = var.getShape();
        for (size_t i = 0; i < shape.size(); i++) {
        }
        CDMDataType datatype = var.getDataType();
        if (datatype == CDM_NAT && shape.size() == 0) {
            datatype = CDM_INT;
        }
    }

    const CDM::StrAttrVecMap& cdmAttrs = cdm.getAttributes();
    for (CDM::StrAttrVecMap::const_iterator it = cdmAttrs.begin(); it != cdmAttrs.end(); ++it) {
        int varId;
        if (it->first == CDM::globalAttributeNS()) {
            varId = 0;
        } else {
            varId = 1;
        }
        for (CDM::AttrVec::const_iterator ait = it->second.begin(); ait != it->second.end(); ++ait) {
            const CDMAttribute& attr = *ait;
            if (!convertData(attr.getDataType(), attr.getData())) {
                throw CDMException("cannot convert datatype for attribute " + attr.getName());
            }
        }
    }

    // write data
    const CDMDimension* unLimDim = cdm.getUnlimitedDim();
    const long long maxUnLim = (unLimDim ? unLimDim->getLength() : 0);
#ifdef _OPENMP
#pragma omp parallel for default(shared)
#endif
    for (long long unLimDimPos = -1; unLimDimPos < maxUnLim; ++unLimDimPos) {
#ifdef HAVE_MPI
        if (mifi_mpi_initialized()) {
            // only work on variables which belong to this mpi-process (modulo-base)
            if ((unLimDimPos % mifi_mpi_size) != mifi_mpi_rank) {
                LOG4FIMEX(logger, Logger::DEBUG, "processor " << mifi_mpi_rank << " skipping on unLimDimPos " << unLimDimPos);
                continue;
            } else {
                LOG4FIMEX(logger, Logger::DEBUG, "processor " << mifi_mpi_rank << " working on unLimDimPos " << unLimDimPos);
            }
        }
#endif
        for (const CDMVariable& cdmVar : cdmVars) {
            const bool has_unlimited = cdm.hasUnlimitedDim(cdmVar);
            DataPtr data;
            if (unLimDimPos == -1 && !has_unlimited) {
                data = cdmReader->getData(cdmVar.getName());
            } else if (unLimDimPos >= 0 && has_unlimited) {
                data = cdmReader->getDataSlice(cdmVar.getName(), unLimDimPos);
            } else {
                continue;
            }
            if (!convertData(cdmVar.getDataType(), data)) {
                throw CDMException("problems writing data to var " + cdmVar.getName() + ": " + ", datalength: " + type2string(data->size()) +
                                   ", datatype: " + type2string(cdmVar.getDataType()));
            }
        }
    }
}

Null_CDMWriter::~Null_CDMWriter()
{
}

} // namespace MetNoFimex
