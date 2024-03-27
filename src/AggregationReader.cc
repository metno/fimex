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
    } else if (aggType == "joinNew") {
        return AGG_JOIN_NEW;
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

/*
joinex:
- condition = variable has unlim dim
- var may appear in some readers only; fillvalue for other readers
- for other readers
  - must have same shape and same sizes (size except for unlim dim)
  - else fillvalue

union:
- not joined (different from has "unlim dim" if agg==union)
- not bad
- from first reader where it appears
- add only if dimension sizes and unlim-ness match with first reader that has the dimension
*/

/*
getData
- no reader: nullptr
- 1 reader: read from this reader
- more readers:
- if aggtype=join, check join condition, find reader by unlimdimpos, check dimensions, return data or fill
- other vars: find reader by varname (already qc-passed), return data
*/

void AggregationReader::addReader(CDMReader_p reader, const std::string& id)
{
    auto id_ = id;
    if (!id_.empty()) {
        id_ = "reader_" + type2string(readers_.size());
    }
    readers_.push_back(std::make_pair(id, reader));

    if (readers_.size() == 1) {
        addFirstReader(reader, id_);
    } else {
        addOtherReader(reader, id_);
    }
}

void AggregationReader::addFirstReader(CDMReader_p reader, const std::string& id)
{
    // start out with CDM of first reader
    *cdm_ = readers_.front().second->getCDM();

    if (aggType_ == AGG_JOIN_EXISTING) {
        if (const auto* uDim = cdm_->getUnlimitedDim()) {
            // disable all cached information about variable-data for variables with unlimited dimension
            // also update set of variables for join and union
            for (const auto& var : cdm_->getVariables()) {
                const auto& varName = var.getName();
                if (cdm_->hasUnlimitedDim(var)) {
                    cdm_->getVariable(varName).setData(nullptr);
                    LOG4FIMEX(logger, Logger::DEBUG, "adding jex var '" << varName << "' from reader '" << id << "'");
                    joinVars.insert(varName);
                }
            }

            extendJoinedUnLimDimBy(uDim->getLength());
        } else {
            // if first reader does not have unlimited dimension, some unlimited dimension may appear later
            extendJoinedUnLimDimBy(0);
        }
    } else if (aggType_ == AGG_JOIN_NEW) {
        if (const auto* uDim = cdm_->getUnlimitedDim()) {
            auto& aUdim = cdm_->getDimension(uDim->getName());
            aUdim.setUnlimited(false);
        }
        CDMDimension joinUdim(joinNewDim, 0);
        joinUdim.setUnlimited(true);
        cdm_->addDimension(joinUdim);

        // for all join variables, copy and change shape
        for (const auto& varName : joinVars) {
            if (cdm_->hasVariable(varName)) {
                auto& var = cdm_->getVariable(varName);
                auto shape = var.getShape(); // take a copy
                shape.push_back(joinNewDim);
                var.setShape(shape);
            }
        }

        extendJoinedUnLimDimBy(1);
    }

    for (const auto& v : cdm_->getVariables()) {
        const auto& varName = v.getName();
        if (!joinVars.count(varName)) {
            LOG4FIMEX(logger, Logger::DEBUG, "adding union var '" << varName << "' from reader '" << id << "'");
            unionReaders_[varName] = 0;
        }
    }
}

void AggregationReader::extendJoinedUnLimDimBy(size_t len)
{
    const size_t before = readerUdimPos_.empty() ? 0 : readerUdimPos_.back();
    readerUdimPos_.push_back(before + len);

    // change size of unlimited dimension
    if (auto uDim = cdm_->getUnlimitedDim()) {
        cdm_->getDimension(uDim->getName()).setLength(readerUdimPos_.back());
    }
}

namespace {

void copyAttributes(const CDM& from, const std::string& fromVarName, CDM& to)
{
    for (const auto& attr : from.getAttributes(fromVarName)) {
        to.addAttribute(fromVarName, attr);
    }
}

void copyVar(const CDM& from, const CDMVariable& fromVar, CDM& to)
{
    to.addVariable(fromVar);
    copyAttributes(from, fromVar.getName(), to);
}

} // namespace

void AggregationReader::addOtherReader(CDMReader_p reader, const std::string& id)
{
    CDM& aCdm = *cdm_;
    auto* aUdim = aCdm.getUnlimitedDim();

    auto& rCdm = reader->getCDM();
    auto* rUdim = rCdm.getUnlimitedDim();

    if (aggType_ == AGG_JOIN_EXISTING) {
        // join all variables with unlimited dimension

        if (!rUdim) {
            // added reader has no unlimited dimension
            LOG4FIMEX(logger, Logger::INFO, "reader '" << id << "' has no unlimited dimension");
            extendJoinedUnLimDimBy(0);
        } else if (aUdim && rUdim->getName() != aUdim->getName()) {
            // agg and added reader both have an unlimited dimension, but they have different names
            LOG4FIMEX(logger, Logger::WARN,
                      "reader '" << id << "' has unlimited dimension '" << rUdim->getName() << "' while aggregation has '" << aUdim->getName() << "'");
            extendJoinedUnLimDimBy(0);
        } else {
            if (!aUdim) {
                // aggregation does not have an unlimited dimension from before; add, and add variables
                aCdm.addDimension(*rUdim);
            } else {
                // aggregation and reader both have the same unlimited dimension
            }

            // add all variables with unlim dim to join
            for (const auto& rVar : rCdm.getVariables()) {
                const auto& rVarName = rVar.getName();
                if (aCdm.hasVariable(rVar.getName())) {
                    // variable already in CDM from another reader; all other checks in getData
                } else if (rCdm.hasUnlimitedDim(rVar)) {
                    bool dimsOk = true;
                    for (const auto& rDimName : rVar.getShape()) {
                        if (rDimName == rUdim->getName()) {
                            continue;
                        }
                        if (!aCdm.hasDimension(rDimName)) {
                            aCdm.addDimension(rCdm.getDimension(rDimName));
                        } else if (aCdm.getDimension(rDimName).getLength() != rCdm.getDimension(rDimName).getLength()) {
                            dimsOk = false;
                            LOG4FIMEX(logger, Logger::DEBUG, "dim '" << rDimName << "' from reader '" << id << "' has length mismatch");
                        }
                    }

                    if (dimsOk) {
                        copyVar(rCdm, rVar, aCdm);
                        const auto& rVarName = rVar.getName();
                        cdm_->getVariable(rVarName).setData(nullptr);
                        LOG4FIMEX(logger, Logger::DEBUG, "adding jex var '" << rVarName << "' from reader '" << id << "'");
                        joinVars.insert(rVarName);
                    }
                }
            }

            extendJoinedUnLimDimBy(rUdim->getLength());
        }
    } else if (aggType_ == AGG_JOIN_NEW) {
        // for all join variables not known already, copy and change shape
        for (const auto& rVar : rCdm.getVariables()) {
            const auto& varName = rVar.getName();
            if (!aCdm.hasVariable(varName) && joinVars.count(varName)) {
                auto aVar = rVar;
                auto shape = rVar.getShape(); // take a copy
                shape.push_back(joinNewDim);
                aVar.setShape(shape);
                aCdm.addVariable(aVar);
                copyAttributes(rCdm, varName, *cdm_);
            }
        }

        extendJoinedUnLimDimBy(1);
    }

    // remaining variables are treated as for "union"

    const size_t readerIndex = readers_.size() - 1;
    for (const auto& rVar : rCdm.getVariables()) {
        const auto& rVarName = rVar.getName();

        if (joinVars.count(rVarName)) {
            // variable is joined
            continue;
        }

        if (unionReaders_.count(rVarName)) {
            // variable coming from a previous reader
            continue;
        }

        // check that dimensions of shape have same length
        bool dimsOk = true;
        for (const auto& rshpdim : rVar.getShape()) {
            const auto& rDim = rCdm.getDimension(rshpdim);
            if (aCdm.hasDimension(rshpdim)) {
                CDMDimension& dim = aCdm.getDimension(rshpdim);
                if (dim.isUnlimited() != rDim.isUnlimited()) {
                    dimsOk = false;
                    LOG4FIMEX(logger, Logger::WARN, rVar.getName() << " differs in unlimited-ness in " << id);
                } else if (rDim.getLength() != dim.getLength()) {
                    dimsOk = false;
                    LOG4FIMEX(logger, Logger::WARN, rVar.getName() << " differs in length in " << id);
                }
            } else if (rDim.isUnlimited() && aCdm.getUnlimitedDim()) {
                dimsOk = false;
                LOG4FIMEX(logger, Logger::WARN, "two unlimited dimensions: " << rDim.getName() << " at " << id);
            } else {
                aCdm.addDimension(rDim); // FIXME this should wait until all dims are checked
            }
        }
        if (dimsOk) {
            LOG4FIMEX(logger, Logger::DEBUG, "adding union var '" << rVarName << "' from reader '" << id << "'");
            unionReaders_[rVarName] = readerIndex;
            copyVar(rCdm, rVar, aCdm);
        }
    }
}

void AggregationReader::initAggregation() {}

CDMReader_p AggregationReader::findJoinReader(size_t& unLimDimPos) const
{
    const auto it = std::upper_bound(readerUdimPos_.begin(), readerUdimPos_.end(), unLimDimPos);
    if (it == readerUdimPos_.end()) {
        // out of bounds for unlimited dim
        return nullptr;
    }

    const size_t idx = std::distance(readerUdimPos_.begin(), it);
    const size_t before = it == readerUdimPos_.begin() ? 0 : *(it - 1);
    unLimDimPos -= before;
    return readers_[idx].second;
}

bool AggregationReader::checkJoinExistingDims(CDMReader_p reader, const std::string& varName) const
{
    const auto& rCdm = reader->getCDM();
    if (!rCdm.hasVariable(varName)) {
        return false;
    }

    const auto& rShape = rCdm.getVariable(varName).getShape();
    if (rShape != cdm_->getVariable(varName).getShape()) {
        return false;
    }

    const auto& rUdim = rCdm.getUnlimitedDim();
    for (const auto& dim : rShape) {
        if (!rUdim || dim != rUdim->getName()) {
            if (cdm_->getDimension(dim).getLength() != rCdm.getDimension(dim).getLength()) {
                return false;
            }
        }
    }

    return true;
}

bool AggregationReader::checkJoinNewDims(CDMReader_p reader, const std::string& varName) const
{
    const auto& rCdm = reader->getCDM();
    if (!rCdm.hasVariable(varName)) {
        return false;
    }

    auto aShape = cdm_->getVariable(varName).getShape();
    aShape.pop_back();

    const auto& rShape = rCdm.getVariable(varName).getShape();
    if (rShape != aShape) {
        return false;
    }

    for (const auto& dim : rShape) {
        if (cdm_->getDimension(dim).getLength() != rCdm.getDimension(dim).getLength()) {
            return false;
        }
    }

    return true;
}

CDMReader_p AggregationReader::findUnionReader(const std::string& varName) const
{
    const auto it = unionReaders_.find(varName);
    if (it != unionReaders_.end()) {
        const auto& id_rd = readers_.at(it->second);
        return id_rd.second;
    }
    return nullptr;
}

DataPtr AggregationReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,uDim): (" << varName << "," << unLimDimPos << ")");
    const CDMVariable& aVar = cdm_->getVariable(varName);
    if (aVar.hasData()) {
        return getDataSliceFromMemory(aVar, unLimDimPos);
    }

    if (joinVars.count(varName)) {
        if (auto rd = findJoinReader(unLimDimPos)) {
            if (aggType_ == AGG_JOIN_EXISTING) {
                if (checkJoinExistingDims(rd, varName)) {
                    return rd->getDataSlice(varName, unLimDimPos);
                } else {
                    auto aUdim = cdm_->getUnlimitedDim();
                    size_t unLimSliceSize = 1;
                    for (const auto& dim : aVar.getShape()) {
                        if (!aUdim || dim != aUdim->getName()) {
                            unLimSliceSize *= cdm_->getDimension(dim).getLength();
                        }
                    }
                    return createData(aVar.getDataType(), unLimSliceSize, cdm_->getFillValue(varName));
                }
            } else if (aggType_ == AGG_JOIN_NEW) {
                if (checkJoinNewDims(rd, varName)) {
                    // read everything
                    SliceBuilder sb(rd->getCDM(), varName);
                    return rd->getDataSlice(varName, sb);
                } else {
                    // set everything to fillValue
                    size_t unLimSliceSize = 1;
                    for (const auto& dim : aVar.getShape()) {
                        unLimSliceSize *= cdm_->getDimension(dim).getLength();
                    }
                    return createData(aVar.getDataType(), unLimSliceSize, cdm_->getFillValue(varName));
                }
            }
        }

        return nullptr; // out of bounds for unlimited dim
    }

    // all other variables behave as for "union"
    if (auto rd = findUnionReader(varName)) {
        return rd->getDataSlice(varName, unLimDimPos);
    }

    LOG4FIMEX(logger, Logger::ERROR, "no reader for aggregation/union of variable '" << varName << "'");
    return nullptr;
}

DataPtr AggregationReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,sb): (" << varName << ", sb)");
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData())
        return getDataSliceFromMemory(variable, sb);

    if (joinVars.count(varName)) {
        // merge along the unlim-slices
        // this only works if the unlim dim is the highest (as in netcdf-3)
        const std::string& uDimName = cdm_->getUnlimitedDim()->getName();
        const std::vector<std::string> dimNames = sb.getDimensionNames();
        const std::vector<size_t>& dimStart = sb.getDimensionStartPositions();
        const std::vector<size_t>& dimSize = sb.getDimensionSizes();
        // get the data along the unlimited dimension and join
        // unlimited dimension must be outer=first dimension!
        size_t unLimDimStart = 0;
        size_t unLimDimSize = 0;
        size_t unLimSliceSize = 1;
        for (size_t i = 0; i < dimNames.size(); ++i) {
            if (dimNames.at(i) == uDimName) {
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
            size_t unLimDimPos = unLimDimStart + i;
            if (auto reader = findJoinReader(unLimDimPos)) {
                DataPtr sliceData;
                if (aggType_ == AGG_JOIN_EXISTING) {
                    if (checkJoinExistingDims(reader, varName)) {
                        SliceBuilder sbi(reader->getCDM(), varName);
                        for (size_t j = 0; j < dimNames.size(); ++j) {
                            if (dimNames[j] == uDimName) {
                                sbi.setStartAndSize(dimNames[j], unLimDimPos, 1);
                            } else {
                                sbi.setStartAndSize(dimNames[j], dimStart[j], dimSize[j]);
                            }
                        }
                        sliceData = reader->getDataSlice(varName, sbi);
                    }
                } else if (aggType_ == AGG_JOIN_NEW) {
                    if (checkJoinNewDims(reader, varName)) {
                        SliceBuilder sbi(reader->getCDM(), varName);
                        for (size_t j = 0; j < dimNames.size(); ++j) {
                            if (dimNames[j] != uDimName) {
                                sbi.setStartAndSize(dimNames[j], dimStart[j], dimSize[j]);
                            }
                        }
                        sliceData = reader->getDataSlice(varName, sbi);
                    }
                }
                if (sliceData && sliceData->size() != 0) {
                    assert(sliceData->size() == unLimSliceSize);
                    retData->setValues(i * unLimSliceSize, *sliceData);
                }
            }
        }
        return retData;
    }

    // all other variables behave as for "union"
    if (auto rd = findUnionReader(varName)) {
        return rd->getDataSlice(varName, sb);
    }

    LOG4FIMEX(logger, Logger::ERROR, "no reader for aggregation/union of variable '" << varName << "'");
    return nullptr;
}

} // namespace MetNoFimex
