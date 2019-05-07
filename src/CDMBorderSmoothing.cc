/*
 * Fimex, CDMBorderSmoothing.cc
 *
 * (C) Copyright 2012-2013, met.no
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
 *
 *  Created on: Nov 5, 2013
 *      Author: Alexander Bürger
 */

#include "fimex/CDMBorderSmoothing.h"
#include "fimex/CDMBorderSmoothing_Linear.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/DataIndex.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"

#include "CDMMergeUtils.h"

using namespace MetNoFimex;
using namespace std;

namespace MetNoFimex {

static Logger_p logger(getLogger("fimex.CDMBorderSmoothing"));

// ========================================================================

struct CDMBorderSmoothingPrivate {
    CDMReader_p readerI;
    CDMReader_p readerO;
    CDMInterpolator_p interpolatedO;

    string nameX, nameY;

    bool useOuterIfInnerUndefined;
    CDMBorderSmoothing::SmoothingFactory_p smoothingFactory;
    int gridInterpolationMethod;

    CDM makeCDM();
};

// ========================================================================

CDMBorderSmoothing::CDMBorderSmoothing(CDMReader_p inner, CDMReader_p outer, int grim)
    : p(new CDMBorderSmoothingPrivate)
{
    p->readerI = inner;
    p->readerO = outer;

    p->useOuterIfInnerUndefined = true;
    p->smoothingFactory = CDMBorderSmoothing::SmoothingFactory_p(new CDMBorderSmoothing_LinearFactory());
    p->gridInterpolationMethod = grim;

    *cdm_ = p->makeCDM();
}

CDMBorderSmoothing::~CDMBorderSmoothing() {}

// ------------------------------------------------------------------------

void CDMBorderSmoothing::setSmoothing(SmoothingFactory_p smoothingFactory)
{
    p->smoothingFactory = smoothingFactory;
}

// ------------------------------------------------------------------------

void CDMBorderSmoothing::setUseOuterIfInnerUndefined(bool useOuter)
{
    p->useOuterIfInnerUndefined = useOuter;
}

// ------------------------------------------------------------------------

DataPtr CDMBorderSmoothing::getDataSlice(const std::string &varName, size_t unLimDimPos)
{
    if (not cdm_->hasVariable(varName))
        THROW("variable '" << varName << "' unknown in border smoothing");

    const CDM& cdmO = p->interpolatedO->getCDM();
    if (cdm_->hasDimension(varName) or not cdmO.hasVariable(varName))
        return p->readerI->getDataSlice(varName, unLimDimPos); // not scaled

    DataPtr sliceI = p->readerI->getScaledDataSlice(varName, unLimDimPos);
    DataPtr sliceO = p->interpolatedO->getScaledDataSlice(varName, unLimDimPos);

    const vector<string> &shape = cdm_->getVariable(varName).getShape();
    vector<size_t> dimSizes;
    int shapeIdxX = -1, shapeIdxY = -1;
    for(size_t i=0; i<shape.size(); ++i) {
        if (p->nameX == shape[i])
            shapeIdxX = i;
        else if (p->nameY == shape[i])
            shapeIdxY = i;

        const CDMDimension& dim = cdm_->getDimension(shape[i]);
        if (not dim.isUnlimited())
            dimSizes.push_back(dim.getLength());
    }
    
    if (dimSizes.empty())
        return sliceI;

    Smoothing_p smoothing = (*p->smoothingFactory)(varName);
    smoothing->setHorizontalSizes(dimSizes[shapeIdxX], dimSizes[shapeIdxY]);

    const DataIndex idx(dimSizes);
    
    vector<size_t> current(dimSizes.size(), 0);
    while (current.at(0) < dimSizes.at(0)) {
        const size_t pos = idx.getPos(current);
        const double valueI = sliceI->getDouble(pos);
        const double valueO = sliceO->getDouble(pos);
        double merged;
        if (mifi_isnan(valueI)) {
            merged = p->useOuterIfInnerUndefined ? valueO : MIFI_UNDEFINED_D;
        } else if (mifi_isnan(valueO) or (not smoothing.get())) {
            merged = valueI;
        } else {
            merged = (*smoothing)(current[shapeIdxX], current[shapeIdxY], valueI, valueO);
        }
        sliceO->setValue(pos, merged);
        
        size_t incIdx = 0;
        while (incIdx < current.size()) {
            current[incIdx] += 1;
            if (current[incIdx] >= dimSizes[incIdx]) {
                current[incIdx] = 0;
                incIdx += 1;
            } else {
                break;
            }
        }
        if (incIdx >= current.size())
            break;
    }

    double scale=1, offset=0;
    getScaleAndOffsetOf(varName, scale, offset);
    return sliceO->convertDataType(MIFI_UNDEFINED_D, 1, 0,
        cdm_->getVariable(varName).getDataType(),
        cdm_->getFillValue(varName), scale, offset);
}

// ########################################################################

CDM CDMBorderSmoothingPrivate::makeCDM()
{
    return makeMergedCDM(readerI, readerO, gridInterpolationMethod, interpolatedO, nameX, nameY);
}

} // namespace MetNoFimex
