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

#include "fimex/CDMconstants.h"
#include "fimex/DataIndex.h"
#include "fimex/Logger.h"

#include "CDMMergeUtils.h"

using namespace MetNoFimex;
using namespace std;

namespace MetNoFimex {

static LoggerPtr logger(getLogger("fimex.CDMBorderSmoothing"));

// ========================================================================

struct CDMBorderSmoothingPrivate {
    CDMReaderPtr readerI;
    CDMReaderPtr readerO;
    CDMInterpolatorPtr interpolatedO;

    string nameX, nameY;

    bool useOuterIfInnerUndefined;
    CDMBorderSmoothing::SmoothingFactoryPtr smoothingFactory;
    int gridInterpolationMethod;

    CDM makeCDM();
};

// ========================================================================

CDMBorderSmoothing::CDMBorderSmoothing(CDMReaderPtr inner, CDMReaderPtr outer, int grim)
    : p(new CDMBorderSmoothingPrivate)
{
    p->readerI = inner;
    p->readerO = outer;

    p->useOuterIfInnerUndefined = true;
    p->smoothingFactory = CDMBorderSmoothing::SmoothingFactoryPtr(new CDMBorderSmoothing_LinearFactory());
    p->gridInterpolationMethod = grim;

    *cdm_ = p->makeCDM();
}

// ------------------------------------------------------------------------

void CDMBorderSmoothing::setSmoothing(SmoothingFactoryPtr smoothingFactory)
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
        THROW("variable '" << varName << "' unknown");

    DataPtr sliceI = p->readerI->getDataSlice(varName, unLimDimPos);

    const CDM& cdmO = p->interpolatedO->getCDM();
    if (cdm_->hasDimension(varName) or not cdmO.hasVariable(varName))
        return sliceI;

    DataPtr sliceO = p->interpolatedO->getDataSlice(varName, unLimDimPos);

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
    
    SmoothingPtr smoothing = (*p->smoothingFactory)(varName);
    const double fillI = cdm_->getFillValue(varName), fillO = cdmO.getFillValue(varName);
    smoothing->setFillValues(fillI, fillO);
    smoothing->setHorizontalSizes(dimSizes[shapeIdxX], dimSizes[shapeIdxY]);

    const DataIndex idx(dimSizes);
    
    vector<size_t> current(dimSizes.size(), 0);
    while (current.at(0) < dimSizes.at(0)) {
        const size_t pos = idx.getPos(current);
        const double valueI = sliceI->getDouble(pos);
        const double valueO = sliceO->getDouble(pos);
        double merged;
        if (valueI == fillI) {
            merged = p->useOuterIfInnerUndefined ? valueO : fillO;
        } else if (valueO == fillO or (not smoothing.get())) {
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
    return sliceO;
}

// ########################################################################

CDM CDMBorderSmoothingPrivate::makeCDM()
{
    return makeMergedCDM(readerI, readerO, gridInterpolationMethod, interpolatedO, nameX, nameY);
}

} // namespace MetNoFimex
