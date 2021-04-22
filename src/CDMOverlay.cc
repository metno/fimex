/*
 * Fimex, CDMOverlay.cc
 *
 * (C) Copyright 2013-2021, met.no
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
 *  Created on: Nov 6, 2013
 *      Author: Alexander Bürger
 */

#include "fimex/CDMOverlay.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"

#include "CDMMergeUtils.h"

using namespace std;

namespace MetNoFimex {

static Logger_p logger(getLogger("fimex.CDMOverlay"));

// ========================================================================

struct CDMOverlayPrivate {
    CDMReader_p readerB;
    CDMReader_p readerT;
    CDMInterpolator_p interpolatedB;

    CDM init(int gridInterpolationMethod, bool keepOuterVariables);
};

// ========================================================================

// base == big, top == small
CDMOverlay::CDMOverlay(CDMReader_p base, CDMReader_p top, int grim, bool keepOuterVariables)
    : p(new CDMOverlayPrivate)
{
    p->readerB = base;
    p->readerT = top;
    *cdm_ = p->init(grim, keepOuterVariables);
}

CDMOverlay::~CDMOverlay() {}

// ------------------------------------------------------------------------

DataPtr CDMOverlay::getDataSlice(const std::string &varName, size_t unLimDimPos)
{
    // use cdmB if not defined in cdmT
    // get simple coordinate variables from readerT
    if (cdm_->hasDimension(varName) and p->readerT->getCDM().hasVariable(varName)) {
        // read dimension variables from top
        return p->readerT->getDataSlice(varName, unLimDimPos); // not scaled
    }

    if (not p->readerT->getCDM().hasVariable(varName)) {
        // use complete base-data
        return p->interpolatedB->getDataSlice(varName, unLimDimPos);
    }

    const CDM& cdmB = p->interpolatedB->getCDM();
    if (not cdmB.hasVariable(varName))
        THROW("variable '" << varName << "' unknown in base");

    const std::string unitsT = p->readerT->getCDM().getUnits(varName);
    const std::string unitsB = p->interpolatedB->getCDM().getUnits(varName);
    DataPtr sliceT;
    const bool emptyUnits = unitsT.empty() || unitsB.empty();
    if (emptyUnits || unitsT == unitsB) {
        if (emptyUnits) {
            LOG4FIMEX(logger, Logger::WARN,
                      "no unit conversion for variable '" << varName << "': units '" << unitsT << "' in top and '" << unitsB << "' in base");
        }
        sliceT = p->readerT->getScaledDataSlice(varName, unLimDimPos);
    } else {
        LOG4FIMEX(logger, Logger::INFO, "unit conversion for variable '" << varName << "' from '" << unitsT << "' in top to '" << unitsB << "' in base");
        sliceT = p->readerT->getScaledDataSliceInUnit(varName, unitsB, unLimDimPos);
    }
    DataPtr sliceB = p->interpolatedB->getScaledDataSlice(varName, unLimDimPos);

    for (size_t i=0; i<sliceB->size(); ++i) {
      const double valueT = sliceT->getDouble(i);
      if (not mifi_isnan(valueT))
        sliceB->setValue(i, valueT);
    }

    double scale=1, offset=0;
    getScaleAndOffsetOf(varName, scale, offset);
    return sliceB->convertDataType(MIFI_UNDEFINED_D, 1, 0,
        cdm_->getVariable(varName).getDataType(),
        cdm_->getFillValue(varName), scale, offset);
}

// ########################################################################

CDM CDMOverlayPrivate::init(int gridInterpolationMethod, bool keepOuterVariables)
{
    string nameX, nameY;
    return makeMergedCDM(readerT, readerB, gridInterpolationMethod, interpolatedB, nameX, nameY, keepOuterVariables);
}

} // namespace MetNoFimex
