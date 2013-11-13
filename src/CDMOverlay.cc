/*
 * Fimex, CDMOverlay.cc
 *
 * (C) Copyright 2013, met.no
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
#include "fimex/Logger.h"
#include "CDMMergeUtils.h"

using namespace std;

namespace MetNoFimex {

static LoggerPtr logger(getLogger("fimex.CDMOverlay"));

// ========================================================================

struct CDMOverlayPrivate {
    CDMReaderPtr readerB;
    CDMReaderPtr readerT;
    CDMInterpolatorPtr interpolatedT;

    CDM init(int gridInterpolationMethod);
};

// ========================================================================

CDMOverlay::CDMOverlay(CDMReaderPtr base, CDMReaderPtr top, int grim)
    : p(new CDMOverlayPrivate)
{
    p->readerB = base;
    p->readerT = top;
    *cdm_ = p->init(grim);
}

// ------------------------------------------------------------------------

DataPtr CDMOverlay::getDataSlice(const std::string &varName, size_t unLimDimPos)
{
    if (not cdm_->hasVariable(varName))
        THROW("variable '" << varName << "' unknown");
    
    DataPtr sliceB = p->readerB->getDataSlice(varName, unLimDimPos);

    const CDM& cdmT = p->interpolatedT->getCDM();
    if ((not cdm_->hasDimension(varName)) and cdmT.hasVariable(varName)) {
        DataPtr sliceT = p->interpolatedT->getDataSlice(varName, unLimDimPos);
        const double fillT = cdmT.getFillValue(varName);
        for (size_t i=0; i<sliceB->size(); ++i) {
            const double valueT = sliceT->getDouble(i);
            if (valueT != fillT)
                sliceB->setValue(i, valueT);
        }
    }
    return sliceB;
}

// ########################################################################

CDM CDMOverlayPrivate::init(int gridInterpolationMethod)
{
    string nameX, nameY;
    return makeMergedCDM(readerB, readerT, gridInterpolationMethod, interpolatedT, nameX, nameY);
}

} // namespace MetNoFimex
