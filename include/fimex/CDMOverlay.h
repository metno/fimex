/* -*- c++ -*-
 * Fimex, CDMOverlay.h
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Aug 28, 2012
 *      Author: Alexander Bürger
 */

#ifndef fimex_CDMOverlay_H
#define fimex_CDMOverlay_H 1

#include "fimex/CDMReader.h"
#include "fimex/DataDecl.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/mifi_constants.h"

namespace MetNoFimex {

struct CDMOverlayPrivate;

/**
 * @headerfile fimex/CDMOverlay.h
 */
/**
 * Takes two readers, base and top, and returns top when defined, else
 * base. Interpolates top to base's grid.
 *
 * Used in CDMMerger together with CDMBorderSmoothing, and maybe not
 * very useful elsewhere.
 */
class CDMOverlay : public CDMReader {
public:
    /**
     * Merge data from top grid onto base grid.
     */
    CDMOverlay(CDMReader_p base, CDMReader_p top,
            int gridInterpolationMethod = MIFI_INTERPOL_BILINEAR, bool keepOuterVariables = false);
    ~CDMOverlay();

    using CDMReader::getDataSlice;
    virtual DataPtr getDataSlice(const std::string &varName, std::size_t unLimDimPos);

private:
    std::unique_ptr<CDMOverlayPrivate> p;
};

typedef std::shared_ptr<CDMOverlay> CDMOverlay_p;

} // namespace MetNoFimex

#endif /* fimex_CDMOverlay_H */
