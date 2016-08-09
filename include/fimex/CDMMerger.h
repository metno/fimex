/* -*- c++ -*-
 * Fimex, CDMMerger.h
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
 *  Created on: Aug 28, 2012
 *      Author: Alexander Bürger
 */

#ifndef fimex_CDMMerger_H
#define fimex_CDMMerger_H 1

#include "fimex/CDMBorderSmoothing.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/DataDecl.h"

namespace MetNoFimex {

struct CDMMergerPrivate;

/**
 * @headerfile fimex/CDMMerger.h
 */
/**
 * Allows merging data from an inner, "fine" grid onto data from an outer, "rough" grid.
 *
 * The resulting grid can either be an automatic extension of the
 * inner grid, or defined manually. A smoothing function is applied at
 * the outer border of the inner grid, by default a linear transition.
 *
 * Present limitations:
 *
 * - there is no check at all if other dimensions are compatible
 */
class CDMMerger : public CDMReader {
public:
    /**
     * Merge data from inner grid onto refined outer grid.
     */
    CDMMerger(boost::shared_ptr<CDMReader> inner, boost::shared_ptr<CDMReader> outer);

    /** Set the smooting function factory to be used.
     * Must be called before setting target grid.
     * \parameter smoothingFactory a factory for smoothing functors
     */
    void setSmoothing(CDMBorderSmoothing::SmoothingFactoryPtr smoothingFactory);

    /** Decide if the outer value shall be used if the inner value is undefined. Default: true.
     * Forwarded to CDMOverlay::setUseOuterIfInnerUndefined
     */
    void setUseOuterIfInnerUndefined(bool useOuter);

    /**
     * Keep outer variables, even if no inner variable with the same name exists. Default: false.
     * Forwarded to CDMOverlay-constructor, so needs to set before setTargetGrid.
     */
    void setKeepOuterVariables(bool keepOuterVariabes);

    /** Set grid interpolation method.
     * Must be called before setting target grid.
     * \parameter method one of MIFI_INTERPOL_*
     */
    void setGridInterpolationMethod(int method);

    /** Set target grid from string values, same as in CDMInterpolator.
     * Units must be either "m" (not "km", "cm", or so) or degrees. */
    void setTargetGrid(const std::string& proj, const std::string& tx_axis, const std::string& ty_axis,
            const std::string& tx_unit, const std::string& ty_unit,
            const std::string& tx_type, const std::string& ty_type);

    /** Set target grid from values, same as in CDMInterpolator.
     * Units must be either "m" (not "km", "cm", or so) or degrees. */
    void setTargetGrid(const std::string& proj, const std::vector<double>& tx, const std::vector<double>& ty,
            const std::string& tx_unit, const std::string& ty_unit,
            const CDMDataType& tx_type, const CDMDataType& ty_type);

    /** Set target grid as inner grid expanded to cover outer grid.
     *
     * The resulting grid will be an extension of the first simple spatial grid found in the inner reader.
     * It is constructed by extending the inner grid until it covers the outer grid.
     */
    void setTargetGridFromInner();

    using CDMReader::getDataSlice;
    virtual DataPtr getDataSlice(const std::string &varName, std::size_t unLimDimPos);

private:
    std::auto_ptr<CDMMergerPrivate> p;
};

typedef boost::shared_ptr<CDMMerger> CDMMergerPtr;

} // namespace MetNoFimex

#endif /* fimex_CDMMerger_H */
