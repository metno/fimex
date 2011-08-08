/*
 * Fimex, CDMVerticalInterpolator.h
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Aug 1, 2011
 *      Author: Heiko Klein
 */

#ifndef CDMVERTICALINTERPOLATOR_H_
#define CDMVERTICALINTERPOLATOR_H_

/**
 * @headerfile "fimex/CDMVerticalInterpolator.h"
 */

#include <vector>
#include "fimex/mifi_constants.h"
#include "fimex/CDMReader.h"

namespace MetNoFimex
{

// forward declaration
struct VIntPimpl;
class CoordinateSystem;

/**
 * @brief Interpolation of vertical layers
 *
 * The CDMVerticalInterpolator can be used to interpolate vertical levels.
 *
 * @warning CDMVerticalInterpolator requires a valid coordinate-system. In addition, the vertical axis may
 * not be the unlimited dimension. Furthermore, the vertical layer may not depend
 * or more dimensions than x,y and time. The order of dimensions must be time, k, y, x.
 */
class CDMVerticalInterpolator: public MetNoFimex::CDMReader
{
public:
    /**
     * Initialize a vertical interpolator.
     *
     * @param dataReader the data-source
     * @param verticalType must be 'pressure'
     * @param interpolationMethod one of 'linear', 'log', 'loglog'
     * @param level1 the new vertical levels, for 'pressure', that is pressure in hPa
     * @param level2 only required for hybrid levels, not yet supported
     */
    CDMVerticalInterpolator(boost::shared_ptr<CDMReader> dataReader, std::string verticalType, std::string verticalInterpolationMethod, const std::vector<double> level1, const std::vector<double> level2);
    virtual ~CDMVerticalInterpolator();
    /**
     * retrieve data from the underlying dataReader and interpolate the values to the new vertical levels
     */
    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
private:
    boost::shared_ptr<CDMReader> dataReader_;
    boost::shared_ptr<VIntPimpl> pimpl_;
    boost::shared_ptr<Data> getPressureDataSlice(boost::shared_ptr<const CoordinateSystem> cs, const std::string& varName, size_t unLimDimPos);
};

}

#endif /* CDMVERTICALINTERPOLATOR_H_ */
