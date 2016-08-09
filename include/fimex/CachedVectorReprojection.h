/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#ifndef CACHEDVECTORREPROJECTION_H_
#define CACHEDVECTORREPROJECTION_H_

#include <boost/shared_array.hpp>
#include "fimex/interpolation.h"

namespace MetNoFimex
{

class CachedVectorReprojection
{
public:
    CachedVectorReprojection() : method(MIFI_OK), ox(0), oy(0) {}
    CachedVectorReprojection(int method, boost::shared_array<double> matrix, int ox, int oy) : method(method), matrix(matrix), ox(ox), oy(oy) {}
    virtual ~CachedVectorReprojection() {}
    /**
     *  reproject the vector values
     *
     * @param uValues the values in x-direction. These will be changed in-place.
     * @param vValues the values in y-direction. These will be changed in-place.
     * @param size the size of both arrays
     */
    void reprojectValues(boost::shared_array<float>& uValues, boost::shared_array<float>& vValues, size_t size) const;
    /**
     * reproject directions given in angles in degree
     * @param angles direction of vector in each grid-cell, given in degree
     * @param size the size of the angles-array
     */
    void reprojectDirectionValues(boost::shared_array<float>& angles, size_t size) const;
    // @return size of the spatial plane in x-direction
    size_t getXSize() const {return ox;}
    // @return size of the spatial plane in y-direction
    size_t getYSize() const {return oy;}

private:
    int method;
    boost::shared_array<double> matrix;
    size_t ox;
    size_t oy;
};

}

#endif /*CACHEDVECTORREPROJECTION_H_*/
