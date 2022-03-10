/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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

#include "fimex/CachedVectorReprojection.h"

#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"

#include "reproject.h"

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.CachedVectorReprojection");

CachedVectorReprojection::CachedVectorReprojection(reproject::Matrix_cp matrix)
    : matrix(matrix)
    , ox(matrix->size_x)
    , oy(matrix->size_y)
{
}

void CachedVectorReprojection::reprojectValues(shared_array<float>& uValues, shared_array<float>& vValues, size_t size) const
{
    const size_t oz = size / (getXSize() * getYSize());
    reproject::vector_reproject_values_by_matrix_f(matrix, &uValues[0], &vValues[0], oz);
}

void CachedVectorReprojection::reprojectDirectionValues(shared_array<float>& angles, size_t size) const
{
    const size_t oz = size / (getXSize() * getYSize());
    reproject::vector_reproject_direction_by_matrix_f(matrix, &angles[0], oz);
}

} // namespace MetNoFimex
