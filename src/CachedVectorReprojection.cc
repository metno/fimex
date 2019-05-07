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

#include "fimex/CachedVectorReprojection.h"

#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/interpolation.h"

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.CachedVectorReprojection");

void CachedVectorReprojection::reprojectValues(shared_array<float>& uValues, shared_array<float>& vValues, size_t size) const
{
    if (ox == 0 || oy == 0 || matrix.get() == 0) {
        LOG4FIMEX(logger, Logger::WARN, "CachedVectorReprojection not initialized, using identity");
        return;
    }
    size_t oz = size / (ox*oy);
    int errcode = mifi_vector_reproject_values_by_matrix_f(method, matrix.get(), &uValues[0], &vValues[0], ox, oy, oz);
    if (errcode != MIFI_OK)	throw CDMException("Error during reprojection of vector-values");
}

void CachedVectorReprojection::reprojectDirectionValues(shared_array<float>& angles, size_t size) const
{
    if (ox == 0 || oy == 0 || matrix.get() == 0) {
        LOG4FIMEX(logger, Logger::WARN, "CachedVectorReprojection not initialized, using identity");
        return;
    }
    size_t oz = size / (ox*oy);
    int errcode = mifi_vector_reproject_direction_by_matrix_f(method, matrix.get(), &angles[0], ox, oy, oz);
    if (errcode != MIFI_OK) throw CDMException("Error during reprojection of vector-direction-values");
}

} // namespace MetNoFimex
