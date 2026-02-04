/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#ifndef FIMEX_RECURSIVESLICECOPY_H
#define FIMEX_RECURSIVESLICECOPY_H

#include <algorithm>
#include <vector>

namespace MetNoFimex {

/**
 * recursively copy data by moving the newData and orgData pointers forward and copy the data at the current position
 *
 * it's assumed that the first dim in the vector is the fastest moving (fortran like)
 *
 * @param orgData pointer to the current postion of the original array
 * @param newData pointer to the current position of the new array
 * @param orgDimSize the original dimensions of orgData
 * @param orgSliceSize helper-array with orgSliceSize[0] = 1; orgSliceSize[n] = orgDimSize[n] * orgSliceSize[n-1]
 * @param newStart the start positions in the new data
 * @param newSize the dimensions of the newData
 * @param currentDim the dimension currently under work, should be between (orgData.size()-1) and 0
 *
 */
template <typename C>
void recursiveCopyMultiDimData(const C** orgData, C** newData, const std::vector<size_t>& orgDimSize, const std::vector<size_t>& orgSliceSize,
                               const std::vector<size_t>& newStart, const std::vector<size_t>& newSize, size_t currentDim)
{
    (*orgData) += newStart[currentDim] * orgSliceSize[currentDim];
    if (currentDim == 0) {
        *newData = std::copy(&(*orgData)[0], &(*orgData)[newSize[0]], *newData);
        (*orgData) += newSize[0]; // forward orgData pointer
    } else {
        for (size_t i = 0; i < newSize[currentDim]; i++) {
            recursiveCopyMultiDimData(orgData, newData, orgDimSize, orgSliceSize, newStart, newSize, currentDim - 1);
        }
    }
    (*orgData) += (orgDimSize[currentDim] - (newStart[currentDim] + newSize[currentDim])) * orgSliceSize[currentDim];
}

template <typename C>
void recursiveCopyMultiDimData(const C* orgData, C* newData, const std::vector<size_t>& orgDimSize, const std::vector<size_t>& orgSliceSize,
                               const std::vector<size_t>& newStart, const std::vector<size_t>& newSize)
{
    recursiveCopyMultiDimData(&orgData, &newData, orgDimSize, orgSliceSize, newStart, newSize, orgDimSize.size() - 1);
}

} // namespace MetNoFimex

#endif /*FIMEX_RECURSIVESLICECOPY_H*/
