/*
 * Fimex, DataIndex.h
 *
 * (C) Copyright 2012-2026, met.no
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
 *
 *  Created on: Jul 18, 2012
 *      Author: Heiko Klein
 */

#ifndef DATAINDEX_H_
#define DATAINDEX_H_

#include <vector>

namespace MetNoFimex
{


/**
 * @headerfile fimex/DataIndex.h
 */
class DataIndex
{
private:
    static std::size_t MIN_DIMS() {return 10;}
    std::vector<std::size_t> dims_;
    std::vector<std::size_t> slices_;
public:
    DataIndex() : dims_(0), slices_(MIN_DIMS(), 0) {}
    /**
     *  Creates an index for an array with applied dimSizes. The first
     *  dimension is the fastest moving (fortran-array style).
     */
    explicit DataIndex(std::vector<std::size_t> dimSizes);
    ~DataIndex();
    /**
     * @return a vector with all dimension sizes
     */
    const std::vector<std::size_t>& getDims() const {return dims_;}
    /**
     * The position named by pos. Uses 0 for all dimensions >= pos.size()
     * @param pos
     * @return position within the array
     * @throws out_of_range Exception if pos.size() > max(getDims().size(), 10)
     */
    std::size_t getPos(std::vector<std::size_t> pos) const {std::size_t p = 0; for (std::size_t i = 0; i < pos.size(); ++i) {p += pos.at(i) * slices_.at(i);} return p;}
    std::size_t getPos(std::size_t a) const {return a;}
    std::size_t getPos(std::size_t a, std::size_t b) const {return a + b*slices_.at(1);}
    std::size_t getPos(std::size_t a, std::size_t b, std::size_t c) const {return a + b*slices_.at(1) + c*slices_.at(2);}
    std::size_t getPos(std::size_t a, std::size_t b, std::size_t c, std::size_t d) const {return a + b*slices_.at(1) + c*slices_.at(2) + d*slices_.at(3);}

};

} /* namespace MetNoFimex */
#endif /* DATAINDEX_H_ */
