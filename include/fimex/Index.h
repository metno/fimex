/*
 * Fimex, Index.h
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
 *  Created on: Jul 18, 2012
 *      Author: Heiko Klein
 */

#ifndef INDEX_H_
#define INDEX_H_

#include <vector>

namespace MetNoFimex
{

class Index
{
private:
    std::vector<std::size_t> dims_;
    std::vector<std::size_t> slices_;
public:
    Index() : dims_(0), slices_(0) {}
    /**
     *  Creates an index for an array with applied dimSizes. The first
     *  dimension is the fastest moving (fortran-array style).
     */
    explicit Index(std::vector<std::size_t> dimSizes);
    ~Index();
    /**
     * @return a vector with all dimension sizes
     */
    const std::vector<std::size_t>& getDims() const {return dims_;}
    /**
     * The position named by pos. Uses 0 for all dimensions >= pos.size()
     * @param pos
     * @return position within the array
     * @throws out_of_range Exception if pos.size() > getDims().size()
     */
    size_t getPos(std::vector<size_t> pos) const {size_t p = 0; for (size_t i = 0; i < pos.size(); ++i) {p += pos.at(i) * slices_.at(i);} return p;}
    size_t getPos(size_t a) const {return a;}
    size_t getPos(size_t a, size_t b) const {return a + b*slices_.at(1);}
    size_t getPos(size_t a, size_t b, size_t c) const {return a + b*slices_.at(1) + c*slices_.at(2);}
    size_t getPos(size_t a, size_t b, size_t c, size_t d) const {return a + b*slices_.at(1) + c*slices_.at(2) + d*slices_.at(3);}

};

} /* namespace MetNoFimex */
#endif /* INDEX_H_ */
