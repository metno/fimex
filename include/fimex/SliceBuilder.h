/*
 * Fimex, SliceBuilder.h
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Mar 16, 2010
 *      Author: Heiko Klein
 */

#ifndef SLICEBUILDER_H_
#define SLICEBUILDER_H_

#include <string>
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{
// forward decl
class CDM;
class CoordinateAxis;


class SliceBuilder
{
public:
    /**
     * Create a new slice builder for variable varName. The default will
     * give a slice of full size.
     * @param cdm
     * @param varName variable name
     * @throw CDMException f varName doesn't exists
     */
    SliceBuilder(const CDM& cdm, const std::string& varName);
    ~SliceBuilder();
    /**
     * set the start position and the size of the slice of that dimension
     * @param dimName name of the dimension to restrict
     * @param start starting point of slice (starts at 0)
     * @param size size of the slice
     * @throw CDMException if dimName not part of the dimensions of variable
     * @throw out_of_range depending on startPos or size
     */
    void setStartAndSize(const std::string& dimName, size_t start, size_t size);
    /**
     * set the start position and the size of the slice of that dimension
     * @param axis name of the dimension to restrict, ignored if NULL
     * @param start starting point of slice (starts at 0)
     * @param size size of the slice
     * @throw CDMException if axis not part of the dimensions of the variable
     * @throw out_of_range depending on startPos or size
     */
    void setStartAndSize(const boost::shared_ptr<const CoordinateAxis>& axis, size_t start, size_t size);
    /**
     * @return vector with start-positions of shape-size and order of the variable
     */
    const std::vector<size_t>& getDimensionStartPositions() const {return start_;}
    /**
     * @return vector with sizes of the dimenions of the variable
     * in the order and size of the variables dimensions
     */
    const std::vector<size_t>& getDimensionSizes() const {return size_;}
    /**
     * @return vector with names of dimensions in correct order
     */
    std::vector<std::string> getDimensionNames() const;
    /**
     * @return vector with maximum sizes of the dimenions of the variable
     * in the order and size of the variables dimensions
     */
    const std::vector<size_t>& getMaxDimensionSizes() const {return maxSize_;}

private:
    size_t getDimPos(const std::string& dimName) const;
    // position of the dimension
    std::map<std::string, size_t> dimPos_;
    std::vector<size_t> maxSize_;
    std::vector<size_t> start_;
    std::vector<size_t> size_;

};

}

#endif /* SLICEBUILDER_H_ */
