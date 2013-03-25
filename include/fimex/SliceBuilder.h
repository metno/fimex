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
#include <set>
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{
// forward decl
class CDM;
class CoordinateAxis;
/**
 * @headerfile "fimex/SliceBuilder.h"
 */


class SliceBuilder
{
public:
    /**
     * Create a new slice builder for variable varName. The default will
     * give a slice of full size.
     * @param cdm
     * @param varName variable name
     * @param setUnlimited enable enlarging of unlimited dimensions
     * @throw CDMException f varName doesn't exists
     */
    SliceBuilder(const CDM& cdm, const std::string& varName, bool setUnlimited = false);
    /**
     * Simple interface to create a slicebuilder. No checks are made if
     * the created object is useful with any reader.
     * @param dimNames names of dimensions
     * @param dimSize maximum size of the dimensions
     */
    SliceBuilder(const std::vector<std::string>& dimNames, const std::vector<size_t>& dimSize);
    virtual ~SliceBuilder();
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
     * Set the start to 0 and the size to the maximum size. Though this is the default
     * this function will reset prevous reduced dimensions and it will mark the dimension as 'set'
     * and not return it in getUnsetDimensionNames.
     * @param axis name of the dimension to restrict
     */
    void setAll(const std::string& dimName);
    /**
     * Set the start to 0 and the size to the maximum size. Though this is the default
     * this function will reset prevous reduced dimensions and it will mark the dimension as 'set'
     * and not return it in getUnsetDimensionNames.
     * @param axis name of the dimension to restrict, ignored if NULL
     */
    void setAll(const boost::shared_ptr<const CoordinateAxis>& axis);
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
     * @return vector with names of dimensions which have not been
     *         set yet, (i.e. through setAll() or setStartAndSize()
     */
    std::vector<std::string> getUnsetDimensionNames() const;
    /**
     * @return vector with maximum sizes of the dimenions of the variable
     * in the order and size of the variables dimensions
     */
    const std::vector<size_t>& getMaxDimensionSizes() const {return maxSize_;}
    /**
     * Dimensions are usually restricted to their maximum size. For writing purposes it might be desired to
     * exend the size. With setUnlimited, it is possible to disable the size-tests for a dimensions.
     */
    void setUnlimited(const std::string& dimName, bool isUnlimited);
protected:
    size_t getDimPos(const std::string& dimName) const;
private:
    void init(const std::vector<std::string>& dimNames, const std::vector<std::size_t>& dimSize, const std::vector<bool>& unlimited);
    // position of the dimension
    std::map<std::string, size_t> dimPos_;
    std::set<std::string> setDims_;
    std::vector<size_t> maxSize_;
    std::vector<bool> unlimited_;
    std::vector<size_t> start_;
    std::vector<size_t> size_;

};

std::ostream& operator<<(std::ostream& os, const SliceBuilder& sb);

}



#endif /* SLICEBUILDER_H_ */
