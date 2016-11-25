/*
 * Fimex, CoordinateSystemSliceBuilder.h
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
 *  Created on: Jun 1, 2011
 *      Author: Heiko Klein
 */

#ifndef COORDINATESYSTEMSLICEBUILDER_H_
#define COORDINATESYSTEMSLICEBUILDER_H_


#include "SliceBuilder.h"
#include "fimex/coordSys/CoordinateAxis.h"

namespace MetNoFimex
{

// forward decl;
class CoordinateSystem;

/**
 * @headerfile fimex/CoordinateSystemSliceBuilder.h
 */
/**
 * SliceBuilder with knowledge about the coordinate-system. It is therefore
 * possible to set some variable on the basis that they are i.e. time-axes.
 */
class CoordinateSystemSliceBuilder: public MetNoFimex::SliceBuilder
{
public:
    CoordinateSystemSliceBuilder(const CDM& cdm, boost::shared_ptr<const CoordinateSystem> cs);
    virtual ~CoordinateSystemSliceBuilder() {}

    /**
     * Get the types of the dimensions.
     * @return vector of size of dimensions
     */
    std::vector<CoordinateAxis::AxisType> getAxisTypes() const;

    /**
     * Set a single reference-time. The CoordinateSystemSliceBuilder
     * will only fetch a single reference-time, by default the first one.
     */
    void setReferenceTimePos(size_t refTimePos);
    /**
     * Set the start and the size of the time-dimension. This might
     * even be a 2-dimensional time-dimension, i.e. (refTime,offset)
     */
    void setTimeStartAndSize(size_t start, size_t size);
    /**
     * Set start and size for all axes of the axisType. If axisType
     * is ReferenceTime, only the start-position but not the size is
     * used.
     *
     * @param axisType
     * @param start
     * @param size
     *
     * @warning When having duplicated axis, this function might return
     * strange results. This happens most likely when having more than
     * 6 dimensions (multiple 'Other' axes). The author didn't see such
     * files yet.
     */
    void setAxisStartAndSize(CoordinateAxis::AxisType axisType, size_t start, size_t size);

    /**
     * Get a slice-builder to fetch data for the time-variable
     * with the same reference-time as set for the current slice. It should be
     * used as:
     * @code
     * reader->getDataSlice(cs->getTimeAxis()->getName(), cssb->getTimeVariableSliceBuilder)
     * @endcode
     */
    SliceBuilder getTimeVariableSliceBuilder();

    /**
     * Return the Coordinate-System of the SliceBuilder.
     * @return coordinateSystem, same as used for initialization
     */
    boost::shared_ptr<const CoordinateSystem> getCoordinateSystem() {return cs_;}

private:
    boost::shared_ptr<const CoordinateSystem> cs_;
    std::vector<std::string> tShape_;
};



}

#endif /* COORDINATESYSTEMSLICEBUILDER_H_ */
