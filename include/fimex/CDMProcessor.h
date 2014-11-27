/*
 * Fimex, CDMProcessor.h
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
 *  Created on: Mar 19, 2012
 *      Author: Heiko Klein
 */

#ifndef CDMPROCESSOR_H_
#define CDMPROCESSOR_H_

#include "fimex/CDMReader.h"
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{

// forward decl
struct CDMProcessorImpl;

/**
 * @headerfile fimex/CDMProcessor.h
 */
/**
 * The CDMProcessor is a class for various smaller data-manipulations.
 * Examples are deaccumulation along the time-axis, ...
 */
class CDMProcessor: public MetNoFimex::CDMReader
{
public:
    CDMProcessor(boost::shared_ptr<CDMReader> dataReader);
    virtual ~CDMProcessor();
    /**
     * add vertical velocity to this CDM, using continuity equation on model-levels
     */
    void addVerticalVelocity();
    /**
     * mark a variable for accumulation along the unlimited dimension, i.e.
     * vnew(0) = vold(0)
     * vnew(n) = vold(n)+vold(n-1)
     * @param varName name of the variable to de-accumulate
     * @warning does not handle fill-values unless those are NaNs
     */
    void accumulate(const std::string& varName);
    /**
     * mark a variable for de-accumulation along the unlimited dimension, i.e.
     * vnew(n) = vold(n)-vold(n-1)
     * @param varName name of the variable to de-accumulate
     * @warning does not handle fill-values unless those are NaNs
     */
    void deAccumulate(const std::string& varName);
    /**
     * rotate the vector from direction in x/y axes to direction in lat/lon axes
     * @param toLatLon convert to latLon if true, otherwise, convert latLon to grid-axes
     * @param varNameX the x-part of the vector
     * @param varNameY the y-part of the vector
     * @param stdNameX optional new standard_name for x
     * @param stdNameY optional new standard_name for y
     */
    void rotateVectorToLatLon(bool toLatLon, const std::vector<std::string>& varNameX, const std::vector<std::string>& varNameY, const std::vector<std::string>& stdNameX = std::vector<std::string>(0), const std::vector<std::string>& stdNameY = std::vector<std::string>(0));
    /**
     * rotate all the vectors from direction in x/y axes to direction in lat/lon axes vectors are detected automatically
     * @param toLatLon convert to latLon if true, otherwise, convert latLon to grid-axes
     */
    void rotateAllVectorsToLatLon(bool toLatLon);
    /**
     * rotate the  direction given in degree from y axes to direction in lat axes (north)
     * @param toLatLon convert to latLon if true, otherwise, convert latLon to grid-axes
     * @param varNames the variable-names containing directions given as angles in degree
     */
    void rotateDirectionToLatLon(bool toLatLon, const std::vector<std::string>& varNames);
    using CDMReader::getDataSlice;
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos);
private:
    // pimpl
    boost::shared_ptr<CDMProcessorImpl> p_;

};

} /* namespace MetNoFimex */
#endif /* CDMPROCESSOR_H_ */
