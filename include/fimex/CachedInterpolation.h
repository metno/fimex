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

#ifndef CACHEDINTERPOLATION_H_
#define CACHEDINTERPOLATION_H_

#include "fimex/CDMReaderDecl.h"
#include "fimex/DataDecl.h"
#include "fimex/SliceBuilder.h"

#include <boost/shared_array.hpp>

namespace MetNoFimex
{

/**
 * @headerfile fimex/CachedInterpolation.h
 */
/**
 * Struct to store information used to create a slicebuilder limiting the amount of input data.
 */
struct ReducedInterpolationDomain {
    std::string xDim; //!< name of x-dimension
    std::string yDim; //!< name of y-dimension
    size_t xMin;      //!< offset on x-axis
    size_t yMin;      //!< offset on y-axis

    ReducedInterpolationDomain(const std::string& xdim, const std::string& ydim, size_t xmin, size_t ymin);
};

typedef std::shared_ptr<ReducedInterpolationDomain> ReducedInterpolationDomain_p;

/**
 * Interface for new cached spatial interpolation as used in #MetNoFimex::CDMInterpolator
 */
class CachedInterpolationInterface {
public:
    CachedInterpolationInterface(const std::string& xDimName, const std::string& yDimName, size_t inX, size_t inY, size_t outX, size_t outY);
    virtual ~CachedInterpolationInterface();

    virtual boost::shared_array<float> interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const = 0;

    /** @return x-size of input array */
    size_t getInX() const { return inX; }

    /** @return y-size of input array */
    size_t getInY() const { return inY; }

    /** @return x-size of output array */
    size_t getOutX() const { return outX; }

    /** @return y-size of output array */
    size_t getOutY() const { return outY; }

    /**
     * Read the input data from the reader, which is later used for the interpolateValues() function. This function will eventually reduce the
     * domain of the input data if createReducedDomain was called earlier.
     * @param reader
     * @param varName
     * @param unLimDim
     * @return Data matching input-data for this CachedInterpolationInterface
     */
    virtual DataPtr getInputDataSlice(CDMReader_p reader, const std::string& varName, size_t unLimDim) const;

    /**
     * Read the input data from the reader, which is later used for the interpolateValues() function. This function will eventually reduce the
     * domain of the input data if createReducedDomain was called earlier.
     * @param reader
     * @param varName
     * @param sb a slicebuilder to reduce other than the horizontal dimensions
     * @return Data matching input-data for this CachedInterpolationInterface
     */
    virtual DataPtr getInputDataSlice(CDMReader_p reader, const std::string& varName, const SliceBuilder& sb) const;

    virtual DataPtr getOutputDataSlice(DataPtr data, const SliceBuilder& sb) const;

    /**
     * allow fetching of a reduced interpolation domain, i.e. to work with a much smaller amount of input data
     * @return a 0-pointer unless a internal function to reduce the domain has been run, e.g. CachedInterpolation::createReducedDomain()
     */
    ReducedInterpolationDomain_p reducedDomain() const { return reducedDomain_; }

private:
    std::string _xDimName, _yDimName;

protected:
    size_t inX;
    size_t inY;
    size_t outX;
    size_t outY;
    std::shared_ptr<ReducedInterpolationDomain> reducedDomain_;
};

typedef std::shared_ptr<CachedInterpolationInterface> CachedInterpolationInterface_p;

CachedInterpolationInterface_p createCachedInterpolation(const std::string& xDimName, const std::string& yDimName, int method,
                                                         const std::vector<double>& pointsOnXAxis, const std::vector<double>& pointsOnYAxis, size_t inX,
                                                         size_t inY, size_t outX, size_t outY);

/**
 * Container to cache projection details to speed up
 * interpolation of lots of fields.
 */
class CachedInterpolation : public CachedInterpolationInterface
{
private:
    std::vector<double> pointsOnXAxis;
    std::vector<double> pointsOnYAxis;
    int (*func)(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);
public:
    /**
     * @param funcType {@link interpolation.h} interpolation method
     * @param pointsOnXAxis projected values of the new projections coordinates expressed in the current x-coordinate (size = outX*outY)
     * @param pointsOnYAxis projected values of the new projections coordinates expressed in the current y-coordinate (size = outX*outY)
     * @param inX size of current X axis
     * @param inY size of current Y axis
     * @param outX size of new X axis
     * @param outY size of new Y axis
     */
    CachedInterpolation(const std::string &xDimName, const std::string &yDimName, int funcType,
                        const std::vector<double> &pointsOnXAxis, const std::vector<double> &pointsOnYAxis,
                        size_t inX, size_t inY, size_t outX, size_t outY);

    /**
     * Actually interpolate the data. The data will be interpolated as floats internally.
     *
     * @param inData the input data
     * @param size the size of the input data array
     * @param newSize return the size of the output-array
     */
    boost::shared_array<float> interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const override;

private:
    /**
     * Create a reduced domain for later generation of a slicebuild to read a smaller domain.
     * It should be run immediately after creating the CachedInterpolation.
     */
    void createReducedDomain(const std::string& xDimName, const std::string& yDimName);
};

/**
 * Container to cache nearest-neighbor reprojection details to speed up
 * interpolation of lots of fields.
 */
class CachedNNInterpolation : public CachedInterpolationInterface
{
private:
    std::vector<size_t> pointsInIn;

public:
    /**
     * @param pointsOnXAxis projected values of the new projections coordinates expressed in the current x-coordinate (size = outX*outY)
     * @param pointsOnYAxis projected values of the new projections coordinates expressed in the current y-coordinate (size = outX*outY)
     * @param inX size of current X axis
     * @param inY size of current Y axis
     * @param outX size of new X axis
     * @param outY size of new Y axis
     */
    CachedNNInterpolation(const std::string& xDimName, const std::string& yDimName, const std::vector<double>& pointsOnXAxis,
                          const std::vector<double>& pointsOnYAxis, size_t inX, size_t inY, size_t outX, size_t outY);

    /**
     * Actually interpolate the data. The data will be interpolated as floats internally.
     *
     * @param inData the input data
     * @param size the size of the input data array
     * @param newSize return the size of the output-array
     */
    boost::shared_array<float> interpolateValues(boost::shared_array<float> inData, size_t size, size_t& newSize) const override;
};

} // namespace MetNoFimex

#endif /*CACHEDINTERPOLATION_H_*/
