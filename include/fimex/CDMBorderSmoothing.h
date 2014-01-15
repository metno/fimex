/* -*- c++ -*-
 * Fimex, CDMBorderSmoothing.h
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Nov 5, 2013
 *      Author: Alexander Bürger
 */

#ifndef fimex_CDMBorderSmoothing_H
#define fimex_CDMBorderSmoothing_H 1

#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Data.h"

namespace MetNoFimex {

class CDMBorderSmoothingPrivate;

/**
 * @headerfile fimex/CDMBorderSmoothing.h
 */
/**
 * Takes two readers, inner and outer, and provides data on the area of
 * the inner grid smoothed for overlaying onto outer. If the outer grid
 * does not match the inner, it will be interpolated. Variable names must
 * match.
 * 
 * Used in CDMMerger together with CDMOverlay, and probably not very useful
 * elsewhere.
 */
class CDMBorderSmoothing : public CDMReader {
public:
    /**
     * Defines a smoothing function for the transition between
     * (interpolated) data from the outer to the inner grid.
     */
    class Smoothing {
    public:
        void setHorizontalSizes(size_t sizeX, size_t sizeY)
            { sizeX_ = sizeX; sizeY_ = sizeY; }

        virtual double operator()(size_t curX, size_t curY, double valueI, double valueO) = 0;

        virtual ~Smoothing() {}

    protected:
        size_t sizeX_, sizeY_;
    };

    typedef boost::shared_ptr<Smoothing> SmoothingPtr;

    //! A factory for creating smoothing function objects.
    class SmoothingFactory {
    public:
        virtual ~SmoothingFactory() {}
        virtual SmoothingPtr operator()(const std::string& varName) = 0;
    };

    typedef boost::shared_ptr<SmoothingFactory> SmoothingFactoryPtr;

public:
    //! Smooth transition to 'outer' from inner's data. Returns data on inner grid.
    CDMBorderSmoothing(boost::shared_ptr<CDMReader> inner, boost::shared_ptr<CDMReader> outer,
            int gridInterpolationMethod = MIFI_INTERPOL_BILINEAR);

    //! Change the smooting function factory.
    void setSmoothing(SmoothingFactoryPtr smoothingFactory);

    //! Decide if the outer value shall be used if the inner value is undefined. Default: true.
    void setUseOuterIfInnerUndefined(bool useOuter);

    using CDMReader::getDataSlice;
    virtual boost::shared_ptr<Data> getDataSlice(const std::string &varName, std::size_t unLimDimPos);

private:
    std::auto_ptr<CDMBorderSmoothingPrivate> p;
};

typedef boost::shared_ptr<CDMBorderSmoothing> CDMBorderSmoothingPtr;

} // namespace MetNoFimex

#endif /* fimex_CDMBorderSmoothing_H */
