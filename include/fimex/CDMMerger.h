/* -*- c++ -*-
 * Fimex, CDMMerger.h
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
 *  Created on: Aug 28, 2012
 *      Author: Alexander Bürger
 */

#ifndef fimex_CDMMerger_H
#define fimex_CDMMerger_H 1

#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Data.h"

namespace MetNoFimex {

class CDMMergerPrivate;

/**
 * @headerfile fimex/CDMMerger.h
 */
/**
 * Allows merge data from an inner, "fine" grid onto data from an outer, "rough" grid.
 *
 * The resulting grid will be defined by extending the inner grid of
 * the first variable merged with "addMergedVariable" with the inner
 * grid's step size until the limits of the outer grid for this
 * variable are reached. If necessary, the outer reader is
 * interpolated to this grid. In the area of the inner grid, the
 * values are replaced with those from the inner grid, by default with
 * a linear transition (fill values are handled specially).
 *
 * Present limitations:
 *
 * - all variables added for merging later must have the same
 *   horizontal axes as the first variable.
 *
 * - there is no check whether the shapes of a merged variables is
 *   compatible in inner and outer readers, only a warning if they are
 *   not equal
 *
 * - there is no check at all if other dimensions are compatible
 */
class CDMMerger : public CDMReader {
public:
    /**
     * Defines a smoothing function for the transition between
     * (interpolated) data from the outer to the inner grid.
     */
    class Smoothing {
    public:
        void setFillValues(double fillI, double fillO)
            { fillI_ = fillI; fillO_ = fillO; }

        void setHorizontalSizes(size_t sizeX, size_t sizeY)
            { sizeX_ = sizeX; sizeY_ = sizeY; }

        virtual double operator()(size_t curX, size_t curY, double valueI, double valueO) = 0;

        virtual ~Smoothing() {}

    protected:
        size_t sizeX_, sizeY_;
        double fillI_, fillO_;
    };

    typedef boost::shared_ptr<Smoothing> SmoothingPtr;

    /**
     * A factory for creating smoothing function objects.
     */
    class SmoothingFactory {
    public:
        virtual ~SmoothingFactory() {}
        virtual SmoothingPtr operator()(const std::string& varName) = 0;
    };

    typedef boost::shared_ptr<SmoothingFactory> SmoothingFactoryPtr;

public:
    /**
     * Merge data from inner grid onto refined outer grid.
     */
    CDMMerger(boost::shared_ptr<CDMReader> inner, boost::shared_ptr<CDMReader> outer);

    /** Change the smooting function factory. */
    void setSmoothing(SmoothingFactoryPtr smoothingFactory);

    /** Set the interpolation method for outer and inner grid interpolations.
     *
     * At present, interpolation of inner grids is not implemented.
     */
    void setGridInterpolationMethod(int methodI, int methodO);

    /**
     * Request to merge variable nameI from inner into nameO from outer.
     */
    void addMergedVariable(const std::string& nameI, const std::string& nameO);

    /**
     * Convenience function for merging variables with the same name in inner and outer.
     */
    void addMergedVariable(const std::string& name)
        { addMergedVariable(name, name); }

    using CDMReader::getDataSlice;
    virtual boost::shared_ptr<Data> getDataSlice(const std::string &varName, std::size_t unLimDimPos);

private:
    std::auto_ptr<CDMMergerPrivate> p;
};

typedef boost::shared_ptr<CDMMerger> CDMMergerPtr;

} // namespace MetNoFimex

#endif /* fimex_CDMMerger_H */
