/*
 * Fimex, VerticalTransformation.h
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
 *  Created on: Aug 6, 2013
 *      Author: Heiko Klein
 */

#ifndef VERTICALTRANSFORMATION_H_
#define VERTICALTRANSFORMATION_H_

#include <boost/shared_ptr.hpp>
#include <string>
#include <vector>
#include <iostream>
#include "fimex/mifi_constants.h"



namespace MetNoFimex
{

// forward declarations
class CDMReader;
class CoordinateSystem;
class ToVLevelConverter;

/**
 * @headerfile fimex/coordSys/verticalTransform/VerticalTransformation.h
 */
/**
 *  Base class for vertical transformations like AtmosphereSigma coordinate or
 *  OceanSG1 coordinate, and for completeness also Pressure and Height.
 *  Vertical-transformation are usually accessed by CoordinateSystem::getVerticalTransformation:
 *
 @code
    string varName = "air_temperature";
    boost::shared_ptr<const CoordinateSystem> cs = findCompleteCoordinateSystemFor(coordSys, varName);
    if (cs.get()) {
        if (cs->hasVerticalTransformation()) {
            boost::shared_ptr<const VerticalTransformation> vtran = cs->getVerticalTransformation();
            if (vtran->getName() == HybridSigmaPressure1::NAME()) {
                const HybridSigmaPressure1* hyb1 = dynamic_cast<const HybridSigmaPressure1*>(vtran.get());
                assert(hyb1 != 0);
                string apVar = hyb1->ap;
                string bVar = hyb1->b;
                string psVar = hyb1->ps
                ...
            }
        }
    }
 @endcode
 *
 * The qualified names of the VerticalTransformation classes can be found in the
 * respectiv classes inheriting from VerticalTransformation, found in
 * fimex/coordSys/verticalTransform/ *.h . The member-names of the sub-classes describe
 * the parametrization of the VerticalTransformation, and their values reflect the
 * variable-names.
 */
class VerticalTransformation
{
public:
    virtual ~VerticalTransformation() {}
    /// the indentifier of the vertical transformation
    virtual std::string getName() const = 0;
    /// list the parameters
    virtual std::string getParameterString() const = 0;
    /// the most natural vertical type, one of the MIFI_VINT_* in fimex/mifi_constants.h
    virtual int getPreferredVerticalType() const = 0;
    /**
     * Indicate if all parameters are given to fully describe the transformation.
     *
     * Even a fully described transformation does not guarantee that a converter exists
     * (e.g. height -> altitude requires in addition topography).
     *
     * And converters might exist, even if a transformation is not complete.
     */
    virtual bool isComplete() const = 0;
    /**
     * get a converter. Pressure will be in unit hPa, height/depth in unit m.
     * @param reader a reader to fetch the data from
     * @param verticalType one of the MIFI_VINT_* in fimex/mifi_constants.h
     * @param unLimDimPos the unlimited position to start at
     * @param cs the coordinate system one is interested in
     * @param nx x-size of the resulting data
     * @param ny y-size of the resulting data
     * @param nz z-size of the resulting data
     * @param nt The final t-size will be nt-unLimDimPos.
     * @return
     */
    virtual boost::shared_ptr<ToVLevelConverter> getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const;

    //! same as getConverter, but determines nx, nz, nt using MetNoFimex::getSimpleAxes from CDMUtils
    boost::shared_ptr<ToVLevelConverter> getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs) const;

protected:
    virtual boost::shared_ptr<ToVLevelConverter> getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const = 0;
    /**
     * Default implementation: Convert to altitude (height above MSL) with pressure(-converter) and standard atmosphere.
     * @param reader
     * @param unLimDimPos
     * @param cs
     * @param nx
     * @param ny
     * @param nz
     * @param nt
     * @return
     */
    virtual boost::shared_ptr<ToVLevelConverter> getAltitudeConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const;
    /**
     * Default implementation: Convert to height above ground with pressure(-converter) and standard atmosphere.
     * @param reader
     * @param unLimDimPos
     * @param cs
     * @param nx
     * @param ny
     * @param nz
     * @param nt
     * @return
     */
    virtual boost::shared_ptr<ToVLevelConverter> getHeightConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const;

private:
    boost::shared_ptr<ToVLevelConverter> getIdentityPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const;
    boost::shared_ptr<ToVLevelConverter> findPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const;
};

std::ostream& operator<<(std::ostream& out, const VerticalTransformation& vt);

} /* namespace MetNoFimex */



#endif /* VERTICALTRANSFORMATION_H_ */
