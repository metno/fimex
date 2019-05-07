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

#include "fimex/CDMReaderDecl.h"
#include "fimex/coordSys/CoordSysDecl.h"
#include "fimex/mifi_constants.h"

#include <string>
#include <vector>
#include <iosfwd>

namespace MetNoFimex {

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
    CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordSys, varName);
    if (cs.get()) {
        if (cs->hasVerticalTransformation()) {
            VerticalTransformation_cp vtran = cs->getVerticalTransformation();
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
 * respective classes inheriting from VerticalTransformation, found in
 * fimex/coordSys/verticalTransform/ *.h . The member-names of the sub-classes describe
 * the parameterization of the VerticalTransformation, and their values reflect the
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

    /** Deprecated, does not work well with additional dimensions. Use VerticalConverter. */
    ToVLevelConverter_p getConverter(CDMReader_p reader, int verticalType, size_t unLimDimPos, CoordinateSystem_cp cs, size_t nx, size_t ny, size_t nz,
                                     size_t nt) const;

    /** Deprecated, does not work well with additional dimensions. Use VerticalConverter. */
    ToVLevelConverter_p getConverter(CDMReader_p reader, int verticalType, size_t unLimDimPos, CoordinateSystem_cp cs) const;

    /**
     * get a converter. Pressure will be in unit hPa, height/depth in unit m.
     * @param reader a reader to fetch the data from
     * @param verticalType one of the MIFI_VINT_* in fimex/mifi_constants.h
     * @param cs the coordinate system one is interested in
     * @return
     */
    virtual VerticalConverter_p getConverter(CDMReader_p reader, CoordinateSystem_cp cs, int verticalType) const;

protected:
    virtual VerticalConverter_p getPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const = 0;

    /**
     * Default implementation: Convert to altitude (height above MSL) with pressure(-converter) and standard atmosphere.
     * @param reader
     * @param cs
     * @return
     */
    virtual VerticalConverter_p getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;

    /**
     * Default implementation: Convert to height above ground with pressure(-converter) and standard atmosphere.
     * @param reader
     * @param cs
     * @return
     */
    virtual VerticalConverter_p getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;

    /**
     * Default implementation: No conversion.
     * @param reader
     * @param cs
     * @return
     */
    virtual VerticalConverter_p getDepthConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;

private:
    VerticalConverter_p getIdentityPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
    VerticalConverter_p findPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
};

typedef std::shared_ptr<const VerticalTransformation> VerticalTransformation_cp;

std::ostream& operator<<(std::ostream& out, const VerticalTransformation& vt);

} /* namespace MetNoFimex */

#endif /* VERTICALTRANSFORMATION_H_ */
