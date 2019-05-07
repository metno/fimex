/*
  Fimex, src/coordSys/verticalTransform/AltitudeHeightConverter.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

#include "fimex/ArrayLoop.h"

#include <boost/make_shared.hpp>

#include <map>
#include <vector>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.AltitudeHeightConverter");

VerticalConverter_p AltitudeHeightConverter::createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitudeOrHeight,
                                                             bool addTopography)
{
    using namespace std;

    const CoordinateAxis_cp xAxis = cs->getGeoXAxis();
    const CoordinateAxis_cp yAxis = cs->getGeoYAxis();
    if (!xAxis || !yAxis)
        return VerticalConverter_p();

    vector<string> dims;
    dims.push_back(xAxis->getShape()[0]);
    dims.push_back(yAxis->getShape()[0]);
    map<string, string> attrs;
    attrs["standard_name"] = "(surface_geopotential|surface_altitude|altitude|geopotential_height|geopotential)";

    std::string topoVar;
    const vector<string> topoVars = reader->getCDM().findVariables(attrs, dims);
    CoordinateSystem_cp_v allCS = listCoordinateSystems(reader);
    for (vector<string>::const_iterator it = topoVars.begin(); it != topoVars.end(); ++it) {
        if (CoordinateSystem_cp topoCS = findCompleteCoordinateSystemFor(allCS, *it)) {
            if (CoordinateAxis_cp zax = topoCS->getGeoZAxis()) {
                if (reader->getCDM().getDimension(zax->getShape().front()).getLength() != 1) {
                    LOG4FIMEX(logger, Logger::INFO, "topo var '" << *it << "' has z axis '" << zax->getName() << "' with length != 1, skipping");
                    continue;
                }
            }
            topoVar = *it;
            break;
        }
    }
    if (topoVar.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "no topography/altitude found to retrieve height");
        return VerticalConverter_p();
    }

    LOG4FIMEX(logger, Logger::INFO, "using altitude " << topoVar << " to retrieve height");
    return boost::make_shared<AltitudeHeightConverter>(reader, cs, altitudeOrHeight, topoVar, addTopography);
}

AltitudeHeightConverter::AltitudeHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitudeOrHeight,
                                                 const std::string& topography, bool addTopography)
    : BasicVerticalConverter(reader, cs)
    , altitudeOrHeight_(altitudeOrHeight)
    , topography_(topography)
    , topographyFactor_(addTopography ? 1 : -1)
{
    // FIXME next lines copied from CDMProcessor::addVerticalVelocity()
    const std::string stdName = reader->getCDM().getAttribute(topography, "standard_name").getStringValue();
    if (stdName == "surface_altitude" || stdName == "altitude" || stdName == "geopotential_height") {
        topographyUnit_ = "m";
    } else {
        topographyFactor_ /= MIFI_EARTH_GRAVITY;
        topographyUnit_ = "m^2/s^2"; // division of gepotential by gravity
    }
}

std::vector<std::string> AltitudeHeightConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_)
            .merge(topography_, true)
            .merge(altitudeOrHeight_)
            .shape();
}

DataPtr AltitudeHeightConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble topo(reader_, topography_, topographyUnit_, sb);
    VarDouble altiOrHeight(reader_, altitudeOrHeight_, sb);

    ArrayDims out_dims = makeArrayDims(sb);
    boost::shared_array<double> out_values(new double[out_dims.volume()]);

    enum { TOPO, IN_ALTI, OUT_HEIGHT };
    ArrayGroup group = ArrayGroup().add(topo.dims).add(altiOrHeight.dims).add(out_dims);
    group.minimizeShared(0); // FIXME do not treat each value separately

    Loop loop(group);
    do { // sharedVolume() == 1 because we called minimizeShared before
        out_values[loop[OUT_HEIGHT]] = altiOrHeight.values[loop[IN_ALTI]]
                + topographyFactor_ * topo.values[loop[TOPO]];
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace MetNoFimex
