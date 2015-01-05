/*
 * Fimex, Height.cc
 *
 * (C) Copyright 2013-2015, met.no
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
 *  Created on: Aug 8, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/Height.h"
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"
#include "fimex/Logger.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"


namespace MetNoFimex {

boost::shared_ptr<ToVLevelConverter> Height::getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const
{
//    DataPtr h = reader->getScaledDataSliceInUnit(height, "m", unLimDimPos);
//    boost::shared_array<double> ha = h->asDouble();
//    return boost::shared_ptr<ToVLevelConverter>(new HeightStandardToPressureConverter(vector<double> (&ha[0],&ha[0] + h->size())));
    // does not exist generally without known topography, which depends the variable to read
    return boost::shared_ptr<ToVLevelConverter>();
}

boost::shared_ptr<ToVLevelConverter> Height::getHeightConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    DataPtr hd = reader->getScaledDataSliceInUnit(height, "m", unLimDimPos);
    const boost::shared_array<double> ha = hd->asDouble();
    return boost::shared_ptr<ToVLevelConverter>(new IdentityToVLevelConverter(vector<double> (&ha[0], &ha[0] + hd->size())));
}

boost::shared_ptr<ToVLevelConverter> Height::getAltitudeConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    // try geopotential_height or fall back to pressure
    using namespace std;
    boost::shared_ptr<ToVLevelConverter> altConv;
    boost::shared_ptr<ToVLevelConverter> heightConv = getHeightConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    assert(heightConv.get() != 0);
    map<string, string> attrs;
    vector<string> dims;
    const CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
    const CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
    const CoordinateSystem::ConstAxisPtr zAxis = cs->getGeoZAxis();
    if (xAxis.get() != 0 && yAxis.get() != 0 && zAxis.get() != 0) {
        dims.push_back(xAxis->getShape()[0]);
        dims.push_back(yAxis->getShape()[0]);
        attrs["standard_name"] = "altitude";
        vector<string> topoVars = reader->getCDM().findVariables(attrs, dims);
        if (topoVars.size() > 0) {
            LOG4FIMEX(getLogger("fimex.VerticalTransform.Height"), Logger::INFO, "using altitude "<<topoVars[0]<<" to retrieve height");
            DataPtr topoData = reader->getScaledDataSliceInUnit(topoVars[0], "m", unLimDimPos);
            if (topoData->size() != (nx * ny)) {
                throw CDMException("altitude '" + topoVars[0] + "' has strange size: " + type2string(topoData->size()) + " != " + type2string(nx * ny));
            }
            altConv = boost::shared_ptr<ToVLevelConverter>(new HeightConverterToAltitudeConverter(heightConv, topoData->asFloat(), nx, ny, nz, nt));
        }
    }
    return altConv;
}

} // namespace MetNoFimex
