/*
 * Fimex, VerticalTransformation.cc
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
 *  Created on: Aug 7, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"
#include "fimex/CDMException.h"
#include "fimex/Logger.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "coordSys/CoordSysUtils.h"
#include <map>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.VerticalTransformation.h");


std::ostream& operator<<(std::ostream& out, const MetNoFimex::VerticalTransformation& vt)
{
    out << vt.getName() << "(" << vt.getParameterString() << ")";
    return out;
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
//    if (not isComplete())
//        throw CDMException("incomplete vertical transformation");
    switch (verticalType) {
    case MIFI_VINT_PRESSURE: return getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt);
    case MIFI_VINT_HEIGHT: return getHeightConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    case MIFI_VINT_DEPTH: return getHeightConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    default: throw CDMException("unknown vertical type");
    }
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs) const
{
    CoordinateSystem::ConstAxisPtr xAxis, yAxis, zAxis, tAxis;
    size_t nx, ny, nz, t0, t1;
    MetNoFimex::getSimpleAxes(cs, reader->getCDM(),
            xAxis, yAxis, zAxis, tAxis,
            nx, ny, nz, t0, t1, unLimDimPos);
    return getConverter(reader, verticalType, unLimDimPos, cs, nx, ny, nz, t1-t0);
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getHeightConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    // try geopotential_height or fall back to pressure
    using namespace std;
    boost::shared_ptr<ToVLevelConverter> heightConv;
    map<string, string> attrs;
    vector<string> dims;
    const CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
    const CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
    const CoordinateSystem::ConstAxisPtr zAxis = cs->getGeoZAxis();
    if (xAxis.get() != 0 && yAxis.get() != 0 && zAxis.get() != 0) {
        dims.push_back(xAxis->getShape()[0]);
        dims.push_back(yAxis->getShape()[0]);
        dims.push_back(zAxis->getShape()[0]);
        attrs["standard_name"] = "geopotential_height";
        vector<string> geoVars = reader->getCDM().findVariables(attrs, dims);
        dims.clear();
        dims.push_back(xAxis->getShape()[0]);
        dims.push_back(yAxis->getShape()[0]);
        attrs["standard_name"] = "altitude";
        vector<string> altVars = reader->getCDM().findVariables(attrs, dims);
        if (geoVars.size() > 0 && altVars.size() > 0) {
            LOG4FIMEX(logger, Logger::INFO, "using geopotential height "<<geoVars[0]<<" to retrieve height");
            DataPtr geoPotData = reader->getScaledDataSliceInUnit(geoVars[0], "m", unLimDimPos);
            if (geoPotData->size() != (nx * ny * nz * nt)) {
                throw CDMException("geopotential height '" + geoVars[0] + "' has strange size: " + type2string(geoPotData->size()) + " != " + type2string(nx * ny * nz * nt));
            }
            LOG4FIMEX(logger, Logger::INFO, "using altitude "<<altVars[0]<<" to retrieve height");
            DataPtr altData = reader->getScaledDataSliceInUnit(altVars[0], "m", unLimDimPos);
            if (altData->size() != (nx * ny)) {
                throw CDMException("altitude '" + altVars[0] + "' has strange size: " + type2string(altData->size()) + " != " + type2string(nx * ny));
            }
            heightConv = boost::shared_ptr<ToVLevelConverter>(new GeopotentialToHeightConverter(geoPotData->asFloat(), altData->asFloat(), nx, ny, nz, nt));
            vector<string> altVars = reader->getCDM().findVariables(attrs, dims);
        } else {
            LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate height levels");
            boost::shared_ptr<ToVLevelConverter> presConv = getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt);
            heightConv = boost::shared_ptr<ToVLevelConverter>(new PressureToStandardHeightConverter(presConv));
        }
    }
    return heightConv;
}



} // namespace MetNoFimex
