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
#include "fimex/Utils.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "coordSys/CoordSysUtils.h"

#include <iostream>
#include <map>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.VerticalTransformation");


std::ostream& operator<<(std::ostream& out, const MetNoFimex::VerticalTransformation& vt)
{
    out << vt.getName() << "(" << vt.getParameterString() << ")";
    return out;
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos,
        boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
//    if (not isComplete())
//        throw CDMException("incomplete vertical transformation");
    switch (verticalType) {
    case MIFI_VINT_PRESSURE: return findPressureConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    case MIFI_VINT_HEIGHT: return  getHeightConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    case MIFI_VINT_ALTITUDE: return getAltitudeConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    case MIFI_VINT_DEPTH: return getAltitudeConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    default: throw CDMException("unknown vertical type");
    }
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos,
        boost::shared_ptr<const CoordinateSystem> cs) const
{
    CoordinateSystem::ConstAxisPtr xAxis, yAxis, zAxis, tAxis;
    size_t nx, ny, nz, t0, t1;
    MetNoFimex::getSimpleAxes(cs, reader->getCDM(),
            xAxis, yAxis, zAxis, tAxis,
            nx, ny, nz, t0, t1, unLimDimPos);
    return getConverter(reader, verticalType, unLimDimPos, cs, nx, ny, nz, t1-t0);
}


boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getAltitudeConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos,
        boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    // try geopotential_height or fall back to pressure
    using namespace std;
    typedef boost::shared_ptr<ToVLevelConverter> conv_p;

    const CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
    const CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
    const CoordinateSystem::ConstAxisPtr zAxis = cs->getGeoZAxis();
    if (!(xAxis.get() && yAxis.get() && zAxis.get()))
        return conv_p();

    vector<string> dims3;
    dims3.push_back(xAxis->getShape()[0]);
    dims3.push_back(yAxis->getShape()[0]);
    dims3.push_back(zAxis->getShape()[0]);
    vector<string> dims2;
    dims2.push_back(xAxis->getShape()[0]);
    dims2.push_back(yAxis->getShape()[0]);
    map<string, string> attrs;
    attrs["standard_name"] = "geopotential_height";
    vector<string> geoVars = reader->getCDM().findVariables(attrs, dims3);
    if (geoVars.size() > 0) {
        LOG4FIMEX(logger, Logger::INFO, "using geopotential height "<<geoVars[0]<<" to retrieve altitude");
        DataPtr geoPotData = reader->getScaledDataSliceInUnit(geoVars[0], "m", unLimDimPos);
        if (geoPotData->size() != (nx * ny * nz * nt)) {
            throw CDMException("geopotential height '" + geoVars[0]
                    + "' has strange size: " + type2string(geoPotData->size())
                    + " != " + type2string(nx * ny * nz * nt));
        }
        return conv_p(new GeopotentialToAltitudeConverter(geoPotData->asFloat(), nx, ny, nz, nt));
    }

    conv_p pressure = findPressureConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);

    attrs["standard_name"] = "surface_air_pressure";
    vector<string> sapVars = reader->getCDM().findVariables(attrs, dims2);

    attrs["standard_name"] = "surface_geopotential";
    vector<string> sgpVars = reader->getCDM().findVariables(attrs, dims2);

    attrs["standard_name"] = "air_temperature";
    vector<string> airtVars = reader->getCDM().findVariables(attrs, dims3);

    if (pressure.get() && !sapVars.empty() && !sgpVars.empty() && !airtVars.empty()) {
        attrs["standard_name"] = "specific_humidity";
        vector<string> shVars = reader->getCDM().findVariables(attrs, dims3);

        LOG4FIMEX(logger, Logger::INFO, "using hypsometric equation with surface pressure '"
                << sapVars[0] << "', surface geopotential '" << sgpVars[0]
                << "', air_temperature '" << airtVars[0] << "' to retrieve altitude");

        DataPtr sapData = reader->getScaledDataSliceInUnit(sapVars[0], "hPa", unLimDimPos);
        DataPtr sgpData = reader->getScaledDataSliceInUnit(sgpVars[0], "m^2/s^2", unLimDimPos);
        DataPtr airtData = reader->getScaledDataSliceInUnit(airtVars[0], "K", unLimDimPos);
        if (!sapData || !sgpData || !airtData) {
            LOG4FIMEX(logger, Logger::INFO, "hypsometric no data");
            return conv_p();
        }
        if (sapData->size() != (nx * ny * nt)) {
            throw CDMException("surface air pressure '" + sapVars[0]
                    + "' has strange size: " + type2string(sapData->size())
                    + " != " + type2string(nx * ny * nt));
        }
        if (sgpData->size() != (nx * ny * nt)) {
            throw CDMException("surface geopotential '" + sgpVars[0]
                    + "' has strange size: " + type2string(sgpData->size())
                    + " != " + type2string(nx * ny * nt));
        }
        if (airtData->size() != (nx * ny * nz * nt)) {
            throw CDMException("air temperature '" + airtVars[0]
                    + "' has strange size: " + type2string(airtData->size())
                    + " != " + type2string(nx * ny * nz * nt));
        }
        boost::shared_array<float> shVal;
        if (!shVars.empty()) {
            DataPtr shData = reader->getScaledDataSliceInUnit(shVars[0], "1", unLimDimPos);
            if (shData->size() != (nx * ny * nz * nt)) {
                LOG4FIMEX(logger, Logger::INFO, "specific humidity '" + shVars[0]
                    + "' has strange size: " + type2string(shData->size())
                        + " != " + type2string(nx * ny * nz * nt)
                        + ", not using specific humidity");
            } else {
                LOG4FIMEX(logger, Logger::INFO, "hypsometric equation uses virtual "
                        "temperature with specific humidity '" << shVars[0] << "'");
            }
            shVal = shData->asFloat();
        }
        return conv_p(new PressureIntegrationToAltitudeConverter(pressure,
                        sapData->asFloat(), sgpData->asFloat(), airtData->asFloat(),
                        shVal, nx, ny, nt));
    }

    if (pressure) {
        LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate altitude levels");
        return conv_p(new PressureToStandardAltitudeConverter(pressure));
    }

    return conv_p();
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getHeightConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos,
        boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    // try geopotential_height or fall back to pressure
    using namespace std;
    boost::shared_ptr<ToVLevelConverter> heightConv;
    boost::shared_ptr<ToVLevelConverter> altConv = getAltitudeConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    if (altConv.get() == 0) {
        return heightConv; // no converter
    }
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
            LOG4FIMEX(logger, Logger::INFO, "using altitude "<<topoVars[0]<<" to retrieve height");
            DataPtr topoData = reader->getScaledDataSliceInUnit(topoVars[0], "m", unLimDimPos);
            if (topoData->size() != (nx * ny)) {
                throw CDMException("altitude '" + topoVars[0] + "' has strange size: " + type2string(topoData->size()) + " != " + type2string(nx * ny));
            }
            heightConv = boost::shared_ptr<ToVLevelConverter>(new AltitudeConverterToHeightConverter(altConv, topoData->asFloat(), nx, ny, nz, nt));
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "no topography/altitude found to retrieve height");
        }
    }
    return heightConv;
}



boost::shared_ptr<ToVLevelConverter> VerticalTransformation::getIdentityPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos,
        boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    // try use 4d pressure field
    using namespace std;
    typedef boost::shared_ptr<ToVLevelConverter> ToVLevelConverter_p;

    const CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
    const CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
    const CoordinateSystem::ConstAxisPtr zAxis = cs->getGeoZAxis();
    if (!(xAxis.get() && yAxis.get() && zAxis.get())) {
        LOG4FIMEX(logger, Logger::INFO, "cs lacks " << (xAxis.get() ? "" : "x ")
                << (yAxis.get() ? "" : "y ") << (zAxis.get() ? "" : "z ") << "axis/axes, no pressure field");
        return ToVLevelConverter_p();
    }

    vector<string> dims;
    dims.push_back(xAxis->getShape()[0]);
    dims.push_back(yAxis->getShape()[0]);
    dims.push_back(zAxis->getShape()[0]);

    map<string, string> attrs;
    attrs["standard_name"] = "(air_)?pressure";

    const vector<string> pVars = reader->getCDM().findVariables(attrs, dims);
    if (pVars.empty()) {
        //LOG4FIMEX(logger, Logger::DEBUG, "no pressure field");
        return ToVLevelConverter_p();
    }

    const string& pVar = pVars.front();

    LOG4FIMEX(logger, Logger::INFO, "using pressure field "<<pVar<<" to retrieve pressure");
    DataPtr pData = reader->getScaledDataSliceInUnit(pVar, "hPa", unLimDimPos);
    if (pData->size() != (nx * ny * nz * nt)) {
        throw CDMException("pressure field '" + pVar + "' has strange size: " + type2string(pData->size()) + " != " + type2string(nx * ny * nz * nt));
    }

    return ToVLevelConverter_p(new Identity4DToVLevelConverter(pData->asFloat(), nx, ny, nz, nt));
}

boost::shared_ptr<ToVLevelConverter> VerticalTransformation::findPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos,
        boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt) const
{
    boost::shared_ptr<ToVLevelConverter> pConv = getIdentityPressureConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    if (!pConv.get())
        pConv = getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt);
    return pConv;
}

} // namespace MetNoFimex
