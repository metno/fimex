/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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

#include "GribApiCDMWriter_Impl2.h"

#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/String2Type.h"
#include "fimex/Type2String.h"
#include "fimex/Units.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/HybridSigmaPressure1.h"

#include <libxml/tree.h>
#include <libxml/xpath.h>

#include <grib_api.h>

#include <cassert>
#include <cmath>

namespace {
double clamp_lon(double lon)
{
    while (lon < 0) {
        lon += 360;
    }
    while (lon > 360) {
        lon -= 360;
    }
    return lon;
}
} // namespace

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.GribApi_CDMWriter.Impl2");

GribApiCDMWriter_Impl2::GribApiCDMWriter_Impl2(CDMReader_p cdmReader, const std::string& outputFile, const std::string& configFile)
    : GribApiCDMWriter_ImplAbstract(2, cdmReader, outputFile, configFile)
{
}

GribApiCDMWriter_Impl2::~GribApiCDMWriter_Impl2() {}

void GribApiCDMWriter_Impl2::setParameter(const std::string& varName, double levelValue)
{
    LOG4FIMEX(logger, Logger::DEBUG, "setParameter(" << varName << ", " << levelValue << ")");
    xmlNodePtr node = getNodePtr(varName, levelValue);
    assert(node != 0);
    std::string parameter = getXmlProp(node, "parameterNumber");
    std::string category = getXmlProp(node, "parameterCategory");
    std::string discipline = getXmlProp(node, "discipline");
    if (parameter == "" || category == "" || discipline == "") {
        throw CDMException("incomplete defininition of " + varName + ": (param, categ, discipl) = (" + parameter + "," + category + "," + discipline + ")");
    }
    GRIB_CHECK(grib_set_long(gribHandle.get(), "parameterNumber", string2type<long>(parameter)), "");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "parameterCategory", string2type<long>(category)), "");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "discipline", string2type<long>(discipline)), "");

    // optional keys
    setNodesAttributes("attribute", node);
    setNodesAttributes("g2attribute", node);
}

void GribApiCDMWriter_Impl2::setProjection(const std::string& varName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "setProjection(" << varName << ")");
    const CDM& cdm = cdmReader->getCDM();
    // TODO: detect more projections
    Projection_cp proj = cdm.getProjectionOf(varName);
    if (proj.get() != 0) {
        CDM::AttrVec projAttrs = proj->getParameters();
        const std::string projection(proj->getName());
        if (projection == "stereographic" || projection == "polar_stereographic") {
            // latitude_of_projection_origin (polar_stereographic, +- 90), via scale_factor_at_projection_origin (stereographic
            // straight_vertical_longitude_from_pole (polar_stereographic), longitude_of_projection_origin (stereographic)
            double latitudeWhereDxAndDyAreSpecifiedInDegrees = 90.;
            double orientationOfTheGridInDegrees = 0.;
            if (projection == "polar_stereograhpic") {
                LOG4FIMEX(logger, Logger::INFO, "polar_stereographic projection for" << varName);
                // get lat_ts fixed
                latitudeWhereDxAndDyAreSpecifiedInDegrees = 90.;
                // get lon0
                CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("straight_vertical_longitude_from_pole"));
                if (ait != projAttrs.end()) {
                    orientationOfTheGridInDegrees = ait->getData()->asDouble()[0];
                }
            } else {
                LOG4FIMEX(logger, Logger::INFO, "stereographic projection for" << varName);
                // test stereographic is +- 90deg latitude (grib knows only polar-stereographic)
                CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("latitude_of_projection_origin"));
                if (ait != projAttrs.end()) {
                    double lat = ait->getData()->asDouble()[0];
                    if (std::fabs(lat) < 89.9995) {
                        throw CDMException("grib doesn't know general stereographic projection: found origin latitude " + type2string(lat));
                    }
                }
                // get lat_ts via scale_factor
                ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("scale_factor_at_projection_origin"));
                if (ait != projAttrs.end()) {
                    double scale = ait->getData()->asDouble()[0];
                    double x = 2 * scale - 1;
                    if (x <= 1 || x >= -1) {
                        latitudeWhereDxAndDyAreSpecifiedInDegrees = rad_to_deg(asin(2 * scale - 1));
                    } else {
                        throw CDMException("scale_factor_at_projection_origin not defined properly: abs(2*scale-1) >= 1: " + type2string(x));
                    }
                }
                // get lon0
                ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("longitude_of_projection_origin"));
                if (ait != projAttrs.end()) {
                    orientationOfTheGridInDegrees = ait->getData()->asDouble()[0];
                }
            }
            orientationOfTheGridInDegrees = clamp_lon(orientationOfTheGridInDegrees);
            std::string polar_stereographic("polar_stereographic");
            size_t ps_size = polar_stereographic.size();
            GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", polar_stereographic.c_str(), &ps_size), "");
            const DataPtr xData = cdmReader->getScaledDataInUnit(cdm.getHorizontalXAxis(varName), "m");
            const DataPtr yData = cdmReader->getScaledDataInUnit(cdm.getHorizontalYAxis(varName), "m");
            if (xData->size() < 2 || yData->size() < 2)
                throw CDMException(varName + " variable has to small x-y dimensions, not a grid for GRIB");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongXAxis", xData->size()), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongYAxis", yData->size()), "");
            const auto xArray = xData->asDouble();
            GRIB_CHECK(grib_set_double(gribHandle.get(), "DxInMetres", (xArray[1] - xArray[0])), "");
            const auto yArray = yData->asDouble();
            GRIB_CHECK(grib_set_double(gribHandle.get(), "DyInMetres", (yArray[1] - yArray[0])), "");
            std::string latitude, longitude;
            if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
                double lon = clamp_lon(cdmReader->getScaledDataInUnit(longitude, "degree")->asDouble()[0]);
                double lat = cdmReader->getScaledDataInUnit(latitude, "degree")->asDouble()[0];
                GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", lat), "");
                GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", lon), "");
            } else {
                throw CDMException("unable to find latitude/longitude for variable " + varName);
            }
            GRIB_CHECK(grib_set_double(gribHandle.get(), "orientationOfTheGridInDegrees", orientationOfTheGridInDegrees), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", latitudeWhereDxAndDyAreSpecifiedInDegrees), "");
        } else if (projection == "latitude_longitude") {
            LOG4FIMEX(logger, Logger::INFO, "latlong projection for" << varName);
            std::string latitude, longitude;
            if (!cdm.getLatitudeLongitude(varName, latitude, longitude))
                throw CDMException("could not find latitude/longitude for varName: " + varName);
            DataPtr lonData = cdmReader->getScaledDataInUnit(longitude, "degree");
            DataPtr latData = cdmReader->getScaledDataInUnit(latitude, "degree");
            const size_t ni = lonData->size();
            const size_t nj = latData->size();
            if (ni < 2 || nj < 2) {
                throw CDMException("longitude, latitude for varName " + varName + " has to small dimension for grid: (" + type2string(ni) + "," +
                                   type2string(nj) + ")");
            }
            const auto longs = lonData->asDouble();
            const auto lats = latData->asDouble();
            const double di = longs[1] - longs[0];
            const double dj = lats[1] - lats[0];
            const double lat0 = lats[0];
            const double latX = lats[nj - 1];
            const double lon0 = clamp_lon(longs[0]);
            const double lonX = clamp_lon(longs[ni - 1]);
            // TODO: untested
            const std::string typeOfGrid("regular_ll");
            size_t tog_size = typeOfGrid.size();
            GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", typeOfGrid.c_str(), &tog_size), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAParallel", ni), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAMeridian", nj), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "iDirectionIncrementInDegrees", fabs(di)), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "jDirectionIncrementInDegrees", fabs(dj)), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", lat0), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", lon0), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfLastGridPointInDegrees", latX), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfLastGridPointInDegrees", lonX), "");
            if (dj < 0) {
                // reading north -> south
                GRIB_CHECK(grib_set_long(gribHandle.get(), "jScansPositively", 0), "");
            } else {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "jScansPositively", 1), "");
            }
            if (di < 0) {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "iScansNegatively", 1), "");
            } else {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "iScansNegatively", 0), "");
            }

        } else if (projection == "rotated_latitude_longitude") {
            LOG4FIMEX(logger, Logger::INFO, "rotated latlong projection for " << varName);
            const std::string rotLon = cdm.getHorizontalXAxis(varName);
            const std::string rotLat = cdm.getHorizontalYAxis(varName);
            DataPtr rLonData = cdmReader->getScaledDataInUnit(rotLon, "degree");
            DataPtr rLatData = cdmReader->getScaledDataInUnit(rotLat, "degree");
            size_t ni = rLonData->size();
            size_t nj = rLatData->size();
            if (ni < 2 || nj < 2) {
                throw CDMException("(ni,nj) for varName " + varName + " has to small dimension for grid: (" + type2string(ni) + "," + type2string(nj) + ")");
            }
            double di, dj, rlon0, rlat0, rlonX, rlatX;
            const auto rlongs = rLonData->asDouble();
            const auto rlats = rLatData->asDouble();
            di = rlongs[1] - rlongs[0];
            dj = rlats[1] - rlats[0];
            rlat0 = rlats[0];
            rlatX = rlats[nj - 1];
            rlon0 = clamp_lon(rlongs[0]);
            rlonX = clamp_lon(rlongs[ni - 1]);
            CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("grid_north_pole_longitude"));
            if (ait == projAttrs.end())
                throw CDMException("grid_north_pole_longitude not found for projection " + proj->toString());
            double northPoleLon = ait->getData()->asDouble()[0];
            ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("grid_north_pole_latitude"));
            if (ait == projAttrs.end())
                throw CDMException("grid_north_pole_latitude not found for projection " + proj->toString());
            double northPoleLat = ait->getData()->asDouble()[0];

            double southPoleLat = -1 * northPoleLat;
            while (southPoleLat < -90) {
                southPoleLat += 180;
            }

            double southPoleLon = clamp_lon(northPoleLon - 180);

            std::string typeOfGrid("rotated_ll");
            // TODO: this seems still to be inperfect, more tests required
            size_t tog_size = typeOfGrid.size();
            GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", typeOfGrid.c_str(), &tog_size), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAParallel", ni), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "numberOfPointsAlongAMeridian", nj), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "iDirectionIncrementInDegrees", fabs(di)), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "jDirectionIncrementInDegrees", fabs(dj)), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "latitudeOfSouthernPoleInDegrees", static_cast<long>(southPoleLat)), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "longitudeOfSouthernPoleInDegrees", static_cast<long>(southPoleLon)), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", rlat0), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", rlon0), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfLastGridPointInDegrees", rlatX), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfLastGridPointInDegrees", rlonX), "");
            if (dj < 0) {
                // reading north -> south
                GRIB_CHECK(grib_set_long(gribHandle.get(), "jScansPositively", 0), "");
            } else {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "jScansPositively", 1), "");
            }
            if (di < 0) {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "iScansNegatively", 1), "");
            } else {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "iScansNegatively", 0), "");
            }

        } else if (projection == "lambert_conformal_conic") {
            double lat0, lat1, lat2, lonV;
            lat0 = 0.;
            lat1 = 60.;
            lat2 = 60.;
            lonV = 0.;
            CDM::AttrVec::iterator ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("standard_parallel"));
            if (ait != projAttrs.end()) {
                DataPtr d = ait->getData();
                if (d->size() == 1) {
                    lat1 = d->asDouble()[0];
                    lat2 = lat1;
                } else if (d->size() > 1) {
                    lat1 = d->asDouble()[0];
                    lat2 = d->asDouble()[1];
                }
            } else {
                throw CDMException("standard_parallel not found for projection " + proj->toString());
            }
            ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("longitude_of_central_meridian"));
            if (ait != projAttrs.end()) {
                lonV = clamp_lon(ait->getData()->asDouble()[0]);
            }
            ait = find_if(projAttrs.begin(), projAttrs.end(), CDMNameEqual("latitude_of_projection_origin"));
            if (ait != projAttrs.end()) {
                LOG4FIMEX(logger, Logger::WARN, "lambert-projection: latitude_of_projection_origin not usable in grib-api");
                lat0 = ait->getData()->asDouble()[0];
            }
            const DataPtr xData = cdmReader->getScaledDataInUnit(cdm.getHorizontalXAxis(varName), "m");
            const DataPtr yData = cdmReader->getScaledDataInUnit(cdm.getHorizontalYAxis(varName), "m");
            if (xData->size() < 2 || yData->size() < 2)
                throw CDMException(varName + " variable has too small x-y dimensions, not a grid for GRIB");
            auto xArray = xData->asDouble();
            double dx = xArray[1] - xArray[0];
            auto yArray = yData->asDouble();
            double dy = yArray[1] - yArray[0];
            std::string typeOfGrid("lambert");
            size_t tog_size = typeOfGrid.size();
            GRIB_CHECK(grib_set_string(gribHandle.get(), "typeOfGrid", typeOfGrid.c_str(), &tog_size), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "Nx", xData->size()), "");
            GRIB_CHECK(grib_set_long(gribHandle.get(), "Ny", yData->size()), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "DxInMetres", fabs(dx)), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "DyInMetres", fabs(dy)), "");
            //  this one does not work for reading or writing in grib-api
            // GRIB_CHECK(grib_set_double(gribHandle.get(), "LaDInDegrees", lat0), "");
            // avoid unused lat0 warning
            lat0 *= 1.;
            GRIB_CHECK(grib_set_double(gribHandle.get(), "Latin1InDegrees", lat1), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "Latin2InDegrees", lat2), "");
            GRIB_CHECK(grib_set_double(gribHandle.get(), "LoVInDegrees", lonV), "");
            std::string latitude, longitude;
            if (cdm.getLatitudeLongitude(varName, latitude, longitude)) {
                const double lat_1st = cdmReader->getScaledDataInUnit(latitude, "degree")->asDouble()[0];
                const double lon_1st = clamp_lon(cdmReader->getScaledDataInUnit(longitude, "degree")->asDouble()[0]);
                GRIB_CHECK(grib_set_double(gribHandle.get(), "latitudeOfFirstGridPointInDegrees", lat_1st), "");
                GRIB_CHECK(grib_set_double(gribHandle.get(), "longitudeOfFirstGridPointInDegrees", lon_1st), "");
            } else {
                throw CDMException("unable to find latitude/longitude for variable " + varName);
            }
            if (dy < 0) {
                // reading north -> south
                GRIB_CHECK(grib_set_long(gribHandle.get(), "jScansPositively", 0), "");
            } else {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "jScansPositively", 1), "");
            }
            if (dx < 0) {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "iScansNegatively", 1), "");
            } else {
                GRIB_CHECK(grib_set_long(gribHandle.get(), "iScansNegatively", 0), "");
            }
        } else if (projection == "transverse_mercator") {
            throw CDMException("grid_mapping_name " + projection + " not supported yet by GribApiCDMWriter");
        } else {
            throw CDMException("grid_mapping_name " + projection + " not supported yet by GribApiCDMWriter");
        }
    } else {
        throw CDMException("No projection found");
    }
}

void GribApiCDMWriter_Impl2::setLevel(const std::string& varName, double levelValue, size_t levelPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "setLevel(" << varName << ", " << levelValue << ", " << levelPos << " )");
    // check for level/parameter dependencies
    const CDM& cdm = cdmReader->getCDM();
    std::string verticalAxis = cdm.getVerticalAxis(varName);
    std::string verticalAxisXPath("/cdm_gribwriter_config/axes/vertical_axis");
    if (verticalAxis != "") {
        CDMAttribute attr;
        if (cdm.getAttribute(verticalAxis, "standard_name", attr)) {
            verticalAxisXPath += "[@standard_name=\"" + attr.getData()->asString() + "\"]";
        } else if (cdm.getAttribute(verticalAxis, "units", attr)) {
            // units compatible to Pa or m
            std::string unit = attr.getStringValue();
            Units units;
            if (units.areConvertible(unit, "m")) {
                verticalAxisXPath += "[@unitCompatibleTo=\"m\"]";
            } else if (units.areConvertible(unit, "Pa")) {
                verticalAxisXPath += "[@unitCompatibleTo=\"Pa\"]";
            } else {
                throw CDMException("units of vertical axis " + verticalAxis + " should be compatible with m or Pa but are: " + unit);
            }
        } else {
            throw CDMException("couldn't find standard_name or units for vertical Axis " + verticalAxis + ". Is this CF compatible?");
        }
    } else {
        // cdmGribWriterConfig should contain something like standard_name=""
        verticalAxisXPath += "[@standard_name=\"\"]";
    }
    bool isHybrid = false;
    verticalAxisXPath += "/grib2";
    xmlXPathObject_p verticalXPObj = xmlConfig->getXPathObject(verticalAxisXPath);
    xmlNodeSetPtr nodes = verticalXPObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size == 1) {
        xmlNodePtr node = nodes->nodeTab[0];
        std::string levelId = getXmlProp(node, "id");
        GRIB_CHECK(grib_set_long(gribHandle.get(), "levelType", string2type<long>(levelId)), ("setting levelId " + levelId).c_str());
        if (string2type<long>(levelId) == 105) {
            // hybrid level, add both ap and b to pv
            isHybrid = true;
            CoordinateSystem_cp_v coordsys = listCoordinateSystems(cdmReader);
            CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordsys, varName);
            if (cs->hasVerticalTransformation()) {
                VerticalTransformation_cp vtrans = cs->getVerticalTransformation();
                if (vtrans->getName() == HybridSigmaPressure1::NAME() && vtrans->isComplete()) {
                    auto hsp = std::dynamic_pointer_cast<const HybridSigmaPressure1>(vtrans);
                    LOG4FIMEX(logger, Logger::DEBUG, "found hybrid-sigma-transformation with ap: " << hsp->ap << " and b " << hsp->b);
                    auto apData = cdmReader->getScaledDataInUnit(hsp->ap, "Pa");
                    auto apD = apData->asDouble();
                    auto bData = cdmReader->getScaledData(hsp->b);
                    auto bD = bData->asDouble();
                    std::vector<double> pvAp;
                    std::vector<double> pvB;
                    // convert from mid-levels to level border, i.e. ml[i] = (lb[i-1]+lb[i])/2
                    // see also GribCDMReader::readValuesFromXPath_ for extraHalvLevels
                     // surface is initial
                    pvAp.push_back(0.);
                    pvB.push_back(1.);
                    if (bD[0] > bD[bData->size()-1]) { // surface is first level
                        for (int i = 0; i < apData->size(); i++) {
                            pvAp.push_back(2*apD[i]-pvAp[i]);
                            pvB.push_back(2*bD[i]-pvB[i]);
                        }
                    } else { // surface is last level
                        int last = apData->size() - 1;
                        for (int i = 0; i < apData->size(); i++) {
                            pvAp.push_back(2*apD[last-i]-pvAp[i]);
                            pvB.push_back(2*bD[last-i]-pvB[i]);
                        }
                        std::reverse(pvAp.begin(), pvAp.end());
                        std::reverse(pvB.begin(), pvB.end());
                    }
                    std::vector<double> pv;
                    pv.insert(pv.end(), &pvAp[0], &pvAp[0]+pvAp.size());
                    pv.insert(pv.end(), &pvB[0], &pvB[0]+pvB.size());
                    GRIB_CHECK(grib_set_long(gribHandle.get(), "PVPresent", 1), 0);
                    GRIB_CHECK(grib_set_double_array(gribHandle.get(), "pv", &pv[0], pv.size()), 0);
                } else {
                    LOG4FIMEX(logger, Logger::ERROR, "no hybrid-sigma-transformation for var " << varName << " found, cannot write pv to grib2");
                }
            } else {
                LOG4FIMEX(logger, Logger::ERROR, "no hybrid-sigma-transformation for var " << varName << " found, cannot write pv to grib2");
            }
        }
    } else if (size > 1) {
        throw CDMException("several entries in grib-config at " + configFile + ": " + verticalAxisXPath);
    } else {
        throw CDMException("could not find vertical Axis " + verticalAxisXPath + " in " + configFile + ", skipping parameter " + varName);
    }
    if (isHybrid) {
        // hybrid level, no value, just position in pv
        GRIB_CHECK(grib_set_long(gribHandle.get(), "level", levelPos+1), "setting level position");
    } else {
        GRIB_CHECK(grib_set_long(gribHandle.get(), "level", static_cast<long>(levelValue)), "setting level");
    }
}

DataPtr GribApiCDMWriter_Impl2::handleTypeScaleAndMissingData(const std::string& varName, double levelValue, DataPtr inData)
{
    LOG4FIMEX(logger, Logger::DEBUG, "handleTypeScaleAndMissingData(" << varName << ", " << levelValue << ")");
    const CDM& cdm = cdmReader->getCDM();
    double inFillValue = cdm.getFillValue(varName);
    double outFillValue = inFillValue;
    // missingValue will not be encoded in message, but in a separate bitmap
    GRIB_CHECK(grib_set_double(gribHandle.get(), "missingValue", outFillValue), "setting missing value");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "bitmapPresent", 1), "setting bitmap");

    double scale = cdm.getScaleFactor(varName);
    double offset = cdm.getAddOffset(varName);
    // scale and offset by units
    const std::string unit = cdm.getUnits(varName);
    if (!unit.empty()) {
        xmlNodePtr node = getNodePtr(varName, levelValue);
        std::string gUnit = getXmlProp(node, "units");
        if (gUnit != "") {
            double slope, uOffset;
            Units u;
            u.convert(unit, gUnit, slope, uOffset);
            // join both scalings: (scale*x + offset)*slope + uOffset
            scale *= slope;
            offset *= slope;
            offset += uOffset;
        }
    }

    LOG4FIMEX(logger, Logger::DEBUG,
              "change from (" << inFillValue << " " << scale << "," << offset << ") to (" << outFillValue << "," << 1 << "," << 0 << ")");

    return inData->convertDataType(inFillValue, scale, offset, CDM_DOUBLE, outFillValue, 1, 0);
}

} // namespace MetNoFimex
