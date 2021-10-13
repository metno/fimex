/*
 * Fimex, GribFileIndex.cc
 *
 * (C) Copyright 2009-2019, met.no
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
 *  Created on: Aug 31, 2009
 *      Author: Heiko Klein
 */

#include "fimex/GribFileIndex.h"

#include "fimex/CDMException.h"
#include "fimex/DataUtils.h"
#include "fimex/GribUtils.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/TimeUtils.h"
#include "fimex/Type2String.h"
#include "fimex/XMLUtils.h"
#include "fimex/interpolation.h"

#include <date/date.h>

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <libxml/tree.h>
#include <libxml/xmlreader.h>
#include <libxml/xmlwriter.h>
#include <libxml/xpath.h>

#include "grib_api.h"

namespace MetNoFimex {

using namespace std;

namespace {

Logger_p logger = getLogger("fimex.GribFileIndex");
Logger_p loggerGFM = getLogger("fimex.GribFileMessage");

typedef std::shared_ptr<grib_handle> grib_handle_p;

typedef std::shared_ptr<FILE> FILE_p;

FILE_p file_open_seek(const std::string& path, size_t position)
{
    FILE* fileh = fopen(path.c_str(), "rb");
    if (!fileh)
        throw runtime_error("cannot open file '" + path + "'");

    FILE_p fh(fileh, fclose);
    fseeko(fh.get(), position, SEEK_SET);
    return fh;
}

grib_handle_p make_grib_handle(FILE_p fh, int& err)
{
    return grib_handle_p(grib_handle_new_from_file(0, fh.get(), &err), grib_handle_delete);
}

int grib_get_nocheck(grib_handle_p gh, const char* key, std::string& value)
{
    char msg[1024];
    size_t msgLength = sizeof(msg);
    const int err = grib_get_string(gh.get(), key, msg, &msgLength);
    if (err == GRIB_SUCCESS)
        value = string(msg, msg + msgLength - 1);
    return err;
}

void grib_get(grib_handle_p gh, const char* key, std::string& value)
{
    char msg[1024];
    size_t msgLength = sizeof(msg);
    MIFI_GRIB_CHECK(grib_get_string(gh.get(), key, msg, &msgLength), 0);
    value = string(msg, msg + msgLength - 1);
}

void grib_set(grib_handle_p gh, const char* key, const std::string& value)
{
    size_t len = value.length();
    MIFI_GRIB_CHECK(grib_set_string(gh.get(), key, value.c_str(), &len), key);
}

int grib_get_nocheck(grib_handle_p gh, const char* key, double& value)
{
    return grib_get_double(gh.get(), key, &value);
}

void grib_get(grib_handle_p gh, const char* key, double& value)
{
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), key, &value), key);
}

void grib_get(grib_handle_p gh, const char* key, size_t& value)
{
    MIFI_GRIB_CHECK(grib_get_size(gh.get(), key, &value), key);
}

int grib_get_nocheck(grib_handle_p gh, const char* key, long& value)
{
    return grib_get_long(gh.get(), key, &value);
}

void grib_set(grib_handle_p gh, const char* key, long value)
{
    MIFI_GRIB_CHECK(grib_set_long(gh.get(), key, value), key);
}

void grib_get(grib_handle_p gh, const char* key, long& value)
{
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), key, &value), key);
}

void grib_get(grib_handle_p gh, const char* key, long& value, long dflt)
{
    int err = grib_get_nocheck(gh, key, value);
    if (err == GRIB_NOT_FOUND)
        value = dflt;
    else
        MIFI_GRIB_CHECK(err, key);
}

/**
 * @warning This variable is only for functions inside the GribFileIndex initialization.
 *       Don't use otherwise.
 */
std::string earthFigure_;

void projConvert(const std::string& projStr, double lon, double lat, double& x, double& y)
{
    if (mifi_reproject_point_from_lonlat(projStr.c_str(), &lon, &lat) != MIFI_OK)
        throw std::runtime_error("point reprojection error");
    x = lon;
    y = lat;
}

std::string getEarthsOblateFigure(grib_handle_p gh, long factorToM)
{
    long majorFactor, minorFactor;
    double majorValue, minorValue;
    grib_get(gh, "scaleFactorOfMajorAxisOfOblateSpheroidEarth", majorFactor);
    grib_get(gh, "scaledValueOfMajorAxisOfOblateSpheroidEarth", majorValue);
    while (majorFactor > 0) {majorValue *= 10; majorFactor--;}
    while (majorFactor < 0) {majorValue /= 10; majorFactor++;}
    // transfer km to m
    majorFactor *= factorToM;

    grib_get(gh, "scaleFactorOfMinorAxisOfOblateSpheroidEarth", minorFactor);
    grib_get(gh, "scaledValueOfMinorAxisOfOblateSpheroidEarth", minorValue);
    while (minorFactor > 0) {minorValue *= 10; minorFactor--;}
    while (minorFactor < 0) {minorValue /= 10; minorFactor++;}
    // transfer (km|m) to m
    minorFactor *= factorToM;

    return "+a=" + type2string(majorValue) + " +b=" + type2string(minorValue);
}

std::string getEarthsSphericalFigure(grib_handle_p gh)
{
    double radius;
    grib_get(gh, "radiusInMetres", radius);
    return "+a=" + type2string(radius) + " +e=0";
}

std::string getEarthsFigure(long edition, grib_handle_p gh)
{
    if (earthFigure_ != "") {
        return earthFigure_;
    }
    // earth specific parameters, depending on grib-edition
    string earth;
    if (edition == 1) {
        long earthIsOblate;
        grib_get(gh, "earthIsOblate", earthIsOblate);
        if (earthIsOblate == 0) {
            earth = "+a=6367470 +e=0"; // sphere
        } else {
            earth = "+a=6378160 +b=6356775"; // oblate, maybe more or better defined in grib2???
        }
    } else {
        long shapeOfTheEarth;
        grib_get(gh, "shapeOfTheEarth", shapeOfTheEarth);
        switch (shapeOfTheEarth) { // see code table 3.2
            case 0: earth = "+a=6367470 +e=0"; break;
            case 1: earth = getEarthsSphericalFigure(gh); break;
            case 2: earth = "+a=6378160 +b=6356775"; break;
            case 3: earth = getEarthsOblateFigure(gh, 1000); break;// number in km
            case 4: earth = "+a=6378137 +b=6356752.314"; break;
            case 5: earth = "+a=6378137 +b=6356752.314245"; break;// WGS84
            case 6: earth = "+a=6371229"; break;
            case 7: earth = getEarthsOblateFigure(gh, 1); break;// numbers in m
            case 8: earth = "+a=6371200 +e=0"; break;// TODO: definition not fully understood
            default: throw CDMException("undefined shape of the earth: " + type2string(shapeOfTheEarth));
        }
    }
    return earth;
}

GridDefinition getGridDefRegularLL(long edition, grib_handle_p gh)
{
    long sizeX, sizeY, ijDirectionIncrementGiven;
    double startX, startY, incrX, incrY;
    grib_get(gh, "Ni", sizeX);
    grib_get(gh, "Nj", sizeY);
    grib_get(gh, "longitudeOfFirstGridPointInDegrees", startX);
    grib_get(gh, "latitudeOfFirstGridPointInDegrees", startY);

    GridDefinition::Orientation orient = gribGetGridOrientation(gh);
    grib_get(gh, "ijDirectionIncrementGiven", ijDirectionIncrementGiven);
    double endX, endY;
    grib_get(gh, "longitudeOfLastGridPointInDegrees", endX);
    grib_get(gh, "latitudeOfLastGridPointInDegrees", endY);
    incrX = (endX - startX) / (sizeX-1);
    incrY = (endY - startY) / (sizeY-1);
    if (ijDirectionIncrementGiven != 0) {
        double incrX1, incrY1;
        grib_get(gh, "iDirectionIncrementInDegrees", incrX1);
        grib_get(gh, "jDirectionIncrementInDegrees", incrY1);
        if (orient & GridDefinition::ScanStartRight) {
            incrX1 *= -1;
        }
        if (!(orient & GridDefinition::ScanStartBottom)) {
            incrY1 *= -1;
        }
        // resolution of ijDirection is 0.001degree in grib1, calculation via first/last point is much more accurate
        if (fabs(incrX1-incrX) > 0.001) {
            incrX = incrX1; // strange behaviour, maybe going over 0/360 degree, better to use increment here
        }
        if (fabs(incrY1-incrY) > 0.001) {
            incrY = incrY1; // strange behaviour, maybe going over 0/360 degree, better to use increment here
        }
    }

    string proj = "+proj=longlat " + getEarthsFigure(edition, gh) + " +no_defs";

    LOG4FIMEX(logger, Logger::DEBUG, "getting griddefinition: " << proj << ": (" << startX << "," << startY << "), (" << incrX << "," << incrY << ")");
    return GridDefinition(proj, true, sizeX, sizeY, incrX, incrY, startX, startY, orient);
}

GridDefinition getGridDefRotatedLL(long edition, grib_handle_p gh)
{
    long sizeX, sizeY, ijDirectionIncrementGiven;
    double startX, startY, incrX, incrY, latRot, lonRot;
    grib_get(gh, "Ni", sizeX);
    grib_get(gh, "Nj", sizeY);
    grib_get(gh, "longitudeOfFirstGridPointInDegrees", startX);
    grib_get(gh, "latitudeOfFirstGridPointInDegrees", startY);
    grib_get(gh, "longitudeOfSouthernPoleInDegrees", lonRot);
    grib_get(gh, "latitudeOfSouthernPoleInDegrees", latRot);

    GridDefinition::Orientation orient = gribGetGridOrientation(gh);
    grib_get(gh, "ijDirectionIncrementGiven", ijDirectionIncrementGiven);
    if (ijDirectionIncrementGiven == 0) {
        double endX, endY;
        grib_get(gh, "longitudeOfLastGridPointInDegrees", endX);
        grib_get(gh, "latitudeOfLastGridPointInDegrees", endY);
        incrX = (endX - startX) / sizeX;
        incrY = (endY - startY) / sizeY;
    } else {
        grib_get(gh, "iDirectionIncrementInDegrees", incrX);
        grib_get(gh, "jDirectionIncrementInDegrees", incrY);
        if (orient & GridDefinition::ScanStartRight) {
            incrX *= -1;
        }
        if (!(orient & GridDefinition::ScanStartBottom)) {
            incrY *= -1;
        }
    }

    ostringstream oss;
    oss << "+proj=ob_tran +o_proj=longlat +lon_0=" << normalizeLongitude180(lonRot) << " +o_lat_p=" << (-1 * latRot);
    oss << " " << getEarthsFigure(edition, gh) <<  " +no_defs";
    string proj =  oss.str();

    return GridDefinition(proj, true, sizeX, sizeY, incrX, incrY, startX, startY, orient);
}

struct GribMetricDef {
    long sizeX;
    long sizeY;
    double incrX;
    double incrY;
    double startLon;
    double startLat;
};

GribMetricDef getGridDefMetric(long /*edition*/, grib_handle_p gh)
{
    GribMetricDef gmd;
    grib_get(gh, "Ni", gmd.sizeX);
    grib_get(gh, "Nj", gmd.sizeY);
    grib_get(gh, "longitudeOfFirstGridPointInDegrees", gmd.startLon);
    grib_get(gh, "latitudeOfFirstGridPointInDegrees", gmd.startLat);

    if (grib_get_nocheck(gh, "DxInMetres", gmd.incrX) != GRIB_SUCCESS) {
        // = DxInMetres, DiInMetres, Di
        grib_get(gh, "DiInMetres", gmd.incrX);
    }
    if (grib_get_nocheck(gh, "DyInMetres", gmd.incrY) != GRIB_SUCCESS) {
        // = DyInMetres, DjInMetres, Dj
        grib_get(gh, "DjInMetres", gmd.incrY);
    }

    return gmd;
}

GridDefinition getGridDefMercator(long edition, grib_handle_p gh)
{
    double startX, startY;
    GribMetricDef gmd = getGridDefMetric(edition, gh);


    double orientationOfGrid, lat_ts;
    grib_get(gh, "LaDInDegrees", lat_ts);
    grib_get(gh, "orientationOfTheGridInDegrees", orientationOfGrid);

    ostringstream oss;
    oss << "+proj=merc +lon_0="<<orientationOfGrid  << " +lat_ts=" << lat_ts << " ";
    oss << getEarthsFigure(edition, gh) << " +no_defs";
    string proj = oss.str();

    // calculate startX and startY from lat/lon
    projConvert(proj, gmd.startLon, gmd.startLat, startX, startY);

    return GridDefinition(proj, false, gmd.sizeX, gmd.sizeY, gmd.incrX, gmd.incrY, startX, startY, gribGetGridOrientation(gh));
}

GridDefinition getGridDefLambert(long edition, grib_handle_p gh)
{
    double startX, startY;
    GribMetricDef gmd = getGridDefMetric(edition, gh);


    double lonV, lat1, lat2;
    // TODO: southPole position not used yet
    // grib_get(gh, "latitudeOfSouthernPoleInDegrees", southPLat);
    // grib_get(gh, "longitudeOfSouthernPoleInDegrees", southPLon);
// ignore LaD, does not make sense for lambert and grib_api returns 60, which is wrong
// grib_get(gh, "LaDInDegrees", latD);
    grib_get(gh, "LoVInDegrees", lonV);
    grib_get(gh, "Latin1InDegrees", lat1);
    grib_get(gh, "Latin2InDegrees", lat2);

    ostringstream oss;
    oss << "+proj=lcc +lat_0="<<lat1 << " +lon_0="<< lonV << " +lat_1=" << lat1 << " " << " +lat_2=" << lat2 << " ";
    oss << getEarthsFigure(edition, gh) << " +no_defs";
    string proj = oss.str();

    // calculate startX and startY from lat/lon
    projConvert(proj, gmd.startLon, gmd.startLat, startX, startY);
    LOG4FIMEX(logger, Logger::DEBUG, "startpos: (lon,lat) " << gmd.startLon  << ", " << gmd.startLat << "  (x,y)= " << startX << "," << startY << " projStr" << proj);
    return GridDefinition(proj, false, gmd.sizeX, gmd.sizeY, gmd.incrX, gmd.incrY, startX, startY, gribGetGridOrientation(gh));
}

GridDefinition getGridDefPolarStereographic(long edition, grib_handle_p gh)
{
    double startX, startY;
    GribMetricDef gmd = getGridDefMetric(edition, gh);


    long projectionCentreFlag;
    double orientationOfGrid, lat0, lat_ts;
    string earth;
    grib_get(gh, "orientationOfTheGridInDegrees", orientationOfGrid);
    if (edition == 1) {
        // grib_get(gh, "latitudeWhereDxAndDyAreSpecifiedInDegrees", lat_ts); // defined in grib-api > 1.6 to 60
        lat_ts = 60.;
        grib_get(gh, "projectionCenterFlag", projectionCentreFlag); // changed to centre in grib-api > 1.8
    } else {
        grib_get(gh, "latitudeWhereDxAndDyAreSpecifiedInDegrees", lat_ts);
        grib_get(gh, "projectionCentreFlag", projectionCentreFlag);
    }
    if (projectionCentreFlag == 0) {
        lat0 = 90.; // northpole
    } else {
        lat0 = -90.; // southpole
    }
    ostringstream oss;
    oss << "+proj=stere +lat_0="<<lat0 << " +lon_0="<< orientationOfGrid << " +lat_ts=" << lat_ts << " ";
    oss << getEarthsFigure(edition, gh) << " +no_defs";
    string proj = oss.str();

    // calculate startX and startY from lat/lon
    projConvert(proj, gmd.startLon, gmd.startLat, startX, startY);

    return GridDefinition(proj, false, gmd.sizeX, gmd.sizeY, gmd.incrX, gmd.incrY, startX, startY, gribGetGridOrientation(gh));
}

const char GK_dataDate[] = "dataDate";
const char GK_edition[] = "edition";
const char GK_endStep[] = "endStep";
const char GK_level[] = "level";
const char GK_levtype[] = "levtype";
const char GK_numberOfForecastsInEnsemble[] = "numberOfForecastsInEnsemble";
const char GK_parameterNumber[] = "parameterNumber";
const char GK_perturbationNumber[] = "perturbationNumber";
const char GK_productDefinitionTemplateNumber[] = "productDefinitionTemplateNumber";
const char GK_startStep[] = "startStep";
const char GK_stepUnits[] = "stepUnits";
const char GK_time[] = "time";
const char GK_totalNumberOfClusters[] = "totalNumberOfClusters";
const char GK_typeOfGrid[] = "typeOfGrid";

// currently unused
const char GK_indicatorOfUnitOfTimeRange[] = "indicatorOfUnitOfTimeRange";

} // namespace

const char GK_discipline[] = "discipline";
const char GK_gribTablesVersionNo[] = "gribTablesVersionNo";
const char GK_identificationOfOriginatingGeneratingCentre[] = "identificationOfOriginatingGeneratingCentre";
const char GK_indicatorOfParameter[] = "indicatorOfParameter";
const char GK_parameterCategory[] = "parameterCategory";
const char GK_stepType[] = "stepType";
const char GK_timeRangeIndicator[] = "timeRangeIndicator";
const char GK_typeOfStatisticalProcessing[] = "typeOfStatisticalProcessing";

GribFileMessage::GribFileMessage(grib_handle_p gh, const std::string& fileURL, long filePos, long msgPos,
                                 const std::vector<std::pair<std::string, std::regex>>& members, const std::vector<std::string>& extraKeys)
    : fileURL_(fileURL)
    , filePos_(filePos)
    , msgPos_(msgPos)
{
    if (!gh) {
        throw runtime_error("GribFileMessage initialized with NULL-ptr");
    }

    grib_get(gh, GK_edition, edition_);

    if (edition_ == 1) {
        gridParameterIds_ = vector<long> (3, 0);
        grib_get(gh, GK_indicatorOfParameter, gridParameterIds_[0]);
        grib_get(gh, GK_gribTablesVersionNo, gridParameterIds_[1]);
        grib_get(gh, "centre", gridParameterIds_[2]);
        if (gridParameterIds_[0] == 254) {
            long level = -1;
            grib_get(gh, GK_level, level);
            if (level == GRIB_MISSING_LONG) {
                throw CDMException("Asimof-message deteced, cannot decode this message");
            }
        }
    } else if (edition_ == 2) {
        gridParameterIds_ = vector<long> (3, 0);
        grib_get(gh, GK_parameterNumber, gridParameterIds_[0]);
        grib_get(gh, GK_parameterCategory, gridParameterIds_[1]);
        grib_get(gh, GK_discipline, gridParameterIds_[2]);
    } else {
        throw runtime_error("unknown grib version: " + type2string(edition_));
    }

    const int nameError = grib_get_nocheck(gh, "name", parameterName_);
    if ((nameError != GRIB_NOT_FOUND) && (parameterName_ != "unknown")) {
        MIFI_GRIB_CHECK(nameError, 0);
    } else {
        parameterName_ = join(gridParameterIds_.begin(), gridParameterIds_.end(), ",");
    }
    const int shortNameError = grib_get_nocheck(gh, "shortName", shortName_);
    if ((shortNameError != GRIB_NOT_FOUND) && (shortName_ != "unknown")) {
        MIFI_GRIB_CHECK(shortNameError, 0);
    } else {
        shortName_ = join(gridParameterIds_.begin(), gridParameterIds_.end(), "_");
    }

    for (std::vector<std::string>::const_iterator keyIt = extraKeys.begin(); keyIt != extraKeys.end(); ++keyIt) {
        long val;
        const int err = grib_get_nocheck(gh, keyIt->c_str(), val);
        if (err != GRIB_NOT_FOUND) {
            MIFI_GRIB_CHECK(err, 0); // other errors, or no errors
            otherKeys_[*keyIt] = val;
        }
    }

    // level
    grib_get(gh, GK_levtype, levelType_);
    grib_get(gh, GK_level, levelNo_);

    // time
    {
        // read files internal time-step unit
        // https://confluence.ecmwf.int/display/ECC/Frequently+Asked+Questions#FrequentlyAskedQuestions-ConfusedaboutstepUnits?
        // use indicatorOfUnitOfTimeRange: https://software.ecmwf.int/issues/browse/SUP-1264
        long iouotr = 255;
        grib_get(gh, GK_indicatorOfUnitOfTimeRange, iouotr);
        if (iouotr != 255)
            grib_set(gh, GK_stepUnits, iouotr);
    }
    grib_get(gh, GK_stepUnits, stepUnits_);
    grib_get(gh, GK_stepType, stepType_);
    grib_get(gh, GK_dataDate, dataDate_);
    grib_get(gh, GK_time, dataTime_);
    grib_get(gh, GK_timeRangeIndicator, timeRangeIndicator_);
    grib_get(gh, GK_typeOfStatisticalProcessing, typeOfStatisticalProcessing_, -1);
    grib_get(gh, GK_startStep, stepStart_);
    grib_get(gh, GK_endStep, stepEnd_);

    // ensemble
    int gribError = grib_get_nocheck(gh, GK_numberOfForecastsInEnsemble, totalNumberOfEnsembles_);
    switch (gribError) {
    case GRIB_SUCCESS: {
        int gribError_pn = grib_get_nocheck(gh, GK_perturbationNumber, perturbationNo_);
        if (gribError_pn == GRIB_NOT_FOUND) {
            perturbationNo_ = 0;
            totalNumberOfEnsembles_ = 0;
            long productDefinitionTemplateNo;
            int gribError_pdn = grib_get_nocheck(gh, GK_productDefinitionTemplateNumber, productDefinitionTemplateNo);
            if ((gribError_pdn == GRIB_SUCCESS) && (productDefinitionTemplateNo == 2 || productDefinitionTemplateNo == 3 || productDefinitionTemplateNo == 4 ||
                    productDefinitionTemplateNo == 12 || productDefinitionTemplateNo == 13 || productDefinitionTemplateNo == 14)) {
                LOG4FIMEX(logger, Logger::DEBUG, "productDefinitionTemplateNumber=" << productDefinitionTemplateNo << "=ensemble derived product, ignoring ensembles");
            } else {
                LOG4FIMEX(logger, Logger::WARN, "numberOfForecastsInEnsemble without perturbationNumber, ignoring ensembles");
            }
        } else {
            MIFI_GRIB_CHECK(gribError_pn, 0);
        }
        break;
    }
    case GRIB_NOT_FOUND: {
        totalNumberOfEnsembles_ = 0;
        perturbationNo_ = 0;
        LOG4FIMEX(logger, Logger::DEBUG, "Checking for ensemblenumber from " << fileURL.c_str() << " in list of " << members.size());
        if (!members.empty()) {
            bool found=false;
            int i = 0;
            for (vector<std::pair<std::string, std::regex>>::const_iterator it = members.begin(); it != members.end(); ++it) {
                if (std::regex_match(fileURL, it->second)) {
                    perturbationNo_=i;
                    totalNumberOfEnsembles_ = members.size();
                    found=true;
                    break;
                }
                ++i;
            }
            if (!found) {
                LOG4FIMEX(logger, Logger::WARN, "perturbationNumber for " << fileURL.c_str() << " not found [" << perturbationNo_ << "," << totalNumberOfEnsembles_ << "]!!!");
            } else {
                LOG4FIMEX(logger, Logger::DEBUG, "perturbationNumber for " << fileURL.c_str() << " is " << perturbationNo_ << " of " << totalNumberOfEnsembles_);
            }
        }
        break;
    }
    default:
        MIFI_GRIB_CHECK(gribError, 0);
        break;
    }
    // cluster (mapped internally like ensembles)
    if (totalNumberOfEnsembles_ == 0) {
        int gribError = grib_get_nocheck(gh, GK_totalNumberOfClusters, totalNumberOfEnsembles_);
        if (gribError == GRIB_SUCCESS) {
            int gribError_cn = grib_get_nocheck(gh, "clusterNumber", perturbationNo_);
            if (gribError_cn == GRIB_NOT_FOUND) {
                perturbationNo_ = 0;
            }
        } else {
            //LOG4FIMEX(logger, Logger::DEBUG, "clusterNumber for " << fileURL.c_str() << " not found [" << perturbationNo_ << "," << totalNumberOfEnsembles_ << "]!!!");
            perturbationNo_ = 0;
            totalNumberOfEnsembles_ = 0;
        }
    }
    // TODO: more definitions, see http://www.ecmwf.int/publications/manuals/grib_api/gribexkeys/ksec2.html
    grib_get(gh, GK_typeOfGrid, typeOfGrid_);
    // =  regular_ll | reduced_ll | mercator | lambert | polar_stereographic | UTM | simple_polyconic | albers |
    //        miller | rotated_ll | stretched_ll | stretched_rotated_ll | regular_gg | rotated_gg | stretched_gg | stretched_rotated_gg |
    //        reduced_gg | sh | rotated_sh | stretched_sh | stretched_rotated_sh | space_view
    if (typeOfGrid_ == "regular_ll") {
        gridDefinition_ = getGridDefRegularLL(edition_, gh);
    } else if (typeOfGrid_ == "lambert") {
        gridDefinition_ = getGridDefLambert(edition_, gh);
    } else if (typeOfGrid_ == "mercator") {
        gridDefinition_ = getGridDefMercator(edition_, gh);
    } else if (typeOfGrid_ == "polar_stereographic") {
        gridDefinition_ = getGridDefPolarStereographic(edition_, gh);
    } else if (typeOfGrid_ == "rotated_ll") {
        gridDefinition_ = getGridDefRotatedLL(edition_, gh);
    } else {
        throw CDMException("unknown gridType: "+ typeOfGrid_);
    }
}

GribFileMessage::GribFileMessage(XMLDoc_p doc, string nsPrefix, xmlNodePtr node)
{
    fileURL_ = getXmlProp(node, "url");
    if (fileURL_.empty()) {
        throw runtime_error("could not find url for node");
    }

    string posStr = getXmlProp(node, "seekPos");
    if (posStr.empty()) {
        throw runtime_error("could not find seekPos for node");
    }
    filePos_ = string2type<off_t>(posStr);

    string msgPosStr = getXmlProp(node, "messagePos");
    if (msgPosStr.empty())
        msgPos_ = 0;
    else
        msgPos_ = string2type<size_t>(msgPosStr);

    {// parameter
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":parameter", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size == 0)
            throw runtime_error("parameter not found in node");
        xmlNodePtr pNode = xp->nodesetval->nodeTab[0];
        parameterName_ = getXmlProp(pNode, "name");
        shortName_ = getXmlProp(pNode, "shortName");
        // grib
        xmlXPathObject_p xpG = doc->getXPathObject(nsPrefix + ":grib1", pNode);
        int gSize = xpG->nodesetval ? xpG->nodesetval->nodeNr : 0;
        if (gSize > 0) {
            edition_ = 1;
            xmlNodePtr gNode = xpG->nodesetval->nodeTab[0];
            gridParameterIds_.push_back(string2type<long>(getXmlProp(gNode, GK_indicatorOfParameter)));
            gridParameterIds_.push_back(string2type<long>(getXmlProp(gNode, GK_gribTablesVersionNo)));
            gridParameterIds_.push_back(string2type<long>(getXmlProp(gNode, GK_identificationOfOriginatingGeneratingCentre)));
        } else {
            xpG = doc->getXPathObject(nsPrefix+":grib2", pNode);
            int gSize = xpG->nodesetval ? xpG->nodesetval->nodeNr : 0;
            if (gSize > 0) {
                edition_ = 2;
                xmlNodePtr gNode = xpG->nodesetval->nodeTab[0];
                gridParameterIds_.push_back(string2type<long>(getXmlProp(gNode, GK_parameterNumber)));
                gridParameterIds_.push_back(string2type<long>(getXmlProp(gNode, GK_parameterCategory)));
                gridParameterIds_.push_back(string2type<long>(getXmlProp(gNode, GK_discipline)));
            } else {
                throw runtime_error("no grib parameters found");
            }
        }
    }
    {
        // level
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":level", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            levelNo_ = string2type<long>(getXmlProp(lNode, "no"));
            levelType_ = string2type<long>(getXmlProp(lNode, "type"));
        }
    }
    {
        // time
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":time", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            dataDate_ = string2type<long>(getXmlProp(lNode, GK_dataDate));
            dataTime_ = string2type<long>(getXmlProp(lNode, "dataTime"));
            stepUnits_ = getXmlProp(lNode, GK_stepUnits);
            timeRangeIndicator_ = string2type<long>(getXmlProp(lNode, GK_timeRangeIndicator));
            typeOfStatisticalProcessing_ = string2type<long>(getXmlProp(lNode, GK_typeOfStatisticalProcessing));
            stepStart_ = string2type<long>(getXmlProp(lNode, "stepStart"));
            stepEnd_ = string2type<long>(getXmlProp(lNode, "stepEnd"));
        }
    }
    {
        // typeOfGrid
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":typeOfGrid", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            typeOfGrid_ = getXmlProp(lNode, "name");
        }
    }
    {
        // ensemble
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":ensemble", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            totalNumberOfEnsembles_ = string2type<long>(getXmlProp(lNode, "total"));
            perturbationNo_ = string2type<long>(getXmlProp(lNode, "no"));
        } else {
            perturbationNo_ = 0;
            totalNumberOfEnsembles_ = 0;
        }
    }
    {
        // extraKeys
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":extraKey", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        for (int i = 0; i < size; ++i) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[i];
            string keyName = getXmlProp(lNode, "name");
            otherKeys_[keyName] = string2type<long>(getXmlProp(lNode, "value"));
        }
    }
    {
        xmlXPathObject_p xp = doc->getXPathObject(nsPrefix + ":gridDefinition", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            string proj4 = getXmlProp(lNode, "proj4");
            bool isDegree = string2type<bool>(getXmlProp(lNode, "isDegree"));
            double startX = string2type<double>(getXmlProp(lNode, "startX"));
            double startY = string2type<double>(getXmlProp(lNode, "startY"));
            double sizeX = string2type<double>(getXmlProp(lNode, "sizeX"));
            double sizeY = string2type<double>(getXmlProp(lNode, "sizeY"));
            double incrX = string2type<double>(getXmlProp(lNode, "incrX"));
            double incrY = string2type<double>(getXmlProp(lNode, "incrY"));
            GridDefinition::Orientation scanMode = static_cast<GridDefinition::Orientation>(string2type<long>(getXmlProp(lNode, "scanMode")));
            gridDefinition_ = GridDefinition(proj4, isDegree, static_cast<size_t>(sizeX), static_cast<size_t>(sizeY), incrX, incrY, startX, startY, scanMode);
        }
    }
}

GribFileMessage::GribFileMessage(xmlTextReaderPtr reader, const std::string& fileName)
{
    while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
        XmlCharPtr name = xmlTextReaderName(reader);
        XmlCharPtr value = xmlTextReaderValue(reader);
        if (name == "url") {
            if (value.len() == 0)
                throw runtime_error("could not find url for node");
            fileURL_ = value.to_string();
        } else if (name == "seekPos") {
            if (value.len() == 0)
                throw runtime_error("could not find seekPos for node");
            filePos_ = value.to_longlong();
        } else if (name == "messagePos") {
            if (value.len() == 0)
                msgPos_ = 0;
            else
                msgPos_ = value.to_longlong();
        }
    }
    // defaults
    perturbationNo_ = 0;
    totalNumberOfEnsembles_ = 0;

    int ret = xmlTextReaderRead(reader);
    while (ret == 1) {
        int type = xmlTextReaderNodeType(reader);
        switch (type) {
        case XML_READER_TYPE_ELEMENT: {
            const xmlChar* name = xmlTextReaderConstName(reader);
            if (name == NULL)
                name = reinterpret_cast<const xmlChar*>("-- NONAME");
            if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("parameter"))) {
                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == "name") {
                        parameterName_ = value.to_string();
                    } else if (name == "shortName") {
                        shortName_ = value.to_string();
                    }
                }
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("grib1"))) {
                // grib1
                edition_ = 1;
                int id = 0;
                int centre = 0;
                int table = 0;

                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == GK_indicatorOfParameter) {
                        id = value.to_long();
                    } else if (name == GK_gribTablesVersionNo) {
                        table = value.to_long();
                    } else if (name == GK_identificationOfOriginatingGeneratingCentre) {
                        centre = value.to_long();
                    }
                }
                gridParameterIds_.push_back(id);
                gridParameterIds_.push_back(table);
                gridParameterIds_.push_back(centre);
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("grib2"))) {
                // grib1
                edition_ = 2;
                int no = 0;
                int cat = 0;
                int dis = 0;

                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == GK_parameterNumber) {
                        no = value.to_long();
                    } else if (name == GK_parameterCategory) {
                        cat = value.to_long();
                    } else if (name == GK_discipline) {
                        dis = value.to_long();
                    }
                }
                gridParameterIds_.push_back(no);
                gridParameterIds_.push_back(cat);
                gridParameterIds_.push_back(dis);
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("level"))) {
                // level

                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == "no") {
                        levelNo_ = value.to_long();
                    } else if (name == "type") {
                        levelType_ = value.to_long();
                    }
                }
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("time"))) {
                // time

                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == GK_dataDate) {
                        dataDate_ = value.to_long();
                    } else if (name == "dataTime") {
                        dataTime_ = value.to_long();
                    } else if (name == GK_stepUnits) {
                        stepUnits_ = value.to_string();
                    } else if (name == GK_stepType) {
                        stepType_ = value.to_string();
                    } else if (name == GK_timeRangeIndicator) {
                        timeRangeIndicator_ = value.to_long();
                    } else if (name == GK_typeOfStatisticalProcessing) {
                        typeOfStatisticalProcessing_ = value.to_long();
                    } else if (name == "stepStart") {
                        stepStart_ = value.to_long();
                    } else if (name == "stepEnd") {
                        stepEnd_ = value.to_long();
                    }
                }
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>(GK_typeOfGrid))) {

                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == "name") {
                        typeOfGrid_ = value.to_string();
                    }
                }
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("ensemble"))) {
                // ensemble

                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == "total") {
                        totalNumberOfEnsembles_ = value.to_long();
                    } else if (name == "no") {
                        perturbationNo_ = value.to_long();
                    }
                }
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("extraKey"))) {
                // extraKeys
                string keyName;
                long keyVal = 0;
                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == "name") {
                        keyName = value.to_string();
                    } else if (name == "value") {
                        keyVal = value.to_long();
                    }
                }
                otherKeys_[keyName] = keyVal;
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gridDefinition"))) {
                // gridDefinition
                string proj4 = "";
                int isDegree = 0;
                double startX = 0;
                double startY = 0;
                size_t sizeX = 0;
                size_t sizeY = 0;
                double incrX = 0;
                double incrY = 0;
                GridDefinition::Orientation scanMode = GridDefinition::LeftLowerHorizontal;
                while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
                    XmlCharPtr name = xmlTextReaderName(reader);
                    XmlCharPtr value = xmlTextReaderValue(reader);
                    if (name == "proj4") {
                        proj4 = value.to_string();
                    } else if (name == "isDegree") {
                        isDegree = value.to_long();
                    } else if (name == "startX") {
                        startX = value.to_float();
                    } else if (name == "startY") {
                        startY = value.to_float();
                    } else if (name == "sizeX") {
                        sizeX = value.to_longlong();
                    } else if (name == "sizeY") {
                        sizeY = value.to_longlong();
                    } else if (name == "incrX") {
                        incrX = value.to_float();
                    } else if (name == "incrY") {
                        incrY = value.to_float();
                    } else if (name == "scanMode") {
                        scanMode = static_cast<GridDefinition::Orientation>(value.to_long());
                    }
                }
                gridDefinition_ = GridDefinition(proj4, isDegree, sizeX, sizeY, incrX, incrY, startX, startY, scanMode);
            } else {
                LOG4FIMEX(logger, Logger::WARN, "unknown node in file :" << fileName << " name: " << name);
            }
            break;
        }
        case XML_READER_TYPE_END_ELEMENT: {
            const xmlChar* name = xmlTextReaderConstName(reader);
            if (name == NULL)
                name = reinterpret_cast<const xmlChar*>("");
            if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribMessage"))) {
                if (gridParameterIds_.size() != 3)
                    throw runtime_error("no grib parameters found in " + fileName);
                if (!isValid())
                    throw runtime_error("unable to parse gribMessage from " + fileName);
                // stop reading and return
                return;
            }
            break;
        }
        default:
            break; // only element nodes
        }
        ret = xmlTextReaderRead(reader);
    }
    throw CDMException("no end node for gribMessage in " + fileName);
}

GribFileMessage::GribFileMessage()
{
}

GribFileMessage::~GribFileMessage()
{
}

static void checkLXML(int status, string msg = "")
{
    if (status < 0)
        throw runtime_error("libxml-error " + msg);
}

static const xmlChar* xmlCast(const std::string& msg)
{
    return reinterpret_cast<const xmlChar*> (msg.c_str());
}

long GribFileMessage::getEdition() const
{
    return edition_;
}

const std::string& GribFileMessage::getFileURL() const
{
    return fileURL_;
}

off_t GribFileMessage::getFilePosition() const
{
    return filePos_;
}

/// messages number within a multi-message
size_t GribFileMessage::getMessageNumber() const
{
    return msgPos_;
}

const std::string& GribFileMessage::getName() const
{
    return parameterName_;
}

const std::string& GribFileMessage::getShortName() const
{
    return shortName_;
}

FimexTime GribFileMessage::getReferenceTime() const
{
    long year = dataDate_ / 10000;
    long month = (dataDate_ - year*10000) / 100;
    long day = dataDate_ % 100;

    long hour = dataTime_ / 100;
    long minutes = dataTime_ % 100;

    return FimexTime(year, month, day, hour, minutes, 0);
}

FimexTime GribFileMessage::getValidTime() const
{
    FimexTime reference = getReferenceTime();
    if (is_invalid_time_point(reference))
        return reference;

    // add step offset
    std::chrono::seconds timeOffset;
    static const std::regex re_stepUnits("(\\d+)?([A-Za-z])");
    std::smatch what;
    if (std::regex_match(stepUnits_, what, re_stepUnits)) {
        int factor = 1;
        if (what[1].matched)
            factor = string2type<int>(what[1].str());
        const std::string& units = what[2].str();
        if (units == "s") {
            timeOffset = std::chrono::seconds(factor*stepEnd_);
        } else if (units == "m") {
            timeOffset = std::chrono::minutes(factor*stepEnd_);
        } else if (units == "h") {
            timeOffset = std::chrono::hours(factor * stepEnd_);
        } else if (units == "D") {
            timeOffset = date::days(factor * stepEnd_);
        } else if (units == "M") {
            timeOffset = date::months(factor * stepEnd_);
        } else if (units == "Y") {
            timeOffset = date::years(factor * stepEnd_);
        } else if (units == "C") {
            timeOffset = date::years(100 * factor * stepEnd_);
        } else {
            throw CDMException("found undefined stepUnits in gribReader: " + stepUnits_ + " (" + units + ")");
        }
    } else {
        throw CDMException("found incomprehensible stepUnits in gribReader: " + stepUnits_);
    }
    return fromTimePoint(asTimePoint(reference) + timeOffset);
}

long GribFileMessage::getTimeRangeIndicator() const
{
    return timeRangeIndicator_;
}

long GribFileMessage::getTypeOfStatisticalProcessing() const
{
    return typeOfStatisticalProcessing_;
}

const std::string& GribFileMessage::getStepType() const
{
    return stepType_;
}

long GribFileMessage::getLevelNumber() const
{
    return levelNo_;
}
long GribFileMessage::getLevelType() const
{
    return levelType_;
}

const std::map<std::string, long>& GribFileMessage::getOtherKeys() const
{
    return otherKeys_;
}

const vector<long>& GribFileMessage::getParameterIds() const
{
    return gridParameterIds_;
}

const std::string& GribFileMessage::getTypeOfGrid() const
{
    return typeOfGrid_;
}

const GridDefinition& GribFileMessage::getGridDefinition() const
{
    return gridDefinition_;
}

string GribFileMessage::toString() const
{
#if defined(LIBXML_WRITER_ENABLED)
    std::shared_ptr<xmlBuffer> buffer(xmlBufferCreate(), xmlBufferFree);
    if (!buffer)
        throw runtime_error("error allocation memory for xmlBuffer");

    {
        std::shared_ptr<xmlTextWriter> writer(xmlNewTextWriterMemory(buffer.get(), 0), xmlFreeTextWriter);

        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("gribMessage")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("url"),
                xmlCast(fileURL_)));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("seekPos"),
                xmlCast(type2string(filePos_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("messagePos"),
                xmlCast(type2string(msgPos_))));
        // parameter
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("parameter")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(),
                xmlCast("shortName"), xmlCast(shortName_)));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(),
                xmlCast("name"), xmlCast(parameterName_)));
        if (edition_ == 1) {
            checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("grib1")));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_indicatorOfParameter), xmlCast(type2string(gridParameterIds_.at(0)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_gribTablesVersionNo), xmlCast(type2string(gridParameterIds_.at(1)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_identificationOfOriginatingGeneratingCentre),
                                                  xmlCast(type2string(gridParameterIds_.at(2)))));
            checkLXML(xmlTextWriterEndElement(writer.get()));
        } else if (edition_ == 2) {
            checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("grib2")));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_parameterNumber), xmlCast(type2string(gridParameterIds_.at(0)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_parameterCategory), xmlCast(type2string(gridParameterIds_.at(1)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_discipline), xmlCast(type2string(gridParameterIds_.at(2)))));
            checkLXML(xmlTextWriterEndElement(writer.get()));
        } else {
            throw runtime_error("unknown gribEdition: " + type2string(edition_));
        }
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // level
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("level")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("type"), xmlCast(
                type2string(levelType_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("no"), xmlCast(
                type2string(levelNo_))));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // ensemble
        if (totalNumberOfEnsembles_ > 0) {
            checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("ensemble")));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("no"), xmlCast(
                    type2string(perturbationNo_))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("total"), xmlCast(
                    type2string(totalNumberOfEnsembles_))));
            checkLXML(xmlTextWriterEndElement(writer.get()));
        }

        // otherKeys / extraKey
        for (std::map<std::string, long>::const_iterator others =
                otherKeys_.begin(); others != otherKeys_.end(); ++others) {
            checkLXML(
                    xmlTextWriterStartElement(writer.get(),
                            xmlCast("extraKey")));
            checkLXML(
                    xmlTextWriterWriteAttribute(writer.get(), xmlCast("name"),
                            xmlCast(others->first)));
            checkLXML(
                    xmlTextWriterWriteAttribute(writer.get(), xmlCast("value"),
                            xmlCast(type2string(others->second))));
            checkLXML(xmlTextWriterEndElement(writer.get()));
        }

        // time
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("time")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_dataDate), xmlCast(type2string(dataDate_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("dataTime"),
                xmlCast(type2string(dataTime_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_stepUnits), xmlCast(stepUnits_)));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_stepType), xmlCast(stepType_)));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_timeRangeIndicator), xmlCast(type2string(timeRangeIndicator_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(GK_typeOfStatisticalProcessing), xmlCast(type2string(typeOfStatisticalProcessing_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("stepStart"),
                xmlCast(type2string(stepStart_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("stepEnd"),
                xmlCast(type2string(stepEnd_))));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // typeOfGrid
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast(GK_typeOfGrid)));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("name"),
                xmlCast(typeOfGrid_)));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // gridDefinition
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("gridDefinition")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("proj4"),
                        xmlCast(gridDefinition_.getProjDefinition())));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("isDegree"),
                        xmlCast(type2string(gridDefinition_.isDegree()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("startX"),
                        xmlCast(type2string(gridDefinition_.getXStart()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("startY"),
                        xmlCast(type2string(gridDefinition_.getYStart()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("incrX"),
                        xmlCast(type2string(gridDefinition_.getXIncrement()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("incrY"),
                        xmlCast(type2string(gridDefinition_.getYIncrement()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("sizeX"),
                        xmlCast(type2string(gridDefinition_.getXSize()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("sizeY"),
                        xmlCast(type2string(gridDefinition_.getYSize()))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("scanMode"),
                        xmlCast(type2string(gridDefinition_.getScanMode()))));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // end the message
        checkLXML(xmlTextWriterEndElement(writer.get()));
    }
#else
    throw runtime_error("libxml_writer not enabled");
#endif

    return string(reinterpret_cast<const char*> (buffer->content));
}

grib_handle_p GribFileMessage::createGribHandle(bool asimofHeader) const
{
    const string url = getFileURL().substr(5); // remove 'file:' prefix, needs to be improved when streams are allowed
    const size_t position = asimofHeader ? 0 : getFilePosition();
    FILE_p fh = file_open_seek(url, position);

    // enable multi-messages
    grib_multi_support_on(0);

    int err = 0;
    const size_t message = asimofHeader ? 0 : getMessageNumber();
    for (size_t i = 0; i < message; i++) {
        // forward to correct multimessage
        grib_handle_p gh = make_grib_handle(fh, err);
    }

    // read the message of interest
    grib_handle_p gh = make_grib_handle(fh, err);
    if (!gh)
        throw CDMException("cannot find grib-handle at file: " + url + " pos: " + type2string(position) + " msg: " + type2string(message) +
                           " asimof: " + type2string(asimofHeader));

    if (err != GRIB_SUCCESS)
        GRIB_CHECK(err, 0);

    return gh;
}

size_t GribFileMessage::readData(double* data, size_t data_size, double missingValue) const
{
    if (!isValid())
        return 0;

    grib_handle_p gh = createGribHandle(false);

    LOG4FIMEX(logger, Logger::DEBUG, "set missing = " << missingValue);
    MIFI_GRIB_CHECK(grib_set_double(gh.get(), "missingValue", missingValue), 0);
    LOG4FIMEX(logger, Logger::DEBUG, "retrieve values");
    MIFI_GRIB_CHECK(grib_get_double_array(gh.get(), "values", &data[0], &data_size), 0);

    return data_size;
}

size_t GribFileMessage::readLevelData(std::vector<double>& levelData, double missingValue, bool asimofHeader) const
{
    if (!isValid())
        return 0;

    grib_handle_p gh = createGribHandle(asimofHeader);

    size_t size = 0;
    long pvpresent = 0;
    grib_get(gh, "PVPresent", pvpresent);
    if (pvpresent) {
        grib_get(gh, "pv", size);
        levelData.resize(size);
        MIFI_GRIB_CHECK(grib_get_double_array(gh.get(), "pv", &levelData[0], &size), 0);
        double inputMissing;
        grib_get(gh, "missingValue", inputMissing);
        if (inputMissing != missingValue) {
            transform(&levelData[0], &levelData[0] + size, &levelData[0], ChangeMissingValue<double, double>(inputMissing, missingValue));
        }
    }
    return size;
}

GribFileIndex::GribFileIndex(const std::string& gribFilePath, const std::vector<std::pair<std::string, std::regex>>& members,
                             std::map<std::string, std::string> options)
    : options_(options)
{
    init(gribFilePath, "", members);
}

GribFileIndex::GribFileIndex(const std::string& gribFilePath, const std::string& grbmlFilePath, const std::vector<std::pair<std::string, std::regex>>& members,
                             std::map<std::string, std::string> options)
    : options_(options)
{
    init(gribFilePath, grbmlFilePath, members);
}

GribFileIndex::GribFileIndex(const std::string& grbmlFilePath)
{
    if (!initByXMLReader(grbmlFilePath))
        throw runtime_error("error reading grbml-file: '" + grbmlFilePath + "'");
}


struct HasSameUrl {
    std::string file_;
    HasSameUrl(string filename) : file_(filename) {}
    bool operator()(GribFileMessage& gfm) const {return gfm.getFileURL() == file_;}
};

void GribFileIndex::init(const std::string& gribFilePath, const std::string& grbmlFilePath, const std::vector<std::pair<std::string, std::regex>>& members)
{
    if (!grbmlFilePath.empty()) {
        // append to existing grbml-file
        initByXMLReader(grbmlFilePath);
        // but remove existing messages for the same file
        messages_.erase(std::remove_if(messages_.begin(), messages_.end(), HasSameUrl("file:" + gribFilePath)), messages_.end());
    }
    std::map<std::string, std::string>::const_iterator efIt = options_.find("earthfigure");
    if (efIt != options_.end()) {
        earthFigure_ = efIt->second;
        LOG4FIMEX(logger, Logger::DEBUG, "using earthfigure '" << earthFigure_ << "'");
    }
    vector<string> extraKeys;
    std::map<std::string, std::string>::const_iterator ekIt = options_.find("extraKeys");
    if (ekIt != options_.end()) {
        LOG4FIMEX(logger, Logger::DEBUG, "using extraKeys '" << ekIt->second << "'");
        extraKeys = tokenize(ekIt->second,",");
    }

    initByGrib(gribFilePath, members, extraKeys);
    earthFigure_ = ""; // remember to reset!
}

void GribFileIndex::initByGrib(const std::string& gribFilePath, const std::vector<std::pair<std::string, std::regex>>& members,
                               const std::vector<std::string>& extraKeys)
{
    url_ = "file:" + gribFilePath;
    std::shared_ptr<FILE> fh = file_open_seek(gribFilePath, 0);
    // enable multi-messages
    grib_multi_support_on(0);
    off_t lastPos = static_cast<size_t>(-1);
    size_t msgPos = 0;
    while (!feof(fh.get())) {
        // read the next message
        off_t pos = ftello(fh.get());
        int err = 0;
        grib_handle_p gh = make_grib_handle(fh, err);
        off_t newPos = ftello(fh.get());
        if (gh) {
            MIFI_GRIB_CHECK(err, 0);
            if (newPos != pos) {
                // new message
                lastPos = pos;
                msgPos = 0;
            } else {
                // new part of multi-message
                msgPos++;
                // don't change lastPos
            }
            try {
                messages_.push_back(GribFileMessage(gh, url_, lastPos, msgPos, members, extraKeys));
            } catch (CDMException& ex) {
                LOG4FIMEX(logger, Logger::WARN, "ignoring grib-message at byte " << msgPos <<": " << ex.what());
            }
        }
    }
}

#if 0
// useful for debugging
static void processNode(xmlTextReaderPtr reader) {
    const xmlChar *name, *value;

    name = xmlTextReaderConstName(reader);
    if (name == NULL)
    name = BAD_CAST "--";

    value = xmlTextReaderConstValue(reader);

    printf("%d %d %s %d %d",
        xmlTextReaderDepth(reader),
        xmlTextReaderNodeType(reader),
        name,
        xmlTextReaderIsEmptyElement(reader),
        xmlTextReaderHasValue(reader));
    if (value == NULL)
    printf("\n");
    else {
        if (value.len() > 40)
            printf(" %.40s...\n", value);
        else
        printf(" %s\n", value);
    }
}
#endif

bool GribFileIndex::initByXMLReader(const std::string& grbmlFilePath)
{
    LOG4FIMEX(logger, Logger::DEBUG, "reading GribFile-index :" << grbmlFilePath);
    xmlTextReaderPtr reader = xmlReaderForFile(grbmlFilePath.c_str(), NULL, 0);
    if (!reader)
        return false;

    std::shared_ptr<xmlTextReader> cleanupReader(reader, xmlFreeTextReader);
    const xmlChar* name;
    int ret = xmlTextReaderRead(reader);
    while (ret == 1) {
        // int depth = xmlTextReaderDepth(reader);
        int type = xmlTextReaderNodeType(reader);
        switch (type) {
        case XML_READER_TYPE_ELEMENT: {
            name = xmlTextReaderConstName(reader);
            if (name == NULL)
                name = reinterpret_cast<const xmlChar*>("");
            if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribFileIndex"))) {
                XmlCharPtr url = xmlTextReaderGetAttribute(reader, reinterpret_cast<const xmlChar*>("url"));
                url_ = url.to_string();
            } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribMessage"))) {
                messages_.push_back(GribFileMessage(reader, grbmlFilePath));
            } else {
                LOG4FIMEX(logger, Logger::WARN, "unknown node in file :" << grbmlFilePath << " name: " << name);
            }
            break;
        }
        case XML_READER_TYPE_END_ELEMENT: {
            name = xmlTextReaderConstName(reader);
            if (name == NULL)
                name = reinterpret_cast<const xmlChar*>("");
            if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribFileIndex"))) {
                return true; // finished
            }
            break;
        }
        default:
            break; // only element nodes of interest
        }
        ret = xmlTextReaderRead(reader);
    }
    if (ret != 0) {
        LOG4FIMEX(logger, Logger::ERROR, "failed to parse '" << grbmlFilePath << "'");
        return false;
    } else {
        return true;
    }
}

void GribFileIndex::initByXML(const std::string& grbmlFilePath)
{
    LOG4FIMEX(logger, Logger::DEBUG, "reading GribFile-index :" << grbmlFilePath);
    initByXMLReader(grbmlFilePath);
    XMLDoc_p doc = std::make_shared<XMLDoc>(grbmlFilePath);
    doc->registerNamespace("gfi", "http://www.met.no/schema/fimex/gribFileIndex");
    xmlXPathObject_p xp = doc->getXPathObject("/gfi:gribFileIndex");
    int size = (xp->nodesetval) ? xp->nodesetval->nodeNr : 0;
    if (size == 0) {
        throw runtime_error("grib-index xmlfile does not contain root node at: " + grbmlFilePath);
    }
    url_ = getXmlProp(xp->nodesetval->nodeTab[0], "url");

    xp = doc->getXPathObject("gfi:gribMessage", xp->nodesetval->nodeTab[0]);
    size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
    for (int i = 0; i < size; ++i) {
        messages_.push_back(GribFileMessage(doc, "gfi", xp->nodesetval->nodeTab[i]));
    }
}

GribFileIndex::~GribFileIndex()
{
}

std::ostream& operator<<( std::ostream& os, const GribFileMessage& gfm)
{
    os << gfm.toString();
    return os;
}

/// outputstream for a GribFileIndex
std::ostream& operator<<( std::ostream& os, const GribFileIndex& gfm)
{
    os << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
    os << "<gribFileIndex url=\""<< gfm.getUrl() << "\" xmlns=\"http://www.met.no/schema/fimex/gribFileIndex\">" << endl;

    const vector<GribFileMessage>& messages = gfm.listMessages();
    for (vector<GribFileMessage>::const_iterator it = messages.begin(); it != messages.end(); ++it) {
        os << *it;
    }
    os << "</gribFileIndex>"<< endl;
    return os;
}

} // namespace MetNoFimex
