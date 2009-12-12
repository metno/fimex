/*
 * Fimex, GribFileIndex.cc
 *
 * (C) Copyright 2009, met.no
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

#include "fimex/config.h"
#ifdef HAVE_GRIBAPI_H
#include "fimex/GribFileIndex.h"
#include "fimex/Utils.h"
#include "fimex/GribUtils.h"
#include "grib_api.h"
#include "proj_api.h"
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem/operations.hpp>
#include <libxml/xmlwriter.h>
#include <cstdlib>
#include <iostream>
#include <cstdio>
#include <libxml/tree.h>
#include <libxml/xpath.h>


namespace MetNoFimex
{
using namespace std;

/**
 * converts a point on earth to a projection plane
 * @param projStr projection definition for proj4
 * @param lon longitude in degree
 * @param lat latitutde in degree
 * @param x output of proj (usually m, but dependent on projStr)
 * @param y output of proj
 * @throws runtime_error on proj-failure
 */
static void projConvert(const std::string& projStr, double lon, double lat, double& x, double& y)
{
    projPJ outputPJ;
    if ( !(outputPJ = pj_init_plus(projStr.c_str())) ) {
        std::string errorMsg(pj_strerrno(pj_errno));
        throw std::runtime_error("Proj error: " + errorMsg);
    }

    projUV uv;
    uv.u = lon * DEG_TO_RAD;
    uv.v = lat * DEG_TO_RAD;

    uv = pj_fwd(uv, outputPJ);
    pj_free(outputPJ);

    if (uv.u == HUGE_VAL) {
        std::ostringstream errMsg;
        errMsg <<  "projection fails:" << projStr << " (lon,lat)=(" << lon << "," << lat << ")";
        throw std::runtime_error(errMsg.str());
    }
    x = uv.u;
    y = uv.v;
}


std::string getEarthsOblateFigure(boost::shared_ptr<grib_handle> gh, long factorToM)
{
    long majorFactor, minorFactor;
    double majorValue, minorValue;
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "scaleFactorOfMajorAxisOfOblateSpheroidEarth", &majorFactor), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "scaledValueOfMajorAxisOfOblateSpheroidEarth", &majorValue), 0);
    while (majorFactor > 0) {majorValue *= 10; majorFactor--;}
    while (majorFactor < 0) {majorValue /= 10; majorFactor++;}
    // transfer km to m
    majorFactor *= factorToM;

    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "scaleFactorOfMinorAxisOfOblateSpheroidEarth", &minorFactor), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "scaledValueOfMinorAxisOfOblateSpheroidEarth", &minorValue), 0);
    while (minorFactor > 0) {minorValue *= 10; minorFactor--;}
    while (minorFactor < 0) {minorValue /= 10; minorFactor++;}
    // transfer (km|m) to m
    minorFactor *= factorToM;

    return "+a=" + type2string(majorValue) + " +b=" + type2string(minorValue);
}

std::string getEarthsSphericalFigure(boost::shared_ptr<grib_handle> gh)
{
    double radius;
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "radiusInMetres", &radius), 0);
    return "+a=" + type2string(radius) + " +e=0";
}

std::string getEarthsFigure(long edition, boost::shared_ptr<grib_handle> gh)
{
    // earth specific parameters, depending on grib-edition
    string earth;
    if (edition == 1) {
        long earthIsOblate;
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "earthIsOblate", &earthIsOblate), 0);
        if (earthIsOblate == 0) {
            earth = "+a=6367470 +e=0"; // sphere
        } else {
            earth = "+a=6378160 +b=6356775"; // oblate, maybe more or better defined in grib2???
        }
    } else {
        long shapeOfTheEarth;
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "shapeOfTheEarth", &shapeOfTheEarth), 0);
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

GridDefinition getGridDefRegularLL(long edition, boost::shared_ptr<grib_handle> gh)
{
    long sizeX, sizeY, ijDirectionIncrementGiven;
    double startX, startY, incrX, incrY;
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "Ni", &sizeX), 0);
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "Nj", &sizeY), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "longitudeOfFirstGridPointInDegrees", &startX), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeOfFirstGridPointInDegrees", &startY), 0);

    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "ijDirectionIncrementGiven", &ijDirectionIncrementGiven), 0);
    if (ijDirectionIncrementGiven == 0) {
        double endX, endY;
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "longitudeOfLastGridPointInDegrees", &endX), 0);
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeOfLastGridPointInDegrees", &endY), 0);
        incrX = (endX - startX) / sizeX;
        incrY = (endY - startY) / sizeY;
    } else {
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "iDirectionIncrementInDegrees", &incrX), 0);
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "jDirectionIncrementInDegrees", &incrY), 0);
    }

    string proj = "+proj=longlat " + getEarthsFigure(edition, gh);

    return GridDefinition(proj, sizeX, sizeY, incrX, incrY, startX, startY, gribGetGridOrientation(gh));
}
GridDefinition getGridDefRotatedLL(long edition, boost::shared_ptr<grib_handle> gh)
{
    long sizeX, sizeY, ijDirectionIncrementGiven;
    double startX, startY, incrX, incrY, latRot, lonRot;
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "Ni", &sizeX), 0);
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "Nj", &sizeY), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "longitudeOfFirstGridPointInDegrees", &startX), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeOfFirstGridPointInDegrees", &startY), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "longitudeOfSouthernPoleInDegrees", &lonRot), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeOfSouthernPoleInDegrees", &latRot), 0);

    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "ijDirectionIncrementGiven", &ijDirectionIncrementGiven), 0);
    if (ijDirectionIncrementGiven == 0) {
        double endX, endY;
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "longitudeOfLastGridPointInDegrees", &endX), 0);
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeOfLastGridPointInDegrees", &endY), 0);
        incrX = (endX - startX) / sizeX;
        incrY = (endY - startY) / sizeY;
    } else {
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "iDirectionIncrementInDegrees", &incrX), 0);
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "jDirectionIncrementInDegrees", &incrY), 0);
    }

    // TODO: test, might be rotation by lat/long = 180degree
    ostringstream oss;
    oss << "+proj=ob_tran +o_proj=longlat +lon_0=" << (lonRot) << " +o_lat_p=" << (-1 * latRot);
    oss << " " + getEarthsFigure(edition, gh);
    string proj =  oss.str();

    return GridDefinition(proj, sizeX, sizeY, incrX, incrY, startX, startY, gribGetGridOrientation(gh));
}
GridDefinition getGridDefPolarStereographic(long edition, boost::shared_ptr<grib_handle> gh)
{
    long sizeX, sizeY;
    double startX, startY, incrX, incrY, startLon, startLat;
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "Ni", &sizeX), 0);
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "Nj", &sizeY), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "longitudeOfFirstGridPointInDegrees", &startLon), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeOfFirstGridPointInDegrees", &startLat), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "xDirectionGridLengthInMetres", &incrX), 0);
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "yDirectionGridLengthInMetres", &incrY), 0);


    long projectionCentreFlag;
    double orientationOfGrid, lat0, lat_ts;
    string earth;
    MIFI_GRIB_CHECK(grib_get_double(gh.get(), "orientationOfTheGridInDegrees", &orientationOfGrid), 0);
    if (edition == 1) {
        // MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", &lat_ts), 0); // defined in grib-api > 1.6 to 60
        lat_ts = 60.;
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "projectionCenterFlag", &projectionCentreFlag), 0); // changed to centre in grib-api > 1.8
    } else {
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "latitudeWhereDxAndDyAreSpecifiedInDegrees", &lat_ts), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "projectionCentreFlag", &projectionCentreFlag), 0);
    }
    if (projectionCentreFlag == 0) {
        lat0 = 90.; // northpole
    } else {
        lat0 = -90.; // southpole
    }
    ostringstream oss;
    oss << "+proj=stere +lat_0="<<lat0 << " +lon_0="<< orientationOfGrid << " +lat_ts=" << lat_ts << " ";
    oss << getEarthsFigure(edition, gh);
    string proj = oss.str();

    // calculate startX and startY from lat/lon
    projConvert(proj, startLon, startLat, startX, startY);

    return GridDefinition(proj, sizeX, sizeY, incrX, incrY, startX, startY, gribGetGridOrientation(gh));
}

GribFileMessage::GribFileMessage(boost::shared_ptr<grib_handle> gh, const std::string& fileURL, long filePos, long msgPos)
: fileURL_(fileURL), filePos_(filePos), msgPos_(msgPos)
{
    if (gh.get() == 0)
        throw runtime_error("GribFileMessage initialized with NULL-ptr");

    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "edition", &edition_), 0);

    char msg[1024];
    size_t msgLength = 1024;

    if (edition_ == 1) {
        gridParmeterIds_ = vector<long> (3, 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "indicatorOfParameter", &gridParmeterIds_[0]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "gribTablesVersionNo", &gridParmeterIds_[1]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "centre", &gridParmeterIds_[2]), 0);
    } else if (edition_ == 2) {
        gridParmeterIds_ = vector<long> (3, 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "parameterNumber", &gridParmeterIds_[0]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "parameterCategory", &gridParmeterIds_[1]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "discipline", &gridParmeterIds_[2]), 0);
    } else {
        throw runtime_error("unknown grib version: " + type2string(edition_));
    }

    int nameError = grib_get_string(gh.get(), "name", msg, &msgLength);
    if ((nameError != GRIB_NOT_FOUND) && (string("unknown") != string(msg))) {
        MIFI_GRIB_CHECK(nameError, 0);
        parameterName_ = msg;
    } else {
        parameterName_ = join(gridParmeterIds_.begin(), gridParmeterIds_.end(), ",");
    }
    int shortNameError = grib_get_string(gh.get(), "shortName", msg, &msgLength);
    if ((shortNameError != GRIB_NOT_FOUND) && (string("unknown") != string(msg))) {
        MIFI_GRIB_CHECK(shortNameError, 0);
        shortName_ = msg;
    } else {
        shortName_ = join(gridParmeterIds_.begin(), gridParmeterIds_.end(), "_");
    }


    // level
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "levtype", &levelType_), 0);
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "level", &levelNo_), 0);
    // time
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "dataDate", &dataDate_), 0);
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "time", &dataTime_), 0);

    // TODO: more definitions, see http://www.ecmwf.int/publications/manuals/grib_api/gribexkeys/ksec2.html
    msgLength = 1024;
    MIFI_GRIB_CHECK(grib_get_string(gh.get(), "typeOfGrid", msg, &msgLength), 0);
    typeOfGrid_ = msg;
    // =  regular_ll | reduced_ll | mercator | lambert | polar_stereographic | UTM | simple_polyconic | albers |
    //        miller | rotated_ll | stretched_ll | stretched_rotated_ll | regular_gg | rotated_gg | stretched_gg | stretched_rotated_gg |
    //        reduced_gg | sh | rotated_sh | stretched_sh | stretched_rotated_sh | space_view
    if (typeOfGrid_ == "regular_ll") {
        gridDefinition_ = getGridDefRegularLL(edition_, gh);
    } else if (typeOfGrid_ == "polar_stereographic") {
        gridDefinition_ = getGridDefPolarStereographic(edition_, gh);
    } else if (typeOfGrid_ == "rotated_ll") {
        gridDefinition_ = getGridDefRotatedLL(edition_, gh); // TODO, switch to rotated!!!
    } else {
        throw CDMException("unknown gridType: "+ typeOfGrid_);
    }

}

GribFileMessage::GribFileMessage(boost::shared_ptr<XMLDoc> doc, string nsPrefix, xmlNodePtr node)
{
    fileURL_ = getXmlProp(node, "url");
    if (fileURL_.size() == 0) {
        throw runtime_error("could not find url for node");
    }
    string posStr = getXmlProp(node, "seekPos");
    if (posStr.size() == 0) {
        throw runtime_error("could not find seekPos for node");
    }
    filePos_ = string2type<size_t>(posStr);
    string msgPosStr = getXmlProp(node, "messagePos");
    if (msgPosStr.size() == 0) msgPos_ = 0;
    else msgPos_ = string2type<size_t>(msgPosStr);


    {// parameter
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":parameter", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size == 0) throw runtime_error("parameter not found in node");
        xmlNodePtr pNode = xp->nodesetval->nodeTab[0];
        parameterName_ = getXmlProp(pNode, "name");
        shortName_ = getXmlProp(pNode, "shortName");
        // grib
        XPathObjPtr xpG = doc->getXPathObject(nsPrefix+":grib1", pNode);
        int gSize = xpG->nodesetval ? xpG->nodesetval->nodeNr : 0;
        if (gSize > 0) {
            edition_ = 1;
            xmlNodePtr gNode = xpG->nodesetval->nodeTab[0];
            gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "indicatorOfParameter")));
            gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "gribTablesVersionNo")));
            gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "identificationOfOriginatingGeneratingCentre")));
        } else {
            xpG = doc->getXPathObject(nsPrefix+":grib2", pNode);
            int gSize = xpG->nodesetval ? xpG->nodesetval->nodeNr : 0;
            if (gSize > 0) {
                edition_ = 2;
                xmlNodePtr gNode = xpG->nodesetval->nodeTab[0];
                gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "parameterNumber")));
                gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "parameterCategory")));
                gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "discipline")));
            } else {
                throw runtime_error("no grib parameters found");
            }
        }
    }
    {
        // level
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":level", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            levelNo_ = string2type<long>(getXmlProp(lNode, "no"));
            levelType_ = string2type<long>(getXmlProp(lNode, "type"));
        }
    }
    {
        // time
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":time", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            dataDate_ = string2type<long>(getXmlProp(lNode, "dataDate"));
            dataTime_ = string2type<long>(getXmlProp(lNode, "dataTime"));
        }
    }
    {
        // typeOfGrid
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":typeOfGrid", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            typeOfGrid_ = getXmlProp(lNode, "name");
        }
    }

    {
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":gridDefinition", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            string proj4 = getXmlProp(lNode, "proj4");
            long startX = string2type<long>(getXmlProp(lNode, "startX"));
            long startY = string2type<long>(getXmlProp(lNode, "startY"));
            long sizeX = string2type<long>(getXmlProp(lNode, "sizeX"));
            long sizeY = string2type<long>(getXmlProp(lNode, "sizeY"));
            long incrX = string2type<long>(getXmlProp(lNode, "incrX"));
            long incrY = string2type<long>(getXmlProp(lNode, "incrY"));
            GridDefinition::Orientation scanMode = static_cast<GridDefinition::Orientation>(string2type<long>(getXmlProp(lNode, "scanMode")));
            gridDefinition_ = GridDefinition(proj4, sizeX, sizeY, incrX, incrY, startX, startY, scanMode);
        }
    }
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

const long GribFileMessage::getEdition() const
{
    return edition_;
}


const std::string& GribFileMessage::getFileURL() const
{
    return fileURL_;
}
const size_t GribFileMessage::getFilePosition() const
{
    return filePos_;
}
/// messages number within a multi-message
const size_t GribFileMessage::getMessageNumber() const
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
boost::posix_time::ptime GribFileMessage::getDateTime() const
{
    long year = dataDate_ / 10000;
    long month = (dataDate_ - year*10000) / 100;
    long day = dataDate_ % 100;

    long hour = dataTime_ / 100;
    long minutes = dataTime_ % 100;

    boost::gregorian::date date(year, month, day);
    boost::posix_time::time_duration clock(hour, minutes, 0);
    return boost::posix_time::ptime(date, clock);
}
long GribFileMessage::getLevelNumber() const
{
    return levelNo_;
}
long GribFileMessage::getLevelType() const
{
    return levelType_;
}

const vector<long>& GribFileMessage::getParameterIds() const
{
    return gridParmeterIds_;
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
    boost::shared_ptr<xmlBuffer> buffer(xmlBufferCreate(), xmlBufferFree);
    if (buffer.get() == 0)
        throw runtime_error("error allocation memory for xmlBuffer");

    {
        boost::shared_ptr<xmlTextWriter> writer(xmlNewTextWriterMemory(buffer.get(), 0),
                xmlFreeTextWriter);

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
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "indicatorOfParameter"), xmlCast(type2string(
                    gridParmeterIds_.at(0)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "gribTablesVersionNo"), xmlCast(type2string(
                    gridParmeterIds_.at(1)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "identificationOfOriginatingGeneratingCentre"), xmlCast(
                    type2string(gridParmeterIds_.at(2)))));
            checkLXML(xmlTextWriterEndElement(writer.get()));
        } else if (edition_ == 2) {
            checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("grib2")));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "parameterNumber"), xmlCast(type2string(
                    gridParmeterIds_.at(0)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "parameterCategory"), xmlCast(type2string(
                    gridParmeterIds_.at(1)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(),
                    xmlCast("discipline"), xmlCast(type2string(
                            gridParmeterIds_.at(2)))));
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

        // time
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("time")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("dataDate"),
                xmlCast(type2string(dataDate_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("dataTime"),
                xmlCast(type2string(dataTime_))));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // typeOfGrid
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("typeOfGrid")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("name"),
                xmlCast(typeOfGrid_)));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // gridDefinition
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("gridDefinition")));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("proj4"),
                        xmlCast(gridDefinition_.getProjDefinition())));
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

size_t gribDataRead(const GribFileMessage& gfm, std::vector<double>& data, double missingValue)
{
    if (!gfm.isValid()) return 0;
    string url = gfm.getFileURL();
    // remove the 'file:' prefix, needs to be improved when streams are allowed
    url = url.substr(5);
    boost::shared_ptr<FILE> fh(fopen(url.c_str(), "r"), fclose);
    if (fh.get() == 0) {
        throw runtime_error("cannot open file: " + gfm.getFileURL());
    }
    fseek(fh.get(), gfm.getFilePosition(), SEEK_SET);

    // enable multi-messages
    grib_multi_support_on(0);

    int err = 0;
    for (size_t i = 0; i < gfm.getMessageNumber(); i++) {
        // forward to correct multimessage
        boost::shared_ptr<grib_handle> gh(grib_handle_new_from_file(0, fh.get(), &err), grib_handle_delete);
    }
    // read the message of interest
    boost::shared_ptr<grib_handle> gh(grib_handle_new_from_file(0, fh.get(), &err), grib_handle_delete);
    size_t size = 0;
    if (gh.get() != 0) {
        if (err != GRIB_SUCCESS) GRIB_CHECK(err,0);
        MIFI_GRIB_CHECK(grib_get_size(gh.get(), "values", &size), 0);
        if (size > data.size()) size = data.size();
        MIFI_GRIB_CHECK(grib_get_double_array(gh.get(), "values", &data[0], &size), 0);
        double inputMissing;
        MIFI_GRIB_CHECK(grib_get_double(gh.get(), "missingValue", &inputMissing), 0);
        if (inputMissing != missingValue) {
            transform(&data[0], &data[size], &data[0], ChangeMissingValue<double, double>(inputMissing, missingValue));
        }

    } else {
        throw CDMException("cannot find grib-handle at file: " + url + " pos: " + type2string(gfm.getFilePosition()) + " msg: " + type2string(gfm.getMessageNumber()));
    }
    return size;
}


GribFileIndex::GribFileIndex()
{
    // dummy generator
}

GribFileIndex::GribFileIndex(boost::filesystem::path gribFilePath, bool ignoreExistingXml)
{
    namespace fs = boost::filesystem;
    if (!fs::exists(gribFilePath) || ! fs::is_regular(gribFilePath)) {
        throw runtime_error("no such file: " + gribFilePath.string());
    }
    if (ignoreExistingXml) {
        initByGrib(gribFilePath);
    } else {
        // find gribml-file younger than original file
        std::string filename = gribFilePath.leaf();
        fs::path xmlDir = gribFilePath.branch_path();
        fs::path xmlFile = xmlDir / (filename + ".grbml");
        if (fs::exists(xmlFile) && (fs::last_write_time(xmlFile) >= fs::last_write_time(gribFilePath))) {
            initByXML(xmlFile);
        } else {
            // try environment path: GRIB_INDEX_PATH
            char* indexDir = getenv("GRIB_INDEX_PATH");
            if (indexDir != 0) {
                xmlFile = fs::path(indexDir) / (filename + ".grbml");
                if (fs::exists(xmlFile) && (fs::last_write_time(xmlFile) >= fs::last_write_time(gribFilePath))) {
                    initByXML(xmlFile);
                } else {
                    initByGrib(gribFilePath);
                }
            } else {
                initByGrib(gribFilePath);
            }
        }
    }
}

void GribFileIndex::initByGrib(boost::filesystem::path gribFilePath)
{
    url_ = "file:"+ gribFilePath.file_string();
    boost::shared_ptr<FILE> fh(fopen(gribFilePath.file_string().c_str(), "r"), fclose);
    if (fh.get() == 0) {
        throw runtime_error("cannot open file: " + gribFilePath.file_string());
    }
    // enable multi-messages
    grib_multi_support_on(0);
    size_t lastPos = -1;
    size_t msgPos = 0;
    while (!feof(fh.get())) {
        // read the next message
        size_t pos = ftell(fh.get());
        int err = 0;
        boost::shared_ptr<grib_handle> gh(grib_handle_new_from_file(0, fh.get(), &err), grib_handle_delete);
        size_t newPos = ftell(fh.get());
        if (gh.get() != 0) {
            if (err != GRIB_SUCCESS) GRIB_CHECK(err,0);
            if (newPos != pos) {
                // new message
                lastPos = pos;
                msgPos = 0;
            } else {
                // new part of multi-message
                msgPos++;
                // don't change lastPos
            }
            messages_.push_back(GribFileMessage(gh, url_, lastPos, msgPos));
        }
    }

}

void GribFileIndex::initByXML(boost::filesystem::path xmlFilePath)
{
    boost::shared_ptr<XMLDoc> doc(new XMLDoc(xmlFilePath.file_string()));
    doc->registerNamespace("gfi", "http://www.met.no/schema/fimex/gribFileIndex");
    XPathObjPtr xp = doc->getXPathObject("/gfi:gribFileIndex");
    int size = (xp->nodesetval) ? xp->nodesetval->nodeNr : 0;
    if (size == 0) {
        throw runtime_error("grib-index xmlfile does not contain root node at: " + xmlFilePath.file_string());
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



}

#endif /* HAVE_GRIB_API */
