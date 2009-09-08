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
#include "grib_api.h"
#include <stdexcept>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/filesystem/operations.hpp>
#include <libxml/xmlwriter.h>
#include <cstdlib>
#include <iostream>
#include <cstdio>


namespace MetNoFimex
{
#define MIFI_GRIB_CHECK(error, msg) mifi_grib_check(error, msg, __LINE__, __FILE__);
void mifi_grib_check(int error, const char* msg, int line, const char* file) throw(std::runtime_error)
{
    if (error) {
        const char* errMsg = grib_get_error_message(error);
        std::ostringstream oss;
        oss << "gribError occured in " << file << " at line "<< line;
        oss << " : " << errMsg;
        if (msg != 0) {
            oss << "; " << msg;
        }
        throw std::runtime_error(oss.str());
    }
}
using namespace std;

GribFileMessage::GribFileMessage(boost::shared_ptr<grib_handle> gh,
        long filePos, long msgPos) :
    filePos_(filePos), msgPos_(msgPos)
{
    if (gh.get() == 0)
        throw runtime_error("GribFileMessage initialized with NULL-ptr");

    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "editionNumber", &edition_), 0);

    char msg[1024];
    size_t msgLength = 1024;

    if (edition_ == 1) {
        gridParmeterIds_ = vector<long> (3, 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "indicatorOfParameter", &gridParmeterIds_[0]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "gribTablesVersionNo", &gridParmeterIds_[1]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "identificationOfOriginatingGeneratingCentre", &gridParmeterIds_[2]), 0);
        // additional information
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "indicatorOfTypeOfLevel", &levelType_), 0);
    } else if (edition_ == 2) {
        gridParmeterIds_ = vector<long> (3, 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "parameterNumber", &gridParmeterIds_[0]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "parameterCategory", &gridParmeterIds_[1]), 0);
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "discipline", &gridParmeterIds_[2]), 0);
        // additional information
        MIFI_GRIB_CHECK(grib_get_long(gh.get(), "typeOfFirstFixedSurface", &levelType_), 0);
    } else {
        throw runtime_error("unknown grib version: " + type2string(edition_));
    }

    int nameError = grib_get_string(gh.get(), "name", msg, &msgLength);
    if (nameError != GRIB_NOT_FOUND) {
        MIFI_GRIB_CHECK(nameError, 0);
        parameterName_ = msg;
    } else {
        parameterName_ = join(gridParmeterIds_.begin(), gridParmeterIds_.end(), ",");
    }
    int shortNameError = grib_get_string(gh.get(), "short_name", msg, &msgLength);
    if (shortNameError != GRIB_NOT_FOUND) {
        MIFI_GRIB_CHECK(shortNameError, 0);
        shortName_ = msg;
    } else {
        shortName_ = join(gridParmeterIds_.begin(), gridParmeterIds_.end(), ",");
    }


    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "level", &levelNo_), 0);
    // time
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "dataDate", &dataDate_), 0);
    MIFI_GRIB_CHECK(grib_get_long(gh.get(), "dataTime", &dataTime_), 0);

    // TODO: gridDefinition_

}

GribFileMessage::GribFileMessage(boost::shared_ptr<XMLDoc> doc, string nsPrefix, xmlNodePtr node)
{
    string posStr = getXmlProp(node, "seekPos");
    if (posStr == "") {
        throw runtime_error("could not find seekPos for node");
    }
    filePos_ = string2type<size_t>(posStr);
    string msgPosStr = getXmlProp(node, "messagePos");
    if (msgPosStr == "") msgPos_ = 0;
    else msgPos_ = string2type<size_t>(msgPosStr);


    {// parameter
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":parameter", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size == 0) throw runtime_error("parameter not found in node");
        xmlNodePtr pNode = xp->nodesetval->nodeTab[0];
        parameterName_ = getXmlProp(pNode, "name");
        shortName_ = getXmlProp(pNode, "short_name");
        // grib
        XPathObjPtr xpG = doc->getXPathObject(nsPrefix+":grib1", pNode);
        int gSize = xpG->nodesetval ? xpG->nodesetval->nodeNr : 0;
        if (gSize > 0) {
            edition_ = 1;
            xmlNodePtr gNode = xpG->nodesetval->nodeTab[0];
            gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "indicatorOfParameter")));
            gridParmeterIds_.push_back(string2type<long>(getXmlProp(gNode, "gridTablesVersionNo")));
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
       // TODO: gridDefinition not implemented yet fully
        XPathObjPtr xp = doc->getXPathObject(nsPrefix+":gridDefinition", node);
        int size = xp->nodesetval ? xp->nodesetval->nodeNr : 0;
        if (size > 0) {
            xmlNodePtr lNode = xp->nodesetval->nodeTab[0];
            string projectionName = getXmlProp(lNode, "projectionName");
            long startX = string2type<long>(getXmlProp(lNode, "startX"));
            long startY = string2type<long>(getXmlProp(lNode, "startY"));
            long sizeX = string2type<long>(getXmlProp(lNode, "sizeX"));
            long sizeY = string2type<long>(getXmlProp(lNode, "sizeY"));
            long incrX = string2type<long>(getXmlProp(lNode, "incrX"));
            long incrY = string2type<long>(getXmlProp(lNode, "incrY"));
            long scanMode = string2type<long>(getXmlProp(lNode, "scanMode"));
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
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("seekPos"),
                xmlCast(type2string(filePos_))));
        checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast("messagePos"),
                xmlCast(type2string(msgPos_))));
        // parameter
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("parameter")));
        if (edition_ == 1) {
            checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("grib1")));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "indicatorOfParameter"), xmlCast(type2string(
                    gridParmeterIds_.at(0)))));
            checkLXML(xmlTextWriterWriteAttribute(writer.get(), xmlCast(
                    "gridTablesVersionNo"), xmlCast(type2string(
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

        // gribDefinition
        checkLXML(xmlTextWriterStartElement(writer.get(), xmlCast("gribDefinition")));
        checkLXML(xmlTextWriterWriteComment(writer.get(), xmlCast(
                "not implemented yet")));
        checkLXML(xmlTextWriterEndElement(writer.get()));

        // end the message
        checkLXML(xmlTextWriterEndElement(writer.get()));
    }
#else
    throw runtime_error("libxml_writer not enabled");
#endif

    return string(reinterpret_cast<const char*> (buffer->content));
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
    // TODO
}

void GribFileIndex::initByGrib(boost::filesystem::path gribFilePath)
{
    url_ = "file:"+ gribFilePath.file_string();
    size_t fsize = boost::filesystem::file_size(gribFilePath);
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
                messages_.push_back(GribFileMessage(gh, lastPos, msgPos));
        }
    }

}

void GribFileIndex::initByXML(boost::filesystem::path xmlFilePath)
{
    boost::shared_ptr<XMLDoc> doc(new XMLDoc(xmlFilePath.file_string()));
    doc->registerNamespace("gfi", "http://www.met.no/schema/fimex/gribFileIndex");
    XPathObjPtr xp = doc->getXPathObject("/gfi:gribFileIndex");
    cerr << xp->nodesetval->nodeNr;
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
