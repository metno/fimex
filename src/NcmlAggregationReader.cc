/*
 * Fimex, NcmlAggregationReader.cc
 *
 * (C) Copyright 2013-2019, met.no
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
 *  Created on: Apr 17, 2013
 *      Author: heikok
 */

#include "NcmlAggregationReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/FileUtils.h"
#include "fimex/Logger.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/StringUtils.h"
#include "fimex/Type2String.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInputString.h"

#include <memory>
#include <regex>

#include <libxml/tree.h>
#include <libxml/xpath.h>

namespace MetNoFimex {

using namespace std;

namespace {
Logger_p logger = getLogger("fimex.NcmlAggregationReader");
}

static void getFileTypeConfig(const string& location, string& file, string& type, string& config)
{
    vector<string> locations = tokenize(location, " ");
    file = (locations.size() > 0) ? locations.at(0) : "";
    type = (locations.size() > 1) ? locations.at(1) : "netcdf";
    config = (locations.size() > 2) ? locations.at(2) : "";
}

NcmlAggregationReader::NcmlAggregationReader(const XMLInput& ncml)
    : AggregationReader("")
{
    XMLDoc_p doc;
    if (!ncml.isEmpty()) {
        doc = ncml.getXMLDoc();
        doc->registerNamespace("nc", "http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2");
        xmlXPathObject_p xpathObj = doc->getXPathObject("/nc:netcdf");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        if (nodes->nodeNr != 1) {
            throw CDMException("config "+ncml.id()+" is not a ncml document with root /nc:netcdf");
        }
    }

    // warn if multiple aggregations, joinNew aggregations
    // and get a global file if no aggregations
    xmlXPathObject_p xpathObj = doc->getXPathObject("/nc:netcdf/nc:aggregation");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size == 0) {
        LOG4FIMEX(logger, Logger::DEBUG, "found no ncml-aggregations in "<< ncml.id());
        xmlXPathObject_p xpathObjL = doc->getXPathObject("/nc:netcdf[@location]");
        xmlNodeSetPtr nodesL = xpathObjL->nodesetval;
        if (nodesL->nodeNr != 1) {
            LOG4FIMEX(logger, Logger::INFO, "config " << ncml.id() << " does not contain location-attribute, only ncml initialization");
        } else {
            string file, type, config;
            getFileTypeConfig(getXmlProp(nodesL->nodeTab[0], "location"), file, type, config);
            LOG4FIMEX(logger, Logger::DEBUG, "reading file '" << file << "' type '" << type << "' config '" << config << "'");
            gDataReader_ = CDMFileReaderFactory::create(type, file, config);
            *(this->cdm_) = gDataReader_->getCDM();
        }
    } else {
        if (size > 1) {
            LOG4FIMEX(logger, Logger::WARN, "found several ncml-aggregations in " << ncml.id() << ", using 1st");
        }
        // find sources from location, scan, ...
        xmlXPathObject_p xpathObjNc = doc->getXPathObject("./nc:netcdf", nodes->nodeTab[0]);
        xmlNodeSetPtr nodesNc = xpathObjNc->nodesetval;
        int sizeNc = (nodesNc) ? nodesNc->nodeNr : 0;
        for (int i = 0; i < sizeNc; i++) {
            // open <netcdf /> tags recursively
            const string id = ncml.id() + ":netcdf:" + type2string(i);
            const string current = doc->toString(nodesNc->nodeTab[i]);
            addReader(std::make_shared<NcmlCDMReader>(XMLInputString(current, id)), id);
        }

        aggType_ = getXmlProp(nodes->nodeTab[0], "type");
        // open reader by scan
        xmlXPathObject_p xpathObjScan = doc->getXPathObject("./nc:scan", nodes->nodeTab[0]);
        xmlNodeSetPtr nodesScan = xpathObjScan->nodesetval;
        int sizeScan = (nodesScan) ? nodesScan->nodeNr : 0;
        for (int i = 0; i < sizeScan; i++) {
            string dir, type, config;
            getFileTypeConfig(getXmlProp(nodesScan->nodeTab[i], "location"), dir, type, config);
            string suffix = getXmlProp(nodesScan->nodeTab[i], "suffix");
            std::string regExp;
            if (!suffix.empty()) {
                regExp = ".*" + regex_escape(suffix) + "$";
            } else {
                regExp = getXmlProp(nodesScan->nodeTab[i], "regExp"); // what type of regex is this?
            }
            string subdirs = getXmlProp(nodesScan->nodeTab[i], "subdirs");
            int depth = -1; //negative depth == unlimited
            if (subdirs == "false" || subdirs == "FALSE" || subdirs == "False") {
                depth = 0;
            }
            vector<string> files;
            scanFiles(files, dir, depth, std::regex(regExp), true);
            for (size_t i = 0; i < files.size(); ++i) {
                LOG4FIMEX(logger, Logger::DEBUG, "scanned file: " << files.at(i));
                try {
                    addReader(CDMFileReaderFactory::create(type, files.at(i), config), files.at(i));
                } catch (CDMException& ex) {
                    LOG4FIMEX(logger, Logger::ERROR, "cannot read scanned file '" << files.at(i) << "' type: " << type << ", config: " << files.at(i) );

                }
            }
        }
        initAggregation();
    }
}

NcmlAggregationReader::~NcmlAggregationReader() {}

} /* namespace MetNoFimex */
