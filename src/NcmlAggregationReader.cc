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

#include "fimex/AggregationReader.h"
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
#include "fimex/XMLUtils.h"

#include <memory>
#include <regex>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.NcmlAggregationReader");

class NullCDMReader : public CDMReader
{
public:
    using CDMReader::getDataSlice;
    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0) override { return nullptr; }
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override { return nullptr; }
};

} // namespace

static void getFileTypeConfig(const std::string& location, std::string& file, std::string& type, std::string& config)
{
    const auto locations = tokenize(location, " ");
    file = (locations.size() > 0) ? locations.at(0) : "";
    type = (locations.size() > 1) ? locations.at(1) : "netcdf";
    config = (locations.size() > 2) ? locations.at(2) : "";
}

NcmlAggregationReader::NcmlAggregationReader(const XMLInput& ncml)
    : reader_(std::make_shared<NullCDMReader>())
{
    XMLDoc_p doc;
    if (!ncml.isEmpty()) {
        doc = ncml.getXMLDoc();
        doc->registerNamespace("nc", "http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2");
        XPathNodeSet nodes(doc, "/nc:netcdf");
        if (nodes.size() != 1) {
            throw CDMException("config "+ncml.id()+" is not a ncml document with root /nc:netcdf");
        }
    }

    // warn if multiple aggregations, joinNew aggregations
    // and get a global file if no aggregations
    XPathNodeSet nodesAgg(doc, "/nc:netcdf/nc:aggregation");
    if (nodesAgg.size() == 0) {
        LOG4FIMEX(logger, Logger::DEBUG, "found no ncml-aggregations in "<< ncml.id());
        XPathNodeSet nodesL(doc, "/nc:netcdf[@location]");
        if (nodesL.size() != 1) {
            LOG4FIMEX(logger, Logger::INFO, "config " << ncml.id() << " does not contain location-attribute, only ncml initialization");
        } else {
            std::string file, type, config;
            getFileTypeConfig(getXmlProp(nodesL[0], "location"), file, type, config);
            LOG4FIMEX(logger, Logger::DEBUG, "reading file '" << file << "' type '" << type << "' config '" << config << "'");
            reader_ = CDMFileReaderFactory::create(type, file, config);
        }
    } else if (nodesAgg.size() > 1) {
        LOG4FIMEX(logger, Logger::WARN, "found several ncml-aggregations in " << ncml.id() << ", using 1st");
    } else {
        auto agg = std::make_shared<AggregationReader>(getXmlProp(nodesAgg[0], "type"));

        // find sources from location, scan, ...
        size_t idx = 0;
        for (auto node : XPathNodeSet(doc, "./nc:netcdf", nodesAgg[0])) {
            // open <netcdf /> tags recursively
            const std::string id = ncml.id() + ":netcdf:" + type2string(idx++);
            const std::string current = doc->toString(node);
            agg->addReader(std::make_shared<NcmlCDMReader>(XMLInputString(current, id)), id);
        }

        // open reader by scan
        const XPathNodeSet nodesScan(doc, "./nc:scan", nodesAgg[0]);
        for (auto node : nodesScan) {
            std::string dir, type, config;
            getFileTypeConfig(getXmlProp(node, "location"), dir, type, config);
            std::string suffix = getXmlProp(node, "suffix");
            std::string regExp;
            if (!suffix.empty()) {
                regExp = ".*" + regex_escape(suffix) + "$";
            } else {
                regExp = getXmlProp(node, "regExp"); // what type of regex is this?
            }
            std::string subdirs = getXmlProp(node, "subdirs");
            int depth = -1; //negative depth == unlimited
            if (subdirs == "false" || subdirs == "FALSE" || subdirs == "False") {
                depth = 0;
            }
            std::vector<std::string> files;
            scanFiles(files, dir, depth, std::regex(regExp), true);
            for (size_t i = 0; i < files.size(); ++i) {
                LOG4FIMEX(logger, Logger::DEBUG, "scanned file: " << files.at(i));
                try {
                    agg->addReader(CDMFileReaderFactory::create(type, files.at(i), config), files.at(i));
                } catch (CDMException& ex) {
                    LOG4FIMEX(logger, Logger::ERROR, "cannot read scanned file '" << files.at(i) << "' type: " << type << ", config: " << files.at(i) );

                }
            }
        }
        agg->initAggregation();
        reader_ = agg;
    }

    *(this->cdm_) = reader_->getCDM();
}

NcmlAggregationReader::~NcmlAggregationReader() {}

DataPtr NcmlAggregationReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    return reader_->getDataSlice(varName, unLimDimPos);
}

DataPtr NcmlAggregationReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    return reader_->getDataSlice(varName, sb);
}

} /* namespace MetNoFimex */
