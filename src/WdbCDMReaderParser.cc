/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#include "fimex/WdbCDMReaderParser.h"

// libxml2
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

// boost
//
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>

// std
//
#include <limits>

namespace po = boost::program_options;

namespace MetNoFimex {

WdbCDMReaderParserInfo::WdbCDMReaderParserInfo()
    : wdbPort_(5432)
{
}

std::string WdbCDMReaderParserInfo::wdbHost() const
{
    return wdbHost_;
}

std::string WdbCDMReaderParserInfo::wdbName() const
{
    return wdbName_;
}

std::string WdbCDMReaderParserInfo::wdbUser() const
{
    return wdbUser_;
}

std::string WdbCDMReaderParserInfo::wciUser() const
{
    return wciUser_;
}

std::string WdbCDMReaderParserInfo::configFileName() const
{
    return configFileName_;
}

std::string WdbCDMReaderParserInfo::provider() const
{
    return provider_;
}

std::string WdbCDMReaderParserInfo::place() const
{
    return place_;
}

std::string WdbCDMReaderParserInfo::referenceTime() const
{
    return referenceTime_;
}

unsigned short WdbCDMReaderParserInfo::wdbPort() const
{
    return wdbPort_;
}


WdbCDMReaderParser::WdbCDMReaderParser()
{
}

WdbCDMReaderParser::~WdbCDMReaderParser()
{
}

std::pair<std::string, std::string> WdbCDMReaderParser::getNameValuePairForXmlPath(const XMLDoc& doc, const std::string& path)
{
    if(path.empty()) // just return empty
        return std::make_pair(std::string(), std::string());

    std::string name;
    std::string value;
    xmlNodePtr xmlNode;
    {
        XPathObjPtr xpathObj = doc.getXPathObject(path);
        xmlNodeSetPtr xmlNodes = xpathObj->nodesetval;
        /**
          * int nodessize = (xmlNodes) ? xmlNodes->nodeNr : 0;
          *
          * ATM:
          * to keep things simple
          * take just firs occurence
          * ignore rest (nodessize set to 1)
          */
        int nodessize = (xmlNodes) ? 1 : 0;
        for(int index = 0; index < nodessize; ++index) {
             xmlNode = xmlNodes->nodeTab[index];
             /**
               * TODO: throw exception in future
               */
             assert(xmlNode->type == XML_ELEMENT_NODE);
             name = getXmlProp(xmlNode, "name");
             value = getXmlProp(xmlNode, "value");
         }
    }
    return std::make_pair(name, value);
}

std::vector<std::pair<std::string, std::string> > WdbCDMReaderParser::getNameValuePairsForXmlPath(const XMLDoc& doc, const std::string& path)
{
    std::vector<std::pair<std::string, std::string> > pairs;
    if(path.empty()) // just return empty
        return pairs;

    std::string name;
    std::string value;
    xmlNodePtr xmlNode;
    {
        XPathObjPtr xpathObj = doc.getXPathObject(path);
        xmlNodeSetPtr xmlNodes = xpathObj->nodesetval;
        int nodessize = (xmlNodes) ? xmlNodes->nodeNr : 0;
        for(int index = 0; index < nodessize; ++index) {
             xmlNode = xmlNodes->nodeTab[index];
             /**
               * TODO: throw exception in future
               */
             assert(xmlNode->type == XML_ELEMENT_NODE);
             name = getXmlProp(xmlNode, "name");
             value = getXmlProp(xmlNode, "value");
             pairs.push_back(std::make_pair(name, value));
         }
    }
    return pairs;
}

std::vector<std::pair<std::string, std::string> > WdbCDMReaderParser::getAttributesForXmlPath(const XMLDoc& doc, const std::string& path)
{
    std::vector<std::pair<std::string, std::string> > pairs;
    if(path.empty()) // just return empty
        return pairs;

    xmlNodePtr xmlNode;
    {
        XPathObjPtr xpathObj = doc.getXPathObject(path);
        xmlNodeSetPtr xmlNodes = xpathObj->nodesetval;
        int nodessize = (xmlNodes) ? xmlNodes->nodeNr : 0;

        for(int index = 0; index < nodessize; ++index) {
            xmlNode = xmlNodes->nodeTab[0];
            assert(xmlNode->type == XML_ELEMENT_NODE);
            // fetch attributes for this node
            //
            xmlNodePtr child = xmlNode->children;
            while(child != 0) {
                if ((child->type == XML_ELEMENT_NODE) &&
                    (std::string("attribute") == std::string(reinterpret_cast<const char *>(child->name)))) {
                    std::string name = getXmlProp(child, "name");
                    std::string value = getXmlProp(child, "value");
                    pairs.push_back(std::make_pair(name, value));
                }
                child = child->next;
          }
       }
    }
    return pairs;
}

void WdbCDMReaderParser::parseCfgFile(const std::string& cfgFileName, WdbCDMReaderParserInfo& info)
{
    if(cfgFileName.empty())
        return;

    // use XML config file information
    //
    XMLDoc doc(cfgFileName);

    std::vector<std::pair<std::string, std::string> >
            wdbConnectionAttributes =
                    getAttributesForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/wdb_connection");
    if(!wdbConnectionAttributes.empty()) {
        std::vector<std::pair<std::string, std::string> >::const_iterator cit;
        for(cit = wdbConnectionAttributes.begin(); cit != wdbConnectionAttributes.end(); ++cit) {
            std::string name = cit->first;
            std::string value = cit->second;
            if(name == std::string("dbhost")) {
                info.wdbHost_ = value;
            } else if(name == std::string("dbname")) {
                info.wdbName_ = value;
            } else if(name == std::string("dbuser")) {
                info.wdbUser_ = value;
            } else if(name == std::string("wciuser")) {
                info.wciUser_ = value;
            } else if(name == std::string("dbport")) {
                info.wdbPort_ = value.empty() ? std::numeric_limits<unsigned int>::quiet_NaN() : boost::lexical_cast<unsigned int>(value);
            }
        }
    }

    info.referenceTime_ = getNameValuePairForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/reference_times/time").second;
    info.provider_ = getNameValuePairForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/data_providers/provider").second;
    info.place_ = getNameValuePairForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/grid_places/place").second;

}

/**
  * the source that has format
  * dbHost=<string>;dbName=<string>;dbPort=<string>;dbUser=<string>;wciUser=<string>;provider=<string>;place=<string>;refTime=<string>
  * refTime is  given as iso string "20110210T000000"
  */
void WdbCDMReaderParser::parseSource(const std::string& source, WdbCDMReaderParserInfo& info)
{
    if(source.empty())
        return;

    std::vector<std::string> splitvector;
    boost::algorithm::split(splitvector, source, boost::algorithm::is_any_of(";"));
    // todo: exception
    //
    assert(splitvector.size() != 0);
    std::map<std::string, std::string> splitmap;
    for(unsigned int i = 0; i < splitvector.size(); ++i) {
        std::vector<std::string> subsplit;
        boost::algorithm::split(subsplit, splitvector.at(i), boost::algorithm::is_any_of("="));
        if(subsplit.size() != 2)
            continue;
        splitmap[subsplit.at(0)] = subsplit.at(1);
    }

    if(!splitmap["dbHost"].empty())
        info.wdbHost_ = splitmap["dbHost"];

    if(!splitmap["dbName"].empty())
        info.wdbName_ = splitmap["dbName"];

    if(!splitmap["dbUser"].empty())
        info.wdbUser_ = splitmap["dbUser"];

    if(!splitmap["wciUser"].empty())
        info.wciUser_ = splitmap["wciUser"];

    if(!splitmap["provider"].empty())
        info.provider_ = splitmap["provider"];

    if(!splitmap["place"].empty())
        info.place_ = splitmap["place"];

    // as ISO formated string
    // convert to POSIX time ?
    if(!splitmap["refTime"].empty())
        info.referenceTime_ = splitmap["refTime"];

    if(!splitmap["dbPort"].empty())
        info.wdbPort_ = boost::lexical_cast<unsigned int>(splitmap["dbPort"]);
}

WdbCDMReaderParserInfo WdbCDMReaderParser::parse(const std::string& source, const std::string& configFileName, bool bParseConfigFile)
{
    WdbCDMReaderParserInfo wdbInfo;

    if(bParseConfigFile) {
        parseCfgFile(configFileName, wdbInfo);
    }

    // source - stuff from command line
    // will override config file
    parseSource(source, wdbInfo);

    return wdbInfo;
}

WdbCDMReaderParserInfo WdbCDMReaderParser::parse(int argc, char* args[], bool bParseConfigFile)
{
    // Declare a group of options
    // relevant only for wdbcdmreader
    // reader
    po::options_description config("wdbcdmreader options");
    config.add_options()
            ("input.file", po::value<std::string>(), "input file")
            ("input.config", po::value<std::string>(), "non-standard input configuration")
            ;

    po::options_description cmdline_options;
    cmdline_options.add(config);

    // read cmd-line
    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(cmdline_options).run(), vm);
    po::notify(vm);

    std::string source;
    if (vm.count("input.file")) {
        source = vm["input.file"].as<std::string>();
    }

    std::string cfgFileName;
    if (vm.count("input.config")) {
        cfgFileName = vm["input.config"].as<std::string>();
    }

    return parse(source, cfgFileName);
}
}
