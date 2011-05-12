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

#include "WdbCDMReaderParser.h"

// libxml2
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

// boost
//
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string.hpp>

// std
//
#include <limits>

namespace MetNoFimex {

namespace wdb
{

WdbCDMReaderParser::WdbCDMReaderParser()
{
}

WdbCDMReaderParser::~WdbCDMReaderParser()
{
}

WdbCDMReaderParser::NameValuePair WdbCDMReaderParser::getNameValuePairForXmlPath(const XMLDoc& doc, const std::string& path)
{
    if(path.empty()) // just return empty
        return std::make_pair(std::string(), std::string());

    std::string name;
    std::string value;
    xmlNodePtr xmlNode;
    {
        XPathObjPtr xpathObj;
        xpathObj = doc.getXPathObject(path);

        xmlNodeSetPtr xmlNodes = xpathObj->nodesetval;

        if ( ! xmlNodes->nodeTab )
        	return std::make_pair(std::string(), std::string());

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

std::vector<WdbCDMReaderParser::NameValuePair > WdbCDMReaderParser::getNameValuePairsForXmlPath(const XMLDoc& doc, const std::string& path)
{
    std::vector<NameValuePair > pairs;
    if(path.empty()) // just return empty
        return pairs;

    std::string name;
    std::string value;
    xmlNodePtr xmlNode;
    {
        XPathObjPtr xpathObj = doc.getXPathObject(path);
        if(xpathObj.get() == 0) {
            return pairs;
        }
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

std::vector<WdbCDMReaderParser::NameValuePair > WdbCDMReaderParser::getAttributesForXmlPath(const XMLDoc& doc, const std::string& path)
{
    std::vector<NameValuePair > pairs;
    if(path.empty()) // just return empty
        return pairs;

    xmlNodePtr xmlNode;
    {
        XPathObjPtr xpathObj = doc.getXPathObject(path);

        if(xpathObj.get() == 0)
            return pairs;

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

    // use XML config file inforitmation
    //
    XMLDoc doc(cfgFileName);

    std::vector<NameValuePair >
            wdbConnectionAttributes =
                    getAttributesForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/wdb_connection");
    if(!wdbConnectionAttributes.empty()) {
        std::vector<NameValuePair >::const_iterator cit;
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

    WciReadQuerySpecification & readSpec = info.wciReadQuerySpecification_;


    std::vector<NameValuePair> dataProviders = getNameValuePairsForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/data_providers/provider");
    for ( std::vector<NameValuePair>::const_iterator it = dataProviders.begin(); it != dataProviders.end(); ++ it )
    	readSpec.addDataProvider(it->second);

    std::vector<NameValuePair> parameters = getNameValuePairsForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/value_parameter/parameter");
    for ( std::vector<NameValuePair>::const_iterator it = parameters.begin(); it != parameters.end(); ++ it )
    	readSpec.addParameter(it->second);

    std::vector<NameValuePair> dataVersions = getNameValuePairsForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/data_version/version");
    for ( std::vector<NameValuePair>::const_iterator it = dataVersions.begin(); it != dataVersions.end(); ++ it )
    	readSpec.addDataVersion(boost::lexical_cast<int>(it->second));


    std::vector<NameValuePair> location = getNameValuePairsForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/grid_places/place");
    if ( location.size() == 1 )
    	readSpec.setLocation(location.front().second);
    else if ( location.size() > 1 )
    	throw CDMException("Only one place name is allowed in configuration");

    std::vector<NameValuePair> referenceTime = getNameValuePairsForXmlPath(doc, "/wdb_fimex_config/wdb_parameters/reference_times/time");
    if ( referenceTime.size() == 1 )
    	readSpec.setReferenceTime(referenceTime.front().second);
    else if ( referenceTime.size() > 1 )
    	throw CDMException("Only one reference time is allowed in configuration");


    std::vector<NameValuePair > globals =
    		getAttributesForXmlPath(doc, "/wdb_fimex_config/global_attributes");
    for ( std::vector<NameValuePair >::const_iterator it = globals.begin(); it != globals.end(); ++ it )
    	info.globalAttributes_.push_back(CDMAttribute(it->first, it->second));
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

    if(splitmap.find("dbHost") != splitmap.end())
        info.wdbHost_ = splitmap["dbHost"];

    if(splitmap.find("dbName") != splitmap.end())
        info.wdbName_ = splitmap["dbName"];

    if(splitmap.find("dbUser") != splitmap.end())
        info.wdbUser_ = splitmap["dbUser"];

    if(splitmap.find("wciUser") != splitmap.end())
        info.wciUser_ = splitmap["wciUser"];

    WciReadQuerySpecification & querySpec = info.wciReadQuerySpecification_;

    if(splitmap.find("provider") != splitmap.end())
    {
    	querySpec.clearDataProvider();
        querySpec.addDataProvider(splitmap["provider"]);
    }

    if(splitmap.find("place") != splitmap.end())
    {
    	querySpec.clearLocation();
    	querySpec.setLocation(splitmap["place"]);
    }

    // as ISO formated string
    // convert to POSIX time ?
    if(splitmap.find("refTime") != splitmap.end())
    {
    	querySpec.clearReferenceTime();
    	querySpec.setReferenceTime(splitmap["refTime"]);
    }

    if(splitmap.find("dbPort") != splitmap.end())
        info.wdbPort_ = boost::lexical_cast<unsigned int>(splitmap["dbPort"]);
}

WdbCDMReaderParserInfo WdbCDMReaderParser::parse(const std::string& source, const std::string& configFileName)
{
    WdbCDMReaderParserInfo wdbInfo;

	parseCfgFile(configFileName, wdbInfo);

    // source - stuff from command line
    // will override config file
    parseSource(source, wdbInfo);

    return wdbInfo;
}

}
}
