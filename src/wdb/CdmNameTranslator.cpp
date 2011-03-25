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

#include "CdmNameTranslator.h"

// fimex
//
#include <fimex/XMLDoc.h>


// 3rd party
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

// system
//
#include <boost/algorithm/string.hpp>


namespace MetNoFimex
{
namespace wdb
{

CdmNameTranslator::CdmNameTranslator()
{
}

CdmNameTranslator::~CdmNameTranslator()
{
}

bool CdmNameTranslator::isEmpty() const
{
    return mapWdbToCdm.empty();
}

size_t CdmNameTranslator::size() const
{
    return mapWdbToCdm.size();
}

void CdmNameTranslator::clear()
{
    mapWdbToCdm.clear();
}

void CdmNameTranslator::readXML(const XMLDoc& xmlDoc, const std::string& xpath)
{
    std::string wdbName;
	std::string cdmName;
	xmlNodePtr theNode;
	XPathObjPtr xpathObj = xmlDoc.getXPathObject(xpath);
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	for(int node_index = 0; node_index < size; ++node_index) {
	    theNode = nodes->nodeTab[node_index];
		if(theNode->type != XML_ELEMENT_NODE)
		    throw CdmNameTranslatorException("not XML_ELEMENT_NODE!");
		wdbName = getXmlProp(theNode, "name");

		// fetch attributes for this node
		//
		xmlNodePtr child = theNode->children;
		while(child != 0) {
			if ((child->type == XML_ELEMENT_NODE) &&
				(std::string("attribute") == std::string(reinterpret_cast<const char *>(child->name)))) {
				    std::string name = getXmlProp(child, "name");
					std::string value = getXmlProp(child, "value");
					if(name == std::string("standard_name")) {
					    cdmName = value;
						addNamePair(wdbName, cdmName);
					}
			}

			child = child->next;
		}
	}
}

void CdmNameTranslator::readXML(const XMLDoc& xmlDoc)
{
    readXML(xmlDoc, "/wdb_fimex_config/wdb_parameters/value_parameter");
	readXML(xmlDoc, "/wdb_fimex_config/wdb_parameters/level_parameter");
}

void CdmNameTranslator::readXML(const std::string& xmlFileName)
{
    XMLDoc xmlDoc(xmlFileName);
	readXML(xmlDoc);
}

void CdmNameTranslator::addNamePair(const std::string& wdbName, const std::string& cdmName)
{
    if(hasWdbName(wdbName))
	    throw CdmNameTranslatorException("wdbName: " + wdbName + " already exists!");

    if(hasCdmName(cdmName))
	    throw CdmNameTranslatorException("cdmName: " + cdmName + " already exists!");

    mapWdbToCdm.insert(std::pair<std::string, std::string>(wdbName, cdmName));
}

bool CdmNameTranslator::hasCdmName(const std::string& cdmName) const
{
    /**
	 * TODO:
	 *     try boost bidirectional map
	 *
	 *     ATM we can assume that map
	 *     will be relatively small to
	 *     do linera search
	 */
    std::map<std::string, std::string>::const_iterator cit = mapWdbToCdm.begin();
	for(; cit != mapWdbToCdm.end(); ++cit) {
			if(cit->second == cdmName)
                return true;
	}

	return false;
}

void CdmNameTranslator::removeCdmName(const std::string& cdmName)
{
    std::map<std::string, std::string>::iterator it = mapWdbToCdm.begin();
	for(; it != mapWdbToCdm.end(); ++it) {
	    if(it->second == cdmName){
		    mapWdbToCdm.erase(it);
		    return;
		}
	}
}

bool CdmNameTranslator::hasWdbName(const std::string& wdbName) const
{
    return mapWdbToCdm.find(wdbName) != mapWdbToCdm.end();
}

void CdmNameTranslator::removeWdbName(const std::string& wdbName)
{
    mapWdbToCdm.erase(wdbName);
}

std::string CdmNameTranslator::toCdmName(const std::string& wdbName) const
{
    if(hasWdbName(wdbName)) {
	    std::map<std::string, std::string>::const_iterator cit = mapWdbToCdm.find(wdbName);
	    return cit->second;
	} else {
	    /**
		 * default case
		 */
        return boost::algorithm::replace_all_copy(wdbName, " ", "_");
	}
}

std::string CdmNameTranslator::toWdbName(const std::string& cdmName) const
{
    if(hasCdmName(cdmName)) {
	   std::map<std::string, std::string>::const_iterator cit;
	   for(cit = mapWdbToCdm.begin(); cit != mapWdbToCdm.end(); ++cit) {
	       if(cit->second == cdmName)
	           return cit->first;
	   }
	} else {
        /**
		 * default
		 */
        return boost::algorithm::replace_all_copy(cdmName, "_", " ");
	}
}
}
}
