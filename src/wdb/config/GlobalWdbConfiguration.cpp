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

#include "GlobalWdbConfiguration.h"
#include <fimex/CDMException.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

#include <iostream>


namespace MetNoFimex
{
namespace wdb
{

const boost::filesystem::path GlobalWdbConfiguration::defaultConfigFile_ = PKGDATADIR"/wdb_config.xml";

GlobalWdbConfiguration::GlobalWdbConfiguration(const boost::filesystem::path & configFile)
{
	if ( configFile.empty() )
		init_(defaultConfigFile_);
	else
		init_(configFile);
}

void GlobalWdbConfiguration::init_(const boost::filesystem::path & configFile)
{
	if ( ! exists(configFile) )
		throw CDMException(configFile.string() + ": Unable to find configuration file");

	if ( is_directory(configFile) )
		throw CDMException(configFile.string() + ": Configuration file is a directory");

	XMLDoc config(configFile.string());
	initParseGlobalAttributes_(config);
	initParseAllParameters_(config);
}

GlobalWdbConfiguration::~GlobalWdbConfiguration()
{
}

std::string GlobalWdbConfiguration::cfName(const std::string & wdbName) const
{
	NameTranslation::const_iterator find = wdb2cf_.find(wdbName);
	if ( find != wdb2cf_.end() )
		return find->second;

	return boost::algorithm::replace_all_copy(wdbName, " ", "_");
}

namespace
{
	template <typename T>
	void setAttribute(GlobalWdbConfiguration::AttributeList & out, const std::string & name, T value)
	{
		for ( GlobalWdbConfiguration::AttributeList::const_iterator find = out.begin(); find != out.end(); ++ find )
			if ( find->getName() == name )
				return;
		out.push_back(CDMAttribute(name, value));
	}
}

GlobalWdbConfiguration::AttributeList GlobalWdbConfiguration::getAttributes(const std::string & wdbParameter, const std::string & wdbUnit) const
{
	AttributeList ret;

	NameToAttributeMap::const_iterator find = attributes_.find(wdbParameter);
	if ( find != attributes_.end() )
		ret = find->second;

	// all these may be overridden by config file:
	setAttribute(ret, "units", cfName(wdbUnit));
	setAttribute(ret, "standard_name", cfName(wdbParameter));
	setAttribute(ret, "long_name", wdbParameter);

	//setAttribute(ret, "_FillValue", std::numeric_limits<float>::quiet_NaN());

	return ret;
}

void GlobalWdbConfiguration::initParseGlobalAttributes_(XMLDoc & config)
{
	XPathObjPtr parameters = config.getXPathObject("/wdb_netcdf_config/global_attributes/attribute");
	if ( parameters->nodesetval )
	{
		for ( int i = 0; i < parameters->nodesetval->nodeNr; ++ i )
		{
			xmlNodePtr node = parameters->nodesetval->nodeTab[i];
			if(node->type != XML_ELEMENT_NODE)
				throw CDMException("not XML_ELEMENT_NODE!");
			std::string name = getXmlProp(node, "name");
			std::string value = getXmlProp(node, "value");
			std::string type = getXmlProp(node, "type");
			if ( type.empty() )
				type = "string";

			globalAttributes_.push_back(CDMAttribute(name, type, value));
		}
	}
}

void GlobalWdbConfiguration::initParseTranslation_(const xmlNodePtr & translationNode)
{
	if(translationNode->type != XML_ELEMENT_NODE)
	    throw CDMException("not XML_ELEMENT_NODE!");
	std::string wdbName = getXmlProp(translationNode, "wdbname");
	std::string cfName  = getXmlProp(translationNode, "cfname");

	if ( not cfName.empty() )
	{
		wdb2cf_[wdbName] = cfName;
		cf2wdb_[cfName] = wdbName;
	}
}

void GlobalWdbConfiguration::initParseParameters_(const XPathObjPtr parameters)
{
	if ( parameters->nodesetval )
		{
			for ( int i = 0; i < parameters->nodesetval->nodeNr; ++ i )
			{
				xmlNodePtr node = parameters->nodesetval->nodeTab[i];
				initParseTranslation_(node);

				std::string wdbName = getXmlProp(node, "wdbname");

				for ( xmlNodePtr child = node->children; child; child = child->next )
				{
					std::string name = std::string(reinterpret_cast<const char *>(child->name));
					if ((child->type == XML_ELEMENT_NODE) and "attribute" == name )
					{
						std::string name = getXmlProp(child, "name");
						std::string value = getXmlProp(child, "value");
						std::string dataType = getXmlProp(child, "type");
						if ( dataType.empty() )
							dataType = "string";

						attributes_[wdbName].push_back(CDMAttribute(name, dataType, value));
					}
				}
			}
		}
}

void GlobalWdbConfiguration::initParseAllParameters_(XMLDoc & config)
{
	initParseParameters_(config.getXPathObject("/wdb_netcdf_config/wdb_parameters/value_parameter"));
	initParseParameters_(config.getXPathObject("/wdb_netcdf_config/wdb_parameters/level_parameter"));

	XPathObjPtr units = config.getXPathObject("/wdb_netcdf_config/units/translation");
	if ( units->nodesetval )
			for ( int i = 0; i < units->nodesetval->nodeNr; ++ i )
			{
				xmlNodePtr node = units->nodesetval->nodeTab[i];
				initParseTranslation_(node);
			}
}

}
}
