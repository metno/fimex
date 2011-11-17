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

#include "WdbConfiguration.h"
#include <fimex/CDMException.h>
#include <fimex/XMLDoc.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/program_options.hpp>
#include <boost/foreach.hpp>
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iostream>
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>


namespace MetNoFimex
{
namespace wdb
{

WdbConfiguration::WdbConfiguration(const std::string & configFile) :
		database_("wdb"),port_(5432),user_("wdb")
{
	boost::filesystem::path configFilePath(configFile);

	if ( not exists(configFilePath) )
		init_(configFile);
	else
	{
		if ( is_directory(configFilePath) )
			throw CDMException(configFile + " is a directory");
		init_(configFilePath);
	}

	if ( database_.empty() )
		throw std::runtime_error("Missing database name in spec");
	if ( user_.empty() )
		throw std::runtime_error("Missing user name in spec");
}

WdbConfiguration::~WdbConfiguration()
{
}

std::string WdbConfiguration::pqDatabaseConnectString() const
{
	std::ostringstream ret;
	ret << "dbname=" << database_ ;
	if ( not host_.empty() )
		ret << " host=" << host_;
	ret << " port=" << port_;
	ret << " user=" << user_;
	return ret.str();
}

const std::string & WdbConfiguration::wciUser() const
{
	if ( wciUser_.empty() )
		return user_;
	return wciUser_;
}

namespace
{
/**
 * Returns the default database target of WDB applications
 */
std::string getDefaultTarget()
{
	const char * database = getenv( "PGDATABASE" );
	if ( database )
		return database;
    return "wdb";
}

/**
 * Returns the default database host of WDB applications
 * @deprecated
 */
std::string getDefaultHost()
{
	return "";
}

/** Returns the default user of WDB applications
 * @deprecated
 */
std::string getDefaultUser()
{
    const char * dbuser = getenv( "PGUSER" );
    if ( dbuser )
        return dbuser;
    dbuser = getenv( "USER" );
    if ( dbuser )
    	return dbuser;
    return ""; // should never happen
}

/**
 * Returns the default port for WDB applications to connect on
 */
int getDefaultPort()
{
    const char * port = getenv( "PGPORT" );
    if ( port )
    {
        try
        {
            return boost::lexical_cast<int>( port );
        }
        catch( boost::bad_lexical_cast & )
        {
            std::ostringstream err;
            err << port << " is not a valid port number";
            throw std::runtime_error( err.str() );
        }
    }

    const int DEFAULT_POSTGRES_PORT = 5432;
    return DEFAULT_POSTGRES_PORT;
}
}


namespace
{
std::string singleValue(const XMLDoc & document, const std::string & path, const std::string & defaultValue = std::string())
{
	XPathObjPtr obj = document.getXPathObject(path);

	xmlNodeSetPtr nodeset = obj->nodesetval;

	if ( nodeset->nodeNr == 0 )
		return defaultValue;

	if ( nodeset->nodeNr > 1 )
		throw std::runtime_error(path + ": many such elements in xml (only one allowed)");

	return getXmlContent(nodeset->nodeTab[0]);
}

template<typename T>
std::vector<T> values(const XMLDoc & document, const std::string & path)
{
	std::vector<T> ret;

	XPathObjPtr obj = document.getXPathObject(path);
	xmlNodeSetPtr nodeset = obj->nodesetval;
	for ( int i = 0; i < nodeset->nodeNr; ++ i )
		ret.push_back(boost::lexical_cast<T>(getXmlContent(nodeset->nodeTab[i])));

	return ret;
}

}

void WdbConfiguration::init_(const boost::filesystem::path & configFile)
{
	XMLDoc document(configFile.file_string());

	std::string connection = "//wdb_query/connection/";

	database_ = singleValue(document, connection + "database", getDefaultTarget());
	host_ = singleValue(document, connection + "host", getDefaultHost());
	std::string port = singleValue(document, connection + "port", "");
	if ( not port.empty() )
		port_ = boost::lexical_cast<int>(port);
	else
		port_ = getDefaultPort();
	user_ = singleValue(document, connection + "user", getDefaultUser());

	wciUser_ = singleValue(document, "//wci/begin/user", user_);

	std::string read = "//wci/read/";
	BOOST_FOREACH(const std::string & dataProvider, values<std::string>(document, read + "dataprovider"))
		querySpec_.addDataProvider(dataProvider);
	std::string location = singleValue(document, read + "location");
	if ( not location.empty() )
		querySpec_.setLocation(location);
	std::string referenceTime = singleValue(document, read+"referencetime");
	if ( not referenceTime.empty() )
		querySpec_.setReferenceTime(referenceTime);
	std::string validTime = singleValue(document, read+"validtime");
	if ( not validTime.empty() )
		querySpec_.setValidTime(validTime);
	BOOST_FOREACH(const std::string & parameter, values<std::string>(document, read + "valueparameter"))
		querySpec_.addParameter(parameter);
	// Level is not supported yet
	BOOST_FOREACH(int dataVersion, values<int>(document, read + "dataversion"))
		querySpec_.addDataVersion(dataVersion);
}

void WdbConfiguration::init_(const std::string & configSpec)
{
	std::vector<std::string> elements;
	boost::split(elements, configSpec, boost::is_any_of(std::string(":;")));

	BOOST_FOREACH(const std::string & element, elements)
	{
		if ( element.empty() )
			continue;

		 std::string::size_type splitIndex = element.find('=');
		 if ( splitIndex == std::string::npos )
			 throw std::runtime_error("Invalid element specification: " + element);

		 const std::string & key = element.substr(0, splitIndex);
		 const std::string & value = element.substr(splitIndex +1, std::string::npos);

		 if ( key == "file" )
			 init_(boost::filesystem::path(value));
		 else if ( key == "dbname" )
			 database_ = value;
		 else if ( key == "user" )
			 user_ = value;
		 else if ( key == "host" )
			 host_ = value;
		 else if ( key == "port" )
		 {
			 try
		 	 {
				 port_ = boost::lexical_cast<int>(value);
		 	 }
		 	 catch ( boost::bad_lexical_cast & e )
		 	 {
		 		 throw std::runtime_error("Bad value for port: " + value);
		 	 }
		 }
		 else if ( key == "wciUser" )
			 wciUser_ = value;
		 else if ( key == "dataprovider" )
		 {
			 if ( value == "-" )
				 querySpec_.removeDataProviders();
			 else
				 querySpec_.addDataProvider(value);
		 }
		 else if ( key == "location" )
			 querySpec_.setLocation(value);
		 else if ( key == "referencetime" )
			 querySpec_.setReferenceTime(value);
		 else if ( key == "validtime" )
			 querySpec_.setValidTime(value);
		 else if ( key == "parameter" )
		 {
			 if ( value == "-" )
				 querySpec_.removeParameters();
			 else
				 querySpec_.addParameter(value);
		 }
		 else if ( key == "dataversion" )
		 {
			 if ( value == "-" )
				 querySpec_.removeDataVersions();
			 else
				 try
			 	 {
					 querySpec_.addDataVersion(boost::lexical_cast<int>(value));
			 	 }
			 	 catch ( boost::bad_lexical_cast & e )
			 	 {
			 		 throw std::runtime_error("Bad value for data version: " + value);
			 	 }
		 }

	}
}

}
}
