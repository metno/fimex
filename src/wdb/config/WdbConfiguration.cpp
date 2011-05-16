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
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/program_options.hpp>
#include <boost/foreach.hpp>
#include <fstream>

namespace MetNoFimex
{

namespace wdb
{

WdbConfiguration::WdbConfiguration(const boost::filesystem::path & configFile)
{
	if ( not exists(configFile) )
		throw CDMException(configFile.string() + " no such file");

	if ( is_directory(configFile) )
		throw CDMException(configFile.string() + " is a directory");

	boost::filesystem::ifstream configStream(configFile);
	init_(configStream);
}

WdbConfiguration::WdbConfiguration(std::istream & configStream)
{
	init_(configStream);
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
std::string getDefaultUser() // Not used
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

void WdbConfiguration::init_(std::istream & configStream)
{
	using namespace boost::program_options;

    options_description database("Database configuration");
    database.add_options()
    ( "database,d", value( & database_ )->default_value( getDefaultTarget() ), "Database name (ex. wdb)" )
    ( "host,h", value( & host_ )->default_value( getDefaultHost() ), "Database host (ex. somehost.met.no)" )
    ( "user,u", value( & user_ )->default_value( getDefaultUser() ), "Database user name" )
    ( "port,p", value( & port_ )->default_value( getDefaultPort() ), "Database port number to connect to" )
    ;

    options_description wciBegin("wci.begin options");
    wciBegin.add_options()
    ( "wci.user", value( & wciUser_ ), "User name for wci. Defaults to database user name." )
    ;

    std::vector<std::string> dataprovider;
    std::vector<std::string> valueparameter;
    std::vector<int> dataversion;

    options_description queryOptions("wci.read query options");
    queryOptions.add_options()
    ( "wci.read.dataprovider", value(& dataprovider), "Dataproviders to request")
    ( "wci.read.location", value<std::string>(), "Name of grid to request")
    ( "wci.read.referencetime", value<std::string>(), "Reference time to use")
    ( "wci.read.valueparameter", value(& valueparameter), "Value parameters to use")
    ( "wci.read.dataversion", value(& dataversion), "Data versions to use")
    		;

    options_description moreConfig("Extra config files");
    moreConfig.add_options()
    ( "include", value<boost::filesystem::path>(), "read aditional configuration from this file")
    		;


    options_description allOptions("Options");
    allOptions.add(database);
    allOptions.add(wciBegin);
    allOptions.add(queryOptions);
    allOptions.add(moreConfig);

    variables_map vm;
    try
    {
    	store(parse_config_file(configStream, allOptions, true), vm);
    	notify(vm);
    }
    catch ( std::exception & e )
    {
    	throw CDMException(e.what());
    }

    BOOST_FOREACH(const std::string & val, dataprovider )
    	querySpec_.addDataProvider(val);

    if ( vm.count("wci.read.location") )
    	querySpec_.setLocation(vm["wci.read.location"].as<std::string>());
    if ( vm.count("wci.read.referencetime") )
    	querySpec_.setReferenceTime(vm["wci.read.referencetime"].as<std::string>());

    BOOST_FOREACH(const std::string & val, valueparameter )
    	querySpec_.addParameter(val);
    BOOST_FOREACH(int i, dataversion )
    	querySpec_.addDataVersion(i);

    if ( wciUser_.empty() )
    	wciUser_ = user_;

    if ( vm.count("include") )
    {
    	boost::filesystem::path configFile = vm["include"].as<boost::filesystem::path>();
    	if ( ! exists(configFile) )
    		throw std::runtime_error(configFile.string() + ": no such file");
    	if ( is_directory(configFile) )
    		throw std::runtime_error(configFile.string() + " is a directory");

    	boost::filesystem::ifstream config(configFile);
    	init_(config);
    }
}


}

}
