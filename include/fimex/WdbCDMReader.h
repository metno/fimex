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

#ifndef WDBCDMREADER_H_
#define WDBCDMREADER_H_

#ifndef MIFI_IO_READER_SUPPRESS_DEPRECATED
#warning \
  This header-file is deprecated and \
  may be removed without further notice at a future date. Please use a \
  non-deprecated interface with equivalent functionality instead, i.e. \
  instead of \
    *CDMReader(file,config) \
  use \
    CDMFileReaderFactory::create(MIFI_FILETYPE_*,file,config)
#endif

#include "fimex/CDMReader.h"
#include "fimex/XMLInput.h"
#include <string>




namespace MetNoFimex
{

/**
 * CDM reader for wdb databases.
 *
 * Since wdb is a database system, a configuration file is used in place of a
 * "real" data file. This configuration file tells how to connect to a wdb
 * database, and what query to perform on it.
 *
 * General syntax for the wdb query file may be found in the wdb_query.xsd
 * file, with an annotated example in the wdb.example.wdbml file.
 *
 * Also, it is possible to use a specification in place of the file. That
 * specification is a semicolon separated list of name-value pairs.
 *
 * It is possible to use a hybrid specification, in which you give additions
 * to the queries in the query file. If you want to specify queries in this
 * way you must follow the syntax, used in the following example:
 *
 * "file=whatever.wdbml;dataprovider=whoever;referencetime=latest"
 *
 * The following keywords are recognized: file, dbname, host, port, user,
 * wciUser, dataprovider, location, referencetime, validtime, parameter and
 * dataversion. They follow the same rule as the corresponding keywords in
 * the xml-files.
 *
 * Syntax for global configuration is given in wdb_conf.xsd.
 *
 * Since there is much freedom in wdb, the generated CDMs from different wdb
 * instances can be very different from each other. In general all dimensions
 * of size one will be skipped in the resulting dimensions and variables.
 */
class WdbCDMReader: public CDMReader
{
public:

    /**
     * Specifications of where to find the database, and what query to run on
     * it is given in the file with name source. Generic specifications are
     * given in the file with name configfilename.
     */
    WdbCDMReader(const std::string& source, const XMLInput& configXML);

    WdbCDMReader(const std::string& source, const std::string & configFile );


    virtual ~WdbCDMReader();

    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);

    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

private:
    std::size_t getXSize(const CDMVariable& variable) const;
    std::size_t getYSize(const CDMVariable& variable) const;
    std::size_t getGridSize(const CDMVariable& variable) const;
    boost::shared_ptr<Data> extractDataFromField(const CDMVariable& variable, const std::vector<long long> & fieldIdentifiers) const;

    boost::shared_ptr<Data> cutGrid(const boost::shared_ptr<Data> & d, const CDMVariable& variable, const SliceBuilder & sb) const;

    boost::shared_ptr<Data> getDatabaseFields(const CDMVariable& variable, size_t unLimDimPos) const;
    boost::shared_ptr<Data> getDatabaseFields(const CDMVariable& variable, const SliceBuilder & sb) const;

    class InternalData;
    InternalData * d_;
};


}

#endif /* WDBCDMREADER_H_ */
