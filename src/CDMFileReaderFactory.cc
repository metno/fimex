/*
 * Fimex, CDMFileReaderFactory.cc
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: May 5, 2010
 *      Author: Heiko Klein
 */

#include "fimex/CDMFileReaderFactory.h"
#include <iostream>
#include <fstream>
#include <boost/regex.hpp>
#include <cctype>
#include <algorithm>
#include "../config.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMException.h"
#include "fimex/Utils.h"
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/NcmlCDMReader.h"
#ifdef HAVE_FELT
#include "FeltCDMReader2.h"
#endif
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMReader.h"
#endif
#ifdef HAVE_GRIB_API_H
#include "fimex/GribCDMReader.h"
#endif
#ifdef HAVE_LIBPQ_FE_H
#include "fimex/WdbCDMReader.h"
#endif
#ifdef HAVE_METGM_H
#include "fimex/MetGmCDMReader.h"
#endif
#ifdef HAVE_PRORADXML
#include "ProradXMLCDMReader.h"
#endif
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED


namespace MetNoFimex {

#define MIFI_MAGIC_SIZE 10
static bool detectHDF5(const char* magic) {
    const char* hdf5 = "\211HDF\r\n\032";
    for (int i = 0; i < 7; i++) {
        if (hdf5[i] != magic[i]) return false;
    }
    return true;
}

static bool detectNetCDF(const char* magic) {
    const char* netcdf = "CDF\001";
    for (int i = 0; i < 4; i++) {
        if (netcdf[i] != magic[i]) return false;
    }
    return true;
}

static bool detectXML(const char* magic) {
    return boost::regex_match(magic, boost::regex("\\s*<\\?xml\\s.*"));
}

mifi_filetype CDMFileReaderFactory::detectFileType(const std::string & fileName)
{
    // get appendix
    std::string type("");
    boost::smatch what;
    if (boost::regex_match(fileName, what, boost::regex(".*\\.(\\w+)$"))) {
        type = what[1].str();
        std::transform(type.begin(), type.end(), type.begin(), (int(*)(int)) tolower);
    }

    std::ifstream fs(fileName.c_str());
    if (!fs.is_open()) {
        throw CDMException("cannot open file "+fileName);
    }

    // magic
    char magic[MIFI_MAGIC_SIZE];
    for (int i = 0; i < MIFI_MAGIC_SIZE; ++i) {
        magic[i] = 0;
    }
    for (int i = 0; i < MIFI_MAGIC_SIZE; ++i) {
        if (fs.good()) fs >> magic[i];
    }

    // detection by magic
    if (detectHDF5(magic) || detectNetCDF(magic)) {
        return MIFI_FILETYPE_NETCDF;
    }
    if (detectXML(magic)) {
        if (type != "grbml") { // avoid conflict between grbml and ncml xml-files
            return MIFI_FILETYPE_NCML;
        }
    }

    // detection by appendix
    if (type != "") {
        if (type == "flt" || type == "dat" || type == "felt" || type == "flt2" || type == "dat2" || type == "felt2")
            return MIFI_FILETYPE_FELT;
        if (type == "nc" || type == "cdf" || type == "netcdf" || type == "nc4")
            return MIFI_FILETYPE_NETCDF;
        if (type == "ncml")
            return MIFI_FILETYPE_NCML;
        if (type == "grb" || type == "grib" ||
                type == "grb1" || type == "grib1" ||
                    type == "grb2" || type == "grib2")
            return MIFI_FILETYPE_GRIB;
        if (type == "metgm")
            return MIFI_FILETYPE_METGM;
        if (type == "prorad")
            return MIFI_FILETYPE_PRORAD;
        if (type == "grbml")
            return MIFI_FILETYPE_GRBML;
    }
    return MIFI_FILETYPE_UNKNOWN;
}

boost::shared_ptr<CDMReader> CDMFileReaderFactory::create(int fileType, const std::string & fileName, const std::string & configFile, const std::vector<std::string> & args)
{
    XMLInputFile configXML(configFile);
    return create(fileType, fileName, configXML, args);
}

void CDMFileReaderFactory::parseGribArgs(const std::vector<std::string> & args, std::vector<std::pair<std::string, std::string> >& members, std::vector<std::string>& files)
{
    for (std::vector<std::string>::const_iterator argIt = args.begin(); argIt != args.end(); ++argIt) {
        std::string memberRegex("memberRegex:");
        std::string memberName("memberName:");
        if (argIt->find(memberRegex) == 0) {
            std::vector<std::string> memberRegexParts = tokenize(argIt->substr(memberRegex.size()), ":");
            if (memberRegexParts.size() == 1) {
                memberRegexParts.push_back(memberRegexParts.at(0));
            }
            members.push_back(std::make_pair(memberRegexParts.at(1), memberRegexParts.at(0)));
        } else if (argIt->find(memberName) == 0) {
            std::vector<std::string> memberNameParts = tokenize(argIt->substr(memberName.size()), ":");
            if (memberNameParts.size() == 1) {
                memberNameParts.push_back(memberNameParts.at(0));
            }
            members.push_back(std::make_pair(memberNameParts.at(1), ".*\\Q" + memberNameParts.at(0) + "\\E.*"));
        } else {
            // additional file
            files.push_back(*argIt);
        }
    }
}

boost::shared_ptr<CDMReader> CDMFileReaderFactory::create(int fileType, const std::string & fileName, const XMLInput& configXML, const std::vector<std::string> & args)
{
    switch (fileType) {
#ifdef HAVE_FELT
    case MIFI_FILETYPE_FELT:
        if (configXML.isEmpty()) {
            throw CDMException("config file required for felt-files");
        }
        return boost::shared_ptr<CDMReader>(new FeltCDMReader2(fileName, configXML));
#endif /* FELT */
#ifdef HAVE_GRIB_API_H
    case MIFI_FILETYPE_GRBML: {
        std::vector<std::pair<std::string, std::string> > members;
        std::vector<std::string> files; // files not used for grbml
        parseGribArgs(args, members, files);
        if (configXML.isEmpty()) {
            throw CDMException("config file required for grbml-files");
        }
        return boost::shared_ptr<CDMReader>(new GribCDMReader(fileName, configXML, members));
    }
    case MIFI_FILETYPE_GRIB: {
        std::vector<std::string> files;
        // scanfiles by a glob
        std::string globStr("glob:");
        if (fileName.find(globStr) == 0) {
            std::string glob = fileName.substr(globStr.size());
            globFiles(files, glob);
        } else {
            files.push_back(fileName);
        }
        std::vector<std::pair<std::string, std::string> > members;
        parseGribArgs(args, members, files);
        if (configXML.isEmpty()) {
            throw CDMException("config file required for grib-files");
        }
        return boost::shared_ptr<CDMReader>(new GribCDMReader(files, configXML, members));
    }
#endif
#ifdef HAVE_NETCDF_H
    case MIFI_FILETYPE_NETCDF: {
        boost::shared_ptr<CDMReader> reader;
        // scanfiles by a glob
        std::string globStr("glob:");
        if (fileName.find(globStr) == 0) { // starts with glob:
            std::string glob = fileName.substr(globStr.size());
            std::vector<std::string> files;
            globFiles(files, glob);
            std::ostringstream ncml;
            ncml << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << std::endl
                 << "<netcdf xmlns=\"http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2\">"
                 << "<aggregation type=\"joinExisting\">";
            for (size_t i = 0; i < files.size(); ++i) {
                ncml << "<netcdf location=\"" << files.at(i) << "\" />";
            }
            ncml << "</aggregation></netcdf>";
            reader = boost::shared_ptr<CDMReader>(new NcmlCDMReader(XMLInputString(ncml.str())));
        } else {
            reader = boost::shared_ptr<CDMReader>(new NetCDF_CDMReader(fileName, false));
        }
        if (!configXML.isEmpty()) {
            reader = boost::shared_ptr<CDMReader>(new NcmlCDMReader(reader, configXML));
        }
        return reader;
    }
    case (MIFI_FILETYPE_NETCDF|MIFI_FILETYPE_RW): {
        if (!configXML.isEmpty()) {
            throw CDMException("Cannot open writeable NetCDF file with Ncml config: " + configXML.id());
        }
        return boost::shared_ptr<CDMReader>(new NetCDF_CDMReader(fileName, true));
    }
#endif
#ifdef HAVE_METGM_H
    case MIFI_FILETYPE_METGM: {
        return boost::shared_ptr<CDMReader>(new MetGmCDMReader(fileName, configXML));
    }
#endif
#ifdef HAVE_PRORADXML
    case MIFI_FILETYPE_PRORAD: {
        return boost::shared_ptr<CDMReader>(new ProradXMLCDMReader(fileName));
    }
#endif
#ifdef HAVE_LIBPQ_FE_H
    case MIFI_FILETYPE_WDB: {
        return boost::shared_ptr<CDMReader>(new WdbCDMReader(fileName, configXML));
    }
#endif
    case MIFI_FILETYPE_NCML:
        return boost::shared_ptr<CDMReader>(new NcmlCDMReader(XMLInputFile(fileName)));
    default: throw CDMException("Unknown fileType: " + type2string(fileType) + " for file: "+fileName);
    }
}

boost::shared_ptr<CDMReader> CDMFileReaderFactory::create(const std::string& fileTypeName, const std::string & fileName, const std::string & configFile, const std::vector<std::string> & args)
{
    int fileType = mifi_get_filetype(fileTypeName.c_str());
    return create(fileType, fileName, configFile, args);
}

boost::shared_ptr<CDMReader> CDMFileReaderFactory::create(const std::string& fileTypeName, const std::string & fileName, const XMLInput& configXML, const std::vector<std::string> & args)
{
    int fileType = mifi_get_filetype(fileTypeName.c_str());
    return create(fileType, fileName, configXML, args);
}


}
