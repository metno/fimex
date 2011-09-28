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
#include "fimex/NcmlCDMReader.h"
#ifdef HAVE_FELT
#include "fimex/FeltCDMReader2.h"
#endif
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMReader.h"
#endif
#ifdef HAVE_GRIB_API_H
#include "fimex/GribCDMReader.h"
#endif
#ifdef HAVE_LIBPQ
#include "fimex/WdbCDMReader.h"
#endif
#ifdef HAVE_METGM_H
#include "fimex/MetGmCDMReader.h"
#endif


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

int CDMFileReaderFactory::detectFileType(const std::string & fileName)
{
    std::ifstream fs(fileName.c_str());
    if (!fs.is_open()) {
        throw CDMException("cannot open file "+fileName);
    }
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
        return MIFI_FILETYPE_NCML;
    }

    // detection by appendix
    boost::smatch what;
    if (boost::regex_match(fileName, what, boost::regex(".*\\.(\\w+)$"))) {
        std::string type = what[1].str();
        std::transform(type.begin(), type.end(), type.begin(), (int(*)(int)) tolower);

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
    }
    return MIFI_FILETYPE_UNKNOWN;
}



boost::shared_ptr<CDMReader> CDMFileReaderFactory::create(int fileType, const std::string & fileName, const std::string & configFile, const std::vector<std::string> & args)
{
    switch (fileType) {
#ifdef HAVE_FELT
    case MIFI_FILETYPE_FELT:
        return boost::shared_ptr<CDMReader>(new FeltCDMReader2(fileName, configFile));
#endif /* FELT */
#ifdef HAVE_GRIB_API_H
    case MIFI_FILETYPE_GRIB: {
        std::vector<std::string> files(args.begin(), args.end());
        files.insert(files.begin(), fileName);
        return boost::shared_ptr<CDMReader>(new GribCDMReader(files, configFile));
    }
#endif
#ifdef HAVE_NETCDF_H
    case MIFI_FILETYPE_NETCDF: {
        boost::shared_ptr<CDMReader> reader(new NetCDF_CDMReader(fileName));
        if (configFile != "") {
            reader = boost::shared_ptr<CDMReader>(new NcmlCDMReader(reader, configFile));
        }
        return reader;
    }
#endif
#ifdef HAVE_METGM_H
    case MIFI_FILETYPE_METGM: {
        return boost::shared_ptr<CDMReader>(new MetGmCDMReader(fileName, configFile));
    }
#endif
#ifdef HAVE_LIBPQ
    case MIFI_FILETYPE_WDB: {
        return boost::shared_ptr<CDMReader>(new WdbCDMReader(fileName, configFile));
    }
#endif
    case MIFI_FILETYPE_NCML:
        return boost::shared_ptr<CDMReader>(new NcmlCDMReader(fileName));
    default: throw CDMException("Unknown fileType: " + type2string(fileType) + " for file: "+fileName);
    }
}

boost::shared_ptr<CDMReader> CDMFileReaderFactory::create(const std::string& fileTypeName, const std::string & fileName, const std::string & configFile, const std::vector<std::string> & args)
{
    int fileType = mifi_get_filetype(fileTypeName.c_str());
    return create(fileType, fileName, configFile, args);
}

}
