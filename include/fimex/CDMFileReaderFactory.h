/*
 * Fimex, CDMFileReaderFactory.h
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

#ifndef CDMFILEREADERFACTORY_H_
#define CDMFILEREADERFACTORY_H_

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "fimex/XMLInput.h"
#include "fimex/deprecated.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMReaderDecl.h"

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMFileReaderFactory.h
 */
/**
 * helper class to simplify file-reader detection and creation
 */
class CDMFileReaderFactory
{
public:
    /**
     * @brief detect the filetype of a input-file
     *
     * The detectFileType function uses heuristics (appendix, magic characters)
     * to detect the filetype
     *
     * @param fileName input file
     * @return one of the #mifi_filetype flags
     *
     * @throw if file not found
     */
    static mifi_filetype detectFileType(const std::string& fileName);

    /**
     * @brief Factory for CDMReader of input-files
     *
     * This function tries to create a reader by filetype #mifi_filetype.
     * The optional arguments are defined by the different readers. Use default objects (empty string, empty vector)
     * if arguments are not desired.
     *
     * @param fileType one of #mifi_filetype, possibly read by detectFileType(). To get a CDMReaderWriter, use MIFI_FILETYPE_NETCDF|MIFI_FILETYPE_RW.
     * @param fileName name of input file (might start with glob: for grib or netcdf, e.g. glob:*.nc (joinExisting aggregation, see NcmlCDMReader.h)
     * @param configFile
     * @param args optional options for the CDMReader, e.g. for grib: additional message files
     * @return pointer to CDMReader
     * @throws CDMException if type not compiled in, or creation fails
     * @deprecated use create(int fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>())
     */
    static CDMReader_p create(int fileType, const std::string& fileName, const std::string& configFile = "", const std::vector<std::string>& args = std::vector<std::string>());
    /**
     * @brief same as the other create(), but with a fileType string
     * @deprecated use create(const std::string& fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>())
     */
    static CDMReader_p create(const std::string& fileType, const std::string& fileName, const std::string& configFile = "", const std::vector<std::string>& args = std::vector<std::string>());
    /**
     * @brief Factory for CDMReader of input-files
     *
     * This function tries to create a reader by filetype #mifi_filetype.
     * The optional arguments are defined by the different readers. Use default objects (empty string, empty vector)
     * if arguments are not desired.
     *
     * @param fileType one of #mifi_filetype, possibly read by detectFileType(). To get a CDMReaderWriter, use MIFI_FILETYPE_NETCDF|MIFI_FILETYPE_RW
     * @param fileName name of input file (might start with glob: for grib or netcdf, e.g. glob:*.nc (joinExisting aggregation, see NcmlCDMReader.h)
     * @param configXML config source
     * @param args optional options for the CDMReader
     * @return pointer to CDMReader
     * @throws CDMException if type not compiled in, or creation fails
     * @deprecated use create(int fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>())
     */
    static CDMReader_p create(int fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>());
    /**
     * @brief same as the other create(), but with a fileType string
     */
    static CDMReader_p create(const std::string& fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>());

    /**
     * @brief parse special arguments for grib-files
     *
     * @param args list of arguments, e.g. combined --input.file --input.optional from fimex command-line
     * @param members return members as memberName/memberRegexp pairs
     * @param files retrun all input files
     */
    static void parseGribArgs(const std::vector<std::string> & args, std::vector<std::pair<std::string, std::string> >& members, std::vector<std::string>& files);

    static CDMReaderWriter_p createReaderWriter(int fileType, const std::string & fileName, const XMLInput& configXML,
                                                const std::vector<std::string> & args = std::vector<std::string>());

    static CDMReaderWriter_p createReaderWriter(int fileType, const std::string & fileName, const std::string& configFile = std::string(),
                                                const std::vector<std::string> & args = std::vector<std::string>());
};

bool isFeltType(const std::string& type);
bool isGrib2Type(const std::string& type);
bool isGribType(const std::string& type);
bool isNetCDF4Type(const std::string& type);
bool isNetCDFType(const std::string& type);

void createWriter(CDMReader_p input, const std::string& fileType, const std::string& fileName,
                  const std::string& configFile);
}

#endif /* CDMFILEREADERFACTORY_H_ */
