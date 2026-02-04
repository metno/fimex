/*
 * Fimex, CDMFileReaderFactory.h
 *
 * (C) Copyright 2010-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/CDMReaderDecl.h"

#include <string>
#include <vector>

namespace MetNoFimex
{
class XMLInput;

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
     * @brief Factory for CDMReader of input-files
     *
     * This function tries to create a reader by filetype #mifi_filetype.
     * The optional arguments are defined by the different readers. Use default objects (empty string, empty vector)
     * if arguments are not desired.
     *
     * @param fileTypeName a string describing the filetype
     * @param fileName name of input file (might start with glob: for grib or netcdf, e.g. glob:*.nc (joinExisting aggregation, see NcmlCDMReader.h)
     * @param configFile
     * @param args optional options for the CDMReader, e.g. for grib: additional message files
     * @return pointer to CDMReader
     * @throws CDMException if type not compiled in, or creation fails
     */
    static CDMReader_p create(const std::string& fileTypeName, const std::string& fileName, const std::string& configFile = "",
                              const std::vector<std::string>& args = std::vector<std::string>());

    /**
     * @brief Factory for CDMReader of input-files
     *
     * This function tries to create a reader by filetype #mifi_filetype.
     * The optional arguments are defined by the different readers. Use default objects (empty string, empty vector)
     * if arguments are not desired.
     *
     * @param fileTypeName a string describing the filetype
     * @param fileName name of input file (might start with glob: for grib or netcdf, e.g. glob:*.nc (joinExisting aggregation, see NcmlCDMReader.h)
     * @param configXML config source
     * @param args optional options for the CDMReader
     * @return pointer to CDMReader
     * @throws CDMException if type not compiled in, or creation fails
     */
    static CDMReader_p create(const std::string& fileTypeName, const std::string& fileName, const XMLInput& configXML,
                              const std::vector<std::string>& args = std::vector<std::string>());

    static CDMReaderWriter_p createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const std::string& configFile = std::string(),
                                                const std::vector<std::string>& args = std::vector<std::string>());

    static CDMReaderWriter_p createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                                const std::vector<std::string>& args = std::vector<std::string>());

    static void createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& config);
    static void createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const std::string& configFile = std::string());
};

inline void createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const std::string& configFile = std::string())
{
    CDMFileReaderFactory::createWriter(input, fileTypeName, fileName, configFile);
}

void parseGribArgs(const std::vector<std::string>& args, std::vector<std::pair<std::string, std::string>>& members, std::vector<std::string>& files);

} // namespace MetNoFimex

#endif /* CDMFILEREADERFACTORY_H_ */
