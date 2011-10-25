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

namespace MetNoFimex
{

// forward decl.
class CDMReader;

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
     * @return one of the MIFI_FILETYPE_* flags
     *
     * @throw if file not found
     */
    static int detectFileType(const std::string& fileName);

    /**
     * @brief Factory for CDMReader of input-files
     *
     * The function create reader tries to create a reader by filetype MIFI_FILETYPE_*.
     * The optional arguments are defined by the different readers. Use default objects (empty string, empty vector)
     * if arguments are not desired.
     *
     * @param fileNype, one of MIFI_FILETYPE_*, possibly read by detectFileType()
     * @param fileName, name of input type
     * @param configFile
     * @param options optional options for the CDMReader
     * @return pointer to CDMReader
     * @throws CDMException if type not compiled in, or creation fails
     * @deprecated use create(int fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>())
     */
    static boost::shared_ptr<CDMReader> create(int fileType, const std::string& fileName, const std::string& configFile = "", const std::vector<std::string>& args = std::vector<std::string>());
    /**
     * @brief same as the other create(), but with a fileType string
     * @deprecated use create(const std::string& fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>())
     */
    static boost::shared_ptr<CDMReader> create(const std::string& fileType, const std::string& fileName, const std::string& configFile = "", const std::vector<std::string>& args = std::vector<std::string>());
    /**
     * @brief Factory for CDMReader of input-files
     *
     * The function create reader tries to create a reader by filetype MIFI_FILETYPE_*.
     * The optional arguments are defined by the different readers. Use default objects (empty string, empty vector)
     * if arguments are not desired.
     *
     * @param fileNype, one of MIFI_FILETYPE_*, possibly read by detectFileType()
     * @param fileName, name of input type
     * @param configXML config source
     * @param options optional options for the CDMReader
     * @return pointer to CDMReader
     * @throws CDMException if type not compiled in, or creation fails
     * @deprecated use create(int fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>())
     */
    static boost::shared_ptr<CDMReader> create(int fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>());
    /**
     * @brief same as the other create(), but with a fileType string
     */
    static boost::shared_ptr<CDMReader> create(const std::string& fileType, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>& args = std::vector<std::string>());


};

}


#endif /* CDMFILEREADERFACTORY_H_ */
