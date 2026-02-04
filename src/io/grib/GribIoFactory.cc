/*
  Fimex, src/GribIoFactory.cc

  Copyright (C) 2019-2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#include "GribIoFactory.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "GribApiCDMWriter.h"
#include "GribCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "fimex/FileUtils.h"
#include "fimex/IoPlugin.h"
#include "fimex/StringUtils.h"

#include <cstring>

namespace MetNoFimex {

namespace {

const char GRBML[] = "grbml";

bool isGrib2Type(const std::string& type)
{
    return (type == "grb2" || type == "grib2");
}

bool isGribType(const std::string& type)
{
    return (type == "grb" || type == "grib" || type == "grb1" || type == "grib1" || isGrib2Type(type));
}

} // namespace

void parseGribArgs(const std::vector<std::string>& args, std::vector<std::pair<std::string, std::string>>& members, std::vector<std::string>& files)
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
            members.push_back(std::make_pair(memberNameParts.at(1), ".*" + regex_escape(memberNameParts.at(0)) + ".*"));
        } else {
            // additional file
            files.push_back(*argIt);
        }
    }
}

size_t GribIoFactory::matchMagicSize()
{
    return 4;
}

int GribIoFactory::matchMagic(const char* magic, size_t count)
{
    if (count >= 4 && strncmp(magic, "GRIB", 4) == 0)
        return 1;
    // TODO check for GRBML
    return 0;
}

int GribIoFactory::matchFileTypeName(const std::string& type)
{
    if (type == GRBML) {
        // actually correct only for reading
        return 1;
    }
    return isGribType(type) ? 1 : 0;
}

CDMReader_p GribIoFactory::createReader(const std::string& fileTypeName, const std::string& fileName, const XMLInput& configXML,
                                        const std::vector<std::string>& args)
{
    if (fileTypeName == GRBML || getExtension(fileName) == GRBML) {
        std::vector<std::pair<std::string, std::string>> members;
        std::vector<std::string> files; // files not used for grbml
        parseGribArgs(args, members, files);
        if (configXML.isEmpty()) {
            throw CDMException("config file required for grbml-files");
        }
        return std::make_shared<GribCDMReader>(fileName, configXML, members);
    } else {
        std::vector<std::string> files;
        // scanfiles by a glob
        std::string globStr("glob:");
        if (fileName.find(globStr) == 0) {
            std::string glob = fileName.substr(globStr.size());
            globFiles(files, glob);
        } else {
            files.push_back(fileName);
        }
        std::vector<std::pair<std::string, std::string>> members;
        parseGribArgs(args, members, files);
        if (configXML.isEmpty()) {
            throw CDMException("config file required for grib-files");
        }
        return std::make_shared<GribCDMReader>(files, configXML, members);
    }
}

void GribIoFactory::createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& config)
{
    if (fileTypeName == GRBML || getExtension(fileName) == GRBML)
        throw CDMException("cannot write grbml-files");

    int gribVersion = 0;
    const std::string ext = getExtension(fileName);
    if (isGrib2Type(fileTypeName) || isGrib2Type(ext))
        gribVersion = 2;
    else if (isGribType(fileTypeName) || isGribType(ext))
        gribVersion = 1;
    else
        IoFactory::createWriter(input, fileTypeName, fileName, config);

    GribApiCDMWriter(input, fileName, gribVersion, config);
}

} // namespace MetNoFimex

DEFINE_IO_PLUGIN("grib-api", MetNoFimex::GribIoFactory)
