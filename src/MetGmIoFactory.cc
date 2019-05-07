/*
  Fimex, src/MetGmIoFactory.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

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


#include "MetGmIoFactory.h"

#include "fimex/FileUtils.h"
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/MetGmCDMReader.h"
#include "fimex/MetGmCDMWriter.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include <cstring>

namespace MetNoFimex {

static const bool installed = IoFactory::install("metgm", std::make_shared<MetGmIoFactory>());

size_t MetGmIoFactory::matchMagicSize()
{
    return 6;
}

int MetGmIoFactory::matchMagic(const char* magic, size_t count)
{
    if (count >= 5 && strncmp(magic, "METGM", 5) == 0)
        return 1;
    if (count >= 6 && strncmp(magic,
                              "\0x89"
                              "METGM",
                              6) == 0)
        return 1;
    return 0;
}

int MetGmIoFactory::matchFileTypeName(const std::string& type)
{
    return (type == "metgm") ? 1 : 0;
}

int MetGmIoFactory::matchFileName(const std::string& fileName)
{
    const std::string ext = getExtension(fileName);
    return (ext == "gm") ? 1 : 0;
}

CDMReader_p MetGmIoFactory::createReader(const std::string&, const std::string& fileName, const XMLInput& config, const std::vector<std::string>&)
{
    return std::make_shared<MetGmCDMReader>(fileName, config);
}

void MetGmIoFactory::createWriter(CDMReader_p input, const std::string&, const std::string& fileName, const std::string& configFile)
{
    MetGmCDMWriter(input, fileName, configFile);
}

} // namespace MetNoFimex
