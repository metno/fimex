/*
  Fimex, src/NcmlIoFactory.cc

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

#include "NcmlIoFactory.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/NcmlCDMReader.h"
#include "fimex/NcmlCDMWriter.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/XMLInputFile.h"

namespace MetNoFimex {

const bool installed = IoFactory::install("ncml", std::make_shared<NcmlIoFactory>());

int NcmlIoFactory::matchFileTypeName(const std::string& type)
{
    return (type == "ncml") ? 1 : 0;
}

CDMReader_p NcmlIoFactory::createReader(const std::string&, const std::string& fileName, const XMLInput&, const std::vector<std::string>&)
{
    return std::make_shared<NcmlCDMReader>(XMLInputFile(fileName));
}

void NcmlIoFactory::createWriter(CDMReader_p input, const std::string&, const std::string& fileName, const XMLInput&)
{
    NcmlCDMWriter writer(input, fileName);
}

} // namespace MetNoFimex
