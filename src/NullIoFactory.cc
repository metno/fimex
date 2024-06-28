/*
  Fimex, src/NullIoFactory.cc

  Copyright (C) 2019-2024 met.no

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

#include "NullIoFactory.h"

#include "fimex/CDMException.h"
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/Null_CDMWriter.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

namespace MetNoFimex {

static const bool installed = IoFactory::install("null", std::make_shared<NullIoFactory>());

int NullIoFactory::matchFileName(const std::string&)
{
    return 0;
}

int NullIoFactory::matchFileTypeName(const std::string& type)
{
    return (type == "null") ? 1 : 0;
}

CDMReader_p NullIoFactory::createReader(const std::string&, const std::string&, const XMLInput&, const std::vector<std::string>&)
{
    throw CDMException("unable to create reader for 'null' file type");
}

void NullIoFactory::createWriter(CDMReader_p input, const std::string&, const std::string& fileName, const XMLInput&)
{
    Null_CDMWriter(input, fileName);
}

} // namespace MetNoFimex
