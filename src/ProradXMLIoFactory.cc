/*
  Fimex, src/ProradXMLIoFactory.cc

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


#include "ProradXMLIoFactory.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "ProradXMLCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

namespace MetNoFimex {

static const bool installed = IoFactory::install("proradxml", std::make_shared<ProradXMLIoFactory>());

int ProradXMLIoFactory::matchFileName(const std::string&)
{
    return 0;
}

int ProradXMLIoFactory::matchFileTypeName(const std::string& type)
{
    return (type == "prorad") ? 1 : 0;
}

CDMReader_p ProradXMLIoFactory::createReader(const std::string&, const std::string& fileName, const XMLInput&, const std::vector<std::string>&)
{
    return std::make_shared<ProradXMLCDMReader>(fileName);
}

} // namespace MetNoFimex
