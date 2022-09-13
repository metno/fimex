/*
  Fimex, src/FeltIoFactory.cc

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

#include "FeltIoFactory.h"

#include "fimex/IoPlugin.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "FeltCDMReader2.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/XMLInputFile.h"

#include "fimex_felt_config.h"

namespace MetNoFimex {

namespace {

bool isFeltType(const std::string& type)
{
    return (type == "flt" || type == "dat" || type == "felt" || type == "flt2" || type == "dat2" || type == "felt2");
}

const std::string FELT_VARIABLES = (std::string(FIMEX_DATADIR) + "/felt2nc_variables.xml");

} // namespace

int FeltIoFactory::matchFileTypeName(const std::string& type)
{
    return isFeltType(type) ? 1 : 0;
}

CDMReader_p FeltIoFactory::createReader(const std::string&, const std::string& fileName, const XMLInput& configXML, const std::vector<std::string>&)
{
    if (!configXML.isEmpty()) {
        return std::make_shared<FeltCDMReader2>(fileName, configXML);
    } else {
        XMLInputFile configDefault(FELT_VARIABLES);
        return std::make_shared<FeltCDMReader2>(fileName, configDefault);
    }
}

} // namespace MetNoFimex

DEFINE_IO_PLUGIN("felt", MetNoFimex::FeltIoFactory)
