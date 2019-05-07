/*
  Fimex, src/coordSys/verticalTransform/IdentityConverter.cc

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

#include "fimex/coordSys/verticalTransform/IdentityConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Logger.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.IdentityConverter");

// static method
VerticalConverter_p IdentityConverter::createConverterForStandardName(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& stdName,
                                                                      const std::string& unit)
{
    return createConverterForVarName(reader, cs, findVariableWithCS(reader->getCDM(), cs, stdName), unit);
}

// static method
VerticalConverter_p IdentityConverter::createConverterForVarName(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& varName,
                                                                 const std::string& unit)
{
    if (!varName.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "createConverterForVarName '" << varName << "'");
        return boost::make_shared<IdentityConverter>(reader, cs, varName, unit);
    }
    return VerticalConverter_p();
}

std::vector<std::string> IdentityConverter::getShape() const
{
    return reader_->getCDM().getVariable(varName_).getShape();
}

DataPtr IdentityConverter::getDataSlice(const SliceBuilder& sb) const
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(" << sb << ")");
    return reader_->getScaledDataSliceInUnit(varName_, varUnit_, sb);
}

} // namespace MetNoFimex
