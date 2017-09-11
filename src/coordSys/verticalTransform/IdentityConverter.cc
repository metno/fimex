#include "fimex/coordSys/verticalTransform/IdentityConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Logger.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.IdentityConverter");

// static method
VerticalConverterPtr IdentityConverter::createConverterForStandardName(CDMReader_p reader, CoordSysPtr cs, const std::string& stdName, const std::string& unit)
{
    return createConverterForVarName(reader, cs, findVariableWithCS(reader->getCDM(), cs, stdName), unit);
}

// static method
VerticalConverterPtr IdentityConverter::createConverterForVarName(CDMReader_p reader, CoordSysPtr cs, const std::string& varName, const std::string& unit)
{
    if (!varName.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "createConverterForVarName '" << varName << "'");
        return boost::make_shared<IdentityConverter>(reader, cs, varName, unit);
    }
    return VerticalConverterPtr();
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
