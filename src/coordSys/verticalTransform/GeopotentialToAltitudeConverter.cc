#include "fimex/coordSys/verticalTransform/GeopotentialToAltitudeConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

// static method
boost::shared_ptr<VerticalConverter> GeopotentialToAltitudeConverter::createConverter(CDMReaderPtr reader, CoordSysPtr cs)
{
    const std::string geopotential_height = findVariableWithCS(reader->getCDM(), cs, "geopotential_height");
    if (geopotential_height.empty())
        return VerticalConverterPtr();
    return boost::make_shared<GeopotentialToAltitudeConverter>(reader, cs, geopotential_height);
}

std::vector<std::string> GeopotentialToAltitudeConverter::getShape() const
{
    return reader_->getCDM().getVariable(geopotential_height_).getShape();
}

DataPtr GeopotentialToAltitudeConverter::getDataSlice(const SliceBuilder& sb) const
{
    return reader_->getScaledDataSliceInUnit(geopotential_height_, "m", sb);
}

} // namespace MetNoFimex
