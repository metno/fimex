
#include "fimex/coordSys/verticalTransform/PressureToStandardAltitudeConverter.h"

#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.PressureToStandardAltitudeConverter");

// static method
VerticalConverterPtr PressureToStandardAltitudeConverter::createConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure)
{
    LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate altitude levels");
    return boost::make_shared<PressureToStandardAltitudeConverter>(reader, cs, pressure);
}

PressureToStandardAltitudeConverter::PressureToStandardAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure)
    : BasicVerticalConverter(reader, cs)
    , pressure_(pressure)
{
}

std::vector<std::string> PressureToStandardAltitudeConverter::getShape() const
{
    return pressure_->getShape();
}

DataPtr PressureToStandardAltitudeConverter::getDataSlice(const SliceBuilder& sb) const
{
    DataPtr pressureData = pressure_->getDataSlice(sb);
    boost::shared_array<double> pVal = pressureData->asDouble();
    const size_t size = pressureData->size();
    boost::shared_array<double> altiVal(new double[size]);
    mifi_barometric_standard_altitude(size, pVal.get(), altiVal.get());
    return createData(size, altiVal);
}

} // namespace MetNoFimex
