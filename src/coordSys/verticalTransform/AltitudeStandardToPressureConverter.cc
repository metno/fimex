#include "fimex/coordSys/verticalTransform/AltitudeStandardToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.AltitudeStandardToPressureConverter");

// static method
VerticalConverterPtr AltitudeStandardToPressureConverter::createConverter(CDMReader_p reader, CoordSysPtr cs, VerticalConverterPtr altitude)
{
    LOG4FIMEX(logger, Logger::INFO, "using altitude and standard atmosphere to estimate pressure levels");
    return boost::make_shared<AltitudeStandardToPressureConverter>(reader, cs, altitude);
}

AltitudeStandardToPressureConverter::AltitudeStandardToPressureConverter(CDMReader_p reader, CoordSysPtr cs,
                                                                         const VerticalConverterPtr& altitude)
    : BasicVerticalConverter(reader, cs)
    , altitude_(altitude)
{
}

std::vector<std::string> AltitudeStandardToPressureConverter::getShape() const
{
    return altitude_->getShape();
}

DataPtr AltitudeStandardToPressureConverter::getDataSlice(const SliceBuilder &sb) const
{
    DataPtr altitudeData = altitude_->getDataSlice(sb);
    boost::shared_array<double> aVal = altitudeData->asDouble();
    const size_t size = altitudeData->size();
    boost::shared_array<double> pVal(new double[size]);
    mifi_barometric_standard_pressure(size, aVal.get(), pVal.get());
    return createData(size, pVal);
}

} // namespace MetNoFimex
