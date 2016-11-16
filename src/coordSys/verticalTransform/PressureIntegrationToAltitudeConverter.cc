#include "fimex/coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/interpolation.h"
#include "fimex/Logger.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.PressureIntegrationToAltitudeConverter");

// static method
VerticalConverterPtr PressureIntegrationToAltitudeConverter::createConverter(CDMReaderPtr reader,
        CoordSysPtr cs, VerticalConverterPtr pressure)
{
    const CDM& rcdm = reader->getCDM();

    // air_temperature and/or pressureConverter define shape
    const std::string air_temperature = findVariableWithCS(rcdm, cs, "air_temperature");
    if (air_temperature.empty())
        return VerticalConverterPtr();

    const std::string specific_humidity = findVariableWithCS(rcdm, cs, "specific_humidity"); // no problem if empty
    std::vector<std::string> surface_shape(reader->getCDM().getVariable(air_temperature).getShape());
    erase_value(surface_shape, cs->getGeoZAxis()->getName());
    removeDimsWithLength1(rcdm, surface_shape);

    const std::string surface_air_pressure = findVariableWithDims(rcdm, "surface_air_pressure", surface_shape);

    erase_value(surface_shape, cs->getTimeAxis()->getName());
    const std::string surface_geopotential = findVariableWithDims(rcdm, "surface_geopotential", surface_shape);

    if (surface_air_pressure.empty() || surface_geopotential.empty())
        return VerticalConverterPtr();

    return boost::make_shared<PressureIntegrationToAltitudeConverter>(reader, cs, pressure,
        air_temperature, specific_humidity, surface_air_pressure, surface_geopotential);
}


PressureIntegrationToAltitudeConverter::PressureIntegrationToAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure,
    const std::string& air_temperature, const std::string& specific_humidity, const std::string& surface_air_pressure, const std::string& surface_geopotential)
    : BasicVerticalConverter(reader, cs)
    , pressure_(pressure)
    , air_temperature_(air_temperature)
    , specific_humidity_(specific_humidity)
    , surface_air_pressure_(surface_air_pressure)
    , surface_geopotential_(surface_geopotential)
{
    LOG4FIMEX(logger, Logger::INFO, "using hypsometric equation with surface pressure '"
            << surface_air_pressure_ << "', surface geopotential '" << surface_geopotential_
            << "', air_temperature '" << air_temperature_ << "' to retrieve altitude");
    if (!specific_humidity_.empty())
        LOG4FIMEX(logger, Logger::INFO, "hypsometric equation uses virtual "
            "temperature with specific humidity '" << specific_humidity_ << "'");
}

std::vector<std::string> PressureIntegrationToAltitudeConverter::getShape() const
{
    return pressure_->getShape();
}

DataPtr PressureIntegrationToAltitudeConverter::getDataSlice(const SliceBuilder& sb) const
{
    DataPtr sapData = getSliceData(reader_, sb, surface_air_pressure_, "hPa");
    DataPtr sgpData = getSliceData(reader_, sb, surface_geopotential_, "m^2/s^2");
    DataPtr airtData = getSliceData(reader_, sb, air_temperature_, "K");
    DataPtr pressureData = pressure_->getDataSlice(sb);

    if (!sapData || !sgpData || !airtData || !pressureData) {
        LOG4FIMEX(logger, Logger::INFO, "hypsometric no data");
        return DataPtr();
    }

    const size_t nx = getSliceSize(sb, cs_->getGeoXAxis()->getName());
    const size_t ny = getSliceSize(sb, cs_->getGeoYAxis()->getName());
    const size_t nl = getSliceSize(sb, cs_->getGeoZAxis()->getName());
    const size_t sizeXY = nx*ny, sizeXYZ = sizeXY*nl;
    const size_t size = pressureData->size(), sizeSurface = size / nl;
    const size_t sizeOther = size / sizeXYZ;

    checkSize(sapData, sizeSurface, "surface air pressure");
    checkSize(sgpData, sizeXY, "surface geopotential");
    checkSize(airtData, size, "air temperature");

    boost::shared_array<float> sapVal = sapData->asFloat();
    boost::shared_array<float> sgpVal = sgpData->asFloat();
    boost::shared_array<float> airtVal = airtData->asFloat();
    boost::shared_array<float> pVal = pressureData->asFloat();
    boost::shared_array<float> shVal;
    if (!specific_humidity_.empty()) {
        if (DataPtr shData = checkSize(getSliceData(reader_, sb, specific_humidity_, "1"), size, "specific humidity"))
            shVal = shData->asFloat();
    }

    size_t l0 = 0, l1 = nl-1, dl = 1;
    if (pVal[0] < pVal[sizeXY*(nl-1)]) { // FIXME this assumes x,y as first dimensions
        std::swap(l0, l1);
        dl = -1;
    }
    l1 += dl;

    boost::shared_array<float> altiVal(new float[size]);

    // loop over all dims but z, integrate pressure -> altitude
    // FIXME the following code assumes that x,y,z are the lowest dimensions for all variables
    size_t offset = 0, offsetSurface = 0;
    for (size_t i=0; i<sizeOther; ++i, offset += sizeXYZ, offsetSurface += sizeXY) {
        for (size_t xy=0; xy < sizeXY; ++xy) {
            const size_t idxSurface = offsetSurface + xy;
            const float p_surf = sapVal[idxSurface];
            double a = sgpVal[xy] / MIFI_EARTH_GRAVITY;

            for (size_t l = l0; l != l1; l += dl) {
                const size_t idx = offset + xy + l*sizeXY;
                const float p_low_alti = (l == l0) ? p_surf : pVal[idx - dl*sizeXY];
                const float p_high_alti = pVal[idx];

                float Tv = airtVal[idx];
                if (shVal)
                    Tv = mifi_virtual_temperature(shVal[idx], Tv);

                a += mifi_barometric_layer_thickness(p_low_alti, p_high_alti, Tv);
                altiVal[idx] = a;
            }
        }
    }
    return createData(size, altiVal);
}

} // namespace MetNoFimex
