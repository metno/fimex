#ifndef PRESSUREINTEGRATIONTOALTITUDECONVERTER_H
#define PRESSUREINTEGRATIONTOALTITUDECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Conversion from pressure to height above MSL (i.e. altitude) integrating pressure levels using the hypsometric equation.
 * The pressure levels are initialized by a previous pressure-conversion.
 */
class PressureIntegrationToAltitudeConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure);

    /**
     * @param presConv another ToVLevelConverter converting to pressure
     */
    PressureIntegrationToAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure,
        const std::string& air_temperature, const std::string& specific_humidity,
        const std::string& surface_air_pressure, const std::string& surface_geopotential);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverterPtr pressure_;
    std::string air_temperature_;
    std::string specific_humidity_;
    std::string surface_air_pressure_;
    std::string surface_geopotential_;
};

} // namespace MetNoFimex

#endif // PRESSUREINTEGRATIONTOALTITUDECONVERTER_H
