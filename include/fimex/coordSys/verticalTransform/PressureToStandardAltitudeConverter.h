#ifndef PRESSURETOSTANDARDALTITUDECONVERTER_H
#define PRESSURETOSTANDARDALTITUDECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Conversion from pressure to height above MSL (i.e. altitude) using the international standard atmosphere.
 * The pressure levels are initialized by a previous pressure-conversion.
 */
class PressureToStandardAltitudeConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure);

    /**
     * @param pressure another ToVLevelConverter converting to pressure
     */
    PressureToStandardAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr pressure);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverterPtr pressure_;
};

} // namespace MetNoFimex

#endif // PRESSURETOSTANDARDALTITUDECONVERTER_H
