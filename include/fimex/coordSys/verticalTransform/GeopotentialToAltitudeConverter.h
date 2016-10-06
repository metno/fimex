#ifndef GEOPOTENTIALTOALTITUDECONVERTER_H
#define GEOPOTENTIALTOALTITUDECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Use geopotential height (in m) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class GeopotentialToAltitudeConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReaderPtr reader, CoordSysPtr cs);

    GeopotentialToAltitudeConverter(CDMReaderPtr reader, CoordSysPtr cs, const std::string& geopotential_height)
        : BasicVerticalConverter(reader, cs), geopotential_height_(geopotential_height) { }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string geopotential_height_;
};

} // namespace MetNoFimex

#endif // GEOPOTENTIALTOALTITUDECONVERTER_H
