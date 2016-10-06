#ifndef ALTITUDEHEIGHTCONVERTER_H
#define ALTITUDEHEIGHTCONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Use altitude (height above MSL) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class AltitudeHeightConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr altitudeOrHeight, bool addTopography);

    AltitudeHeightConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr altitudeOrHeight, const std::string& topography, bool addTopography);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverterPtr altitudeOrHeight_;
    std::string topography_;
    std::string topographyUnit_;
    int topographyFactor_;
};

} // namespace MetNoFimex

#endif // ALTITUDEHEIGHTCONVERTER_H
