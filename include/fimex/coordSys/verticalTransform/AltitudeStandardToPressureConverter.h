#ifndef ALTITUDESTANDARDTOPRESSURECONVERTER_H
#define ALTITUDESTANDARDTOPRESSURECONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"

namespace MetNoFimex {

/**
 * using the international standard atmosphere to convert altitude to pressure
 */
class AltitudeStandardToPressureConverter : public BasicVerticalConverter {
public:
    /**
     * @param h given in m
     */
    AltitudeStandardToPressureConverter(CDMReaderPtr reader, CoordSysPtr cs, const std::string& altitude)
        : BasicVerticalConverter(reader, cs), altitude_(altitude) { }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string altitude_;
};

} // namespace MetNoFimex

#endif // ALTITUDESTANDARDTOPRESSURECONVERTER_H
