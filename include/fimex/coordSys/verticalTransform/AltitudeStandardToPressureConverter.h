#ifndef ALTITUDESTANDARDTOPRESSURECONVERTER_H
#define ALTITUDESTANDARDTOPRESSURECONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"

namespace MetNoFimex {

/**
 * using the international standard atmosphere to convert altitude to pressure
 */
class AltitudeStandardToPressureConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReaderPtr reader, CoordSysPtr cs, VerticalConverterPtr altitude);

    /**
     * @param altitude given in m
     */
    AltitudeStandardToPressureConverter(CDMReaderPtr reader, CoordSysPtr cs, const VerticalConverterPtr& altitude);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverterPtr altitude_;
};

} // namespace MetNoFimex

#endif // ALTITUDESTANDARDTOPRESSURECONVERTER_H
