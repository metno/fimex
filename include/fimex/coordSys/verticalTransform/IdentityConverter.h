#ifndef GEOPOTENTIALTOALTITUDECONVERTER_H
#define GEOPOTENTIALTOALTITUDECONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include <string>

namespace MetNoFimex {

/**
 * Use geopotential height (in m) and topography to calculate the
 * height above ground (hg = height - topo)
 */
class IdentityConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverterForStandardName(CDMReader_p reader, CoordSysPtr cs, const std::string& stdName, const std::string& unit);
    static VerticalConverterPtr createConverterForVarName(CDMReader_p reader, CoordSysPtr cs, const std::string& varName, const std::string& unit);

    IdentityConverter(CDMReader_p reader, CoordSysPtr cs, const std::string& varName, const std::string& unit)
        : BasicVerticalConverter(reader, cs), varName_(varName), varUnit_(unit) {}

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    std::string varName_;
    std::string varUnit_;
};

} // namespace MetNoFimex

#endif // GEOPOTENTIALTOALTITUDECONVERTER_H
