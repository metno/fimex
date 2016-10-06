#ifndef LNPRESSURETOPRESSURECONVERTER_H
#define LNPRESSURETOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/coordSys/verticalTransform/LnPressureToPressureConverter.h
 */

/**
 * Constant pressure levels in time and space, given as ln(p)
 */
class LnPressureToPressureConverter : public BasicVerticalConverter {
public:

    /**
     * @param p0 reference-pressure in hPa
     * @param lnP The constant pressure levels given as ln(P/P0)
     */
    LnPressureToPressureConverter(CDMReaderPtr reader, CoordSysPtr cs, const std::string& p0, const std::string& lnP)
        : BasicVerticalConverter(reader, cs), p0_(p0), lnP_(lnP) { }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string p0_;
    const std::string lnP_;
};

} // namespace MetNoFimex

#endif // LNPRESSURETOPRESSURECONVERTER_H
