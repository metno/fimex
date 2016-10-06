#ifndef SIGMATOPRESSURECONVERTER_H
#define SIGMATOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Sigma levels. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_sigma_pressure()
 */
class SigmaToPressureConverter : public BasicVerticalConverter {
public:
    /**
     * @param sigma vector of size n containing the sigma parameters (dimensionless) of sigma-hybrid
     * @param ptop top of atmosphere in hPa
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param nx x-size of ps
     * @param ny y-size of ps
     * @param nt t-size of ps
     */
    SigmaToPressureConverter(CDMReaderPtr reader, CoordSysPtr cs, const std::string& sigma, const std::string& ptop, const std::string& ps)
        : BasicVerticalConverter(reader, cs), sigma_(sigma), ptop_(ptop), ps_(ps) {}

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string sigma_;
    const std::string ptop_;
    const std::string ps_;
};

} /* namespace MetNoFimex */

#endif // SIGMATOPRESSURECONVERTER_H
