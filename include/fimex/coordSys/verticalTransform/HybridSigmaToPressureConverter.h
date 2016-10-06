#ifndef HYBRIDSIGMATOPRESSURECONVERTER_H
#define HYBRIDSIGMATOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Hybrid sigma levels defined by constant a and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_pressure()
 */
class HybridSigmaToPressureConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReaderPtr reader, CoordSysPtr cs,
        const std::string& a, const std::string& b, const std::string& ps, const std::string& p0);

    /**
     * @param a vector of size n containing the ap-parameters (dimensionless) of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param ps surface pressure variable name
     * @param p0 reference-pressure in hPa
     */
    HybridSigmaToPressureConverter(CDMReaderPtr reader, CoordSysPtr cs, const std::string& a, const std::string& b,
            const std::string& ps, const std::string& p0)
        : BasicVerticalConverter(reader, cs), a_(a), b_(b), ps_(ps), p0_(p0) { }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string a_;
    const std::string b_;
    const std::string ps_;
    const std::string p0_;
};

} // namespace MetNoFimex

#endif // HYBRIDSIGMATOPRESSURECONVERTER_H
