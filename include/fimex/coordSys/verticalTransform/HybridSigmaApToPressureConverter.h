#ifndef HYBRIDSIGMAAPTOPRESSURECONVERTER_H
#define HYBRIDSIGMAAPTOPRESSURECONVERTER_H

#include "VerticalConverter.h"

namespace MetNoFimex {

/**
 * Hybrid sigma levels defined by constant ap and b. The surface-pressure
 * varies in (x,y,z). Corresponds to mifi_atmosphere_hybrid_sigma_ap_pressure()
 */
class HybridSigmaApToPressureConverter : public BasicVerticalConverter {
public:
    static VerticalConverterPtr createConverter(CDMReader_p reader, CoordSysPtr cs,
        const std::string& ap, const std::string& b, const std::string& ps, const std::string& p0 = std::string());

    /**
     * @param ap vector of size n containing the ap-parameters in hPa of sigma-hybrid
     * @param b vector of size n containing the b parameters (dimensionless) of sigma-hybrid
     * @param ps array of size nx*ny*nt containing the surface-pressure
     * @param p0 reference-pressure in hPa
     */
    HybridSigmaApToPressureConverter(CDMReader_p reader, CoordSysPtr cs, const std::string& ap, const std::string& b,
            const std::string& ps, const std::string& p0)
        : BasicVerticalConverter(reader, cs), ap_(ap), b_(b), ps_(ps), p0_(p0) { }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string ap_;
    const std::string b_;
    const std::string ps_;
    const std::string p0_;
};

} // namespace MetNoFimex

#endif // HYBRIDSIGMAAPTOPRESSURECONVERTER_H
