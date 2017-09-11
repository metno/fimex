#ifndef OCEANSCOORDINATEGTODEPTHCONVERTER_H
#define OCEANSCOORDINATEGTODEPTHCONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include "fimex/IndexedData.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.h
 */

struct OceanSGVars {
    //! k-dependent sigma coordinate
    std::string s;
    //! k-dependent stretching function
    std::string C;
    //! ocean-depth, might be time-varying for sediment applications
    std::string depth;
    //! critical depth (~ min(h(x,y)))
    std::string depth_c;
    //! time-varying free surface, might be 0 / empty
    std::string eta;

    OceanSGVars(const std::string& s, const std::string& C, const std::string& depth,
                     const std::string& depth_c, const std::string& eta)
        : s(s), C(C), depth(depth), depth_c(depth_c), eta(eta) { }

    bool isComplete() const
        { return s != "" && C != "" && depth != "" && depth_c != ""; }
};



/**
 * Use ocean_s_coordinate_g1 or g2 to calculate depth
 */
class OceanSCoordinateGToDepthConverter : public BasicVerticalConverter {
public:
    typedef int (*heightconversion_t)(size_t, double, double, double, const double*, const double*, double*);

    /**
     * @param s k-dependent sigma coordinate
     * @param C k-dependent stretching function
     * @param depth_c critical depth (~ min(h(x,y)))
     * @param eta time-varying free surface, might be 0
     * @param depth ocean-depth, might be time-varying for sediment applications
     * @param nx,ny,nk,nt array sizes
     * @param func either mifi_ocean_s_g1_z() or mifi_ocean_s_g2_z()
     */
    OceanSCoordinateGToDepthConverter(CDMReader_p reader, CoordSysPtr cs, const OceanSGVars& vars, heightconversion_t func);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

    std::vector<std::string> getValidityShape(const std::string& verticalDim) const;
    DataPtr getValiditySlice(const SliceBuilder& sb, const std::vector<double>& verticalValues) const;

private:
    const OceanSGVars vars_;
    heightconversion_t func_;
};

} /* namespace MetNoFimex */

#endif // OCEANSCOORDINATEGTODEPTHCONVERTER_H

