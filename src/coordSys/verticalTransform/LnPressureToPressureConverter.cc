
#include "fimex/coordSys/verticalTransform/LnPressureToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex {

std::vector<std::string> LnPressureToPressureConverter::getShape() const
{
    // combine shapes of a, b (length = height) and ps (other dimensions)
    std::vector<std::string> shape = reader_->getCDM().getVariable(lnP_).getShape();
    shape.insert(shape.begin() + 2, cs_->getGeoZAxis()->getName()); // FIXME this is a crazy hack!
    return shape;
}

DataPtr LnPressureToPressureConverter::getDataSlice(const SliceBuilder& sb) const
{
#if 0
    const vector<double> levVec = getDataSliceInUnit(reader_, lev, "", unLimDimPos);
    const vector<double> p0Vec = getDataSliceInUnit(reader_, p0, "hPa", unLimDimPos);

    mifi_atmosphere_ln_pressure(lnP.size(), p0, &lnP[0], &pres_[0]);
#endif
    return DataPtr();
}

} // namespace MetNoFimex
