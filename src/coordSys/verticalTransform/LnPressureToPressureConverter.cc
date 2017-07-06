
#include "fimex/coordSys/verticalTransform/LnPressureToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/vertical_coordinate_transformations.h"

namespace MetNoFimex {

std::vector<std::string> LnPressureToPressureConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_)
            .merge(lnP_, true)
            .merge(p0_, true)
            .shape();
}

DataPtr LnPressureToPressureConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble p0(reader_, p0_, "hPa", sb);
    VarDouble lnP(reader_, lnP_, "", sb);

    ArrayDims out_dims = makeArrayDims(sb);
    boost::shared_array<double> out_values(new double[out_dims.volume()]);

    enum { P0, LNP, OUT };
    ArrayGroup group;
    group.add(p0.dims).add(lnP.dims).add(out_dims);

    const size_t shared = group.sharedVolume();
    Loop loop(group);
    do {
        mifi_atmosphere_ln_pressure(shared, p0.values[loop[P0]], &lnP.values[loop[LNP]],
                &out_values[loop[OUT]]);
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace MetNoFimex
