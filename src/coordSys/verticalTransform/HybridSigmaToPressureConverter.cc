
#include "fimex/coordSys/verticalTransform/HybridSigmaToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <boost/make_shared.hpp>

namespace MetNoFimex {

using std::vector;

// static method
VerticalConverterPtr HybridSigmaToPressureConverter::createConverter(CDMReaderPtr reader, CoordSysPtr cs,
    const std::string& a, const std::string& b, const std::string& ps, const std::string& p0)
{
    return boost::make_shared<HybridSigmaToPressureConverter>(reader, cs, a, b, ps, p0);
}

std::vector<std::string> HybridSigmaToPressureConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_)
            .merge(ps_, true)
            .merge(p0_, true) // empty p0_ handled in ShapeMerger
            .merge(a_, true)
            .merge(b_, true)
            .shape();
}

DataPtr HybridSigmaToPressureConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble ps(reader_, ps_, "hPa", sb);
    VarDouble a(reader_, a_, "", sb);
    VarDouble b(reader_, b_, "", sb);

    ArrayDims out_dims = makeArrayDims(sb);
    boost::shared_array<double> out_values(new double[out_dims.volume()]);

    enum { PS, A, B, OUT };
    ArrayGroup group;
    group.add(ps.dims).add(a.dims).add(b.dims).add(out_dims);

    const int unLimDimPos = 0; // TODO is this correct?
    const double p0 = getDataSliceInUnit(reader_, p0_, "hPa", unLimDimPos).front();

    const size_t shared = group.sharedVolume();
    Loop loop(group);
    do {
        mifi_atmosphere_hybrid_sigma_pressure(shared, p0, ps.values[loop[PS]], &a.values[loop[A]],
                &b.values[loop[B]], &out_values[loop[OUT]]);
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace MetNoFimex
