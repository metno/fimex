/*
  Fimex, src/coordSys/verticalTransform/HybridSigmaToPressureConverter.cc

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/


#include "fimex/coordSys/verticalTransform/HybridSigmaToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <memory>

namespace MetNoFimex {

using std::vector;

// static method
VerticalConverter_p HybridSigmaToPressureConverter::createConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& a, const std::string& b,
                                                                    const std::string& ps, const std::string& p0)
{
    return std::make_shared<HybridSigmaToPressureConverter>(reader, cs, a, b, ps, p0);
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
    if (!ps || !a || !b)
        return DataPtr();

    ArrayDims out_dims = makeArrayDims(sb);
    auto out_values = make_shared_array<double>(out_dims.volume());

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
