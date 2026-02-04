/*
  Fimex, src/coordSys/verticalTransform/LnPressureToPressureConverter.cc

  Copyright (C) 2019-2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

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
    if (!p0 || !lnP)
        return DataPtr();

    ArrayDims out_dims = makeArrayDims(sb);
    auto out_values = make_shared_array<double>(out_dims.volume());

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
