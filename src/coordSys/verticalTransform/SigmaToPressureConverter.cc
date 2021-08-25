/*
 * Fimex, SigmaToPressureConverter.cc
 *
 * (C) Copyright 2016, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Jul 13, 2016
 *      Author: Alexander Bürger
 */

#include "fimex/coordSys/verticalTransform/SigmaToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/vertical_coordinate_transformations.h"

namespace MetNoFimex {

using std::vector;

std::vector<std::string> SigmaToPressureConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_)
            .merge(ps_, true)
            .merge(ptop_, true)
            .merge(sigma_, true)
            .shape();
}

DataPtr SigmaToPressureConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble ps(reader_, ps_, "hPa", sb);
    VarDouble ptop(reader_, ptop_, "hPa", sb);
    VarDouble sigma(reader_, sigma_, "", sb);
    if (!ps || !ptop || !sigma)
        return DataPtr();

    ArrayDims out_dims = makeArrayDims(sb);
    shared_array<double> out_values(new double[out_dims.volume()]);

    enum { PS, PTOP, SIGMA, OUT };
    ArrayGroup group;
    group.add(ps.dims).add(ptop.dims).add(sigma.dims).add(out_dims);

    const size_t shared = group.sharedVolume();
    Loop loop(group);
    do {
        mifi_atmosphere_sigma_pressure(shared, ptop.values[loop[PTOP]], ps.values[loop[PS]],
                &sigma.values[loop[SIGMA]], &out_values[loop[OUT]]);
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace MetNoFimex
