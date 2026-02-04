/*
 * Fimex, AtmosphereHybridHeight.cc
 *
 * (C) Copyright 2024-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 */

#include "fimex/coordSys/verticalTransform/AtmosphereHybridHeight.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"
#include "fimex/coordSys/verticalTransform/SigmaToPressureConverter.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <memory>

namespace MetNoFimex {

namespace {

class HybridToAltitudeConverter : public BasicVerticalConverter
{
public:
    HybridToAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& a, const std::string& b, const std::string& orog)
        : BasicVerticalConverter(reader, cs)
        , a_(a)
        , b_(b)
        , orog_(orog)
    {
    }

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    const std::string a_;
    const std::string b_;
    const std::string orog_;
};

std::vector<std::string> HybridToAltitudeConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_).merge(orog_).merge(b_).merge(a_).shape();
}

DataPtr HybridToAltitudeConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble orog(reader_, orog_, "m", sb);
    VarDouble b(reader_, b_, "", sb);
    VarDouble a(reader_, a_, "m", sb);
    if (!orog || !a || !a)
        return DataPtr();

    ArrayDims out_dims = makeArrayDims(sb);
    auto out_values = make_shared_array<double>(out_dims.volume());

    enum { OROG, B, A, OUT };
    ArrayGroup group;
    group.add(orog.dims).add(b.dims).add(a.dims).add(out_dims);

    const size_t shared = group.sharedVolume();
    Loop loop(group);
    do {
        mifi_atmosphere_hybrid_height(shared, orog.values[loop[OROG]], &a.values[loop[A]], &b.values[loop[B]], &out_values[loop[OUT]]);
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace

VerticalConverter_p AtmosphereHybridHeight::getPressureConverter(CDMReader_p /*reader*/, CoordinateSystem_cp /*cs*/) const
{
    return VerticalConverter_p();
}

VerticalConverter_p AtmosphereHybridHeight::getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    VerticalConverter_p c = getAltitudeConverter(reader, cs);
    return AltitudeHeightConverter::createToHeightConverter(reader, cs, c);
}

VerticalConverter_p AtmosphereHybridHeight::getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const
{
    return std::make_shared<HybridToAltitudeConverter>(reader, cs, a, b, orog);
}

} // namespace MetNoFimex
