/*
  Fimex, src/coordSys/verticalTransform/HybridSigmaApToPressureConverter.cc

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


#include "fimex/coordSys/verticalTransform/HybridSigmaApToPressureConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/vertical_coordinate_transformations.h"

#include <memory>

#include <vector>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.HybridSigmaApToPressureConverter");
#define LOGVAL(x) " " #x "='" << x << "'"

using std::vector;

// static method
VerticalConverter_p HybridSigmaApToPressureConverter::createConverter(CDMReader_p reader, CoordinateSystem_cp cs, const std::string& ap, const std::string& b,
                                                                      const std::string& ps, const std::string& p0)
{
    return std::make_shared<HybridSigmaApToPressureConverter>(reader, cs, ap, b, ps, p0);
}

std::vector<std::string> HybridSigmaApToPressureConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_)
            .merge(ps_, true)
            //.merge(p0_, true) // p0_ is not used
            .merge(ap_, true)
            .merge(b_, true)
            .shape();
}

DataPtr HybridSigmaApToPressureConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble ps(reader_, ps_, "hPa", sb);
    VarDouble ap(reader_, ap_, "hPa", sb);
    VarDouble b(reader_, b_, "", sb);

    ArrayDims out_dims = makeArrayDims(sb);
    boost::shared_array<double> out_values(new double[out_dims.volume()]);

    enum { PS, AP, B, OUT/*, IN_P0*/ };
    ArrayGroup group;
    group.add(ps.dims).add(ap.dims).add(b.dims).add(out_dims);

//    ArrayDims p0_dims;
//    boost::shared_array<float> p0_values;
//    if (!p0_.empty()) {
//        SliceBuilder p0_sb = adaptSliceBuilder(rcdm, p0_, sb);
//        if (DataPtr p0_data = getSliceData(reader_, p0_sb, p0_, "hPa")) {
//            p0_values = p0_data->asFloat();
//            p0_dims = makeArrayDims(p0_sb);
//            group.add(p0_dims);
//        }
//    }

    const size_t shared = group.sharedVolume();
    Loop loop(group);
    do {
        mifi_atmosphere_hybrid_sigma_ap_pressure(shared, ps.values[loop[PS]], &ap.values[loop[AP]],
                &b.values[loop[B]], &out_values[loop[OUT]]);

        LOG4FIMEX(logger, Logger::DEBUG, "ps[" << loop[PS] << "]=" << ps.values[loop[PS]]
                << " ap[" << loop[AP] << "]=" << ap.values[loop[AP]]
                << " b[" << loop[B] << "]=" << b.values[loop[B]]
                << " out[" << loop[OUT] << "]=" << out_values[loop[OUT]]);
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace MetNoFimex
