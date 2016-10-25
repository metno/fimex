/*
 * Fimex, OceanSG1.cc
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Aug 8, 2013
 *      Author: heikok
 */

#include "fimex/coordSys/verticalTransform/OceanSG1.h"
#include "fimex/coordSys/verticalTransform/OceanSG2.h"
#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/vertical_coordinate_transformations.h"

#include "ArrayLoop.h"

#include <algorithm>
#include <functional>
#include <sstream>

namespace MetNoFimex {

static const double ZERO = 0;

OceanSCoordinateGToDepthConverter::OceanSCoordinateGToDepthConverter(CDMReaderPtr reader, CoordSysPtr cs,
                                                                     const OceanSGVars& vars, heightconversion_t func)
    : BasicVerticalConverter(reader, cs)
    , vars_(vars)
    , func_(func)
{
}

std::vector<std::string> OceanSCoordinateGToDepthConverter::getShape() const
{
    // combine shapes of eta(n,j,i) and s(k)
    std::vector<std::string> sgshape;
    const std::vector<std::string> shs= reader_->getCDM().getVariable(vars_.s).getShape();
    if (vars_.eta.empty()) {
        sgshape = reader_->getCDM().getVariable(vars_.depth).getShape();
        sgshape.insert(sgshape.end(), shs.begin(), shs.end()); // FIXME this is a crazy hack!
    } else {
        sgshape= reader_->getCDM().getVariable(vars_.eta).getShape();
        sgshape.insert(sgshape.begin() + 2, shs.front()); // FIXME this is a crazy hack!
        sgshape.insert(sgshape.end(), shs.begin()+1, shs.end());
    }
    return sgshape;
}

DataPtr OceanSCoordinateGToDepthConverter::getDataSlice(const SliceBuilder& sb) const
{
    if (!vars_.isComplete())
        throw CDMException("OceanSCoordinateGToDepthConverter incomplete formula terms");

    const CDM& cdm = reader_->getCDM();

    DataPtr depth_cData = getSliceData(reader_, sb, vars_.depth_c, "m");
    const double depth_c = depth_cData ? depth_cData->asDouble()[0] : 0;

    const boost::shared_array<double> sValues = getSliceDoubles(reader_, sb, vars_.s, "");
    const boost::shared_array<double> CValues = getSliceDoubles(reader_, sb, vars_.C, "");
    const boost::shared_array<double> depthValues = getSliceDoubles(reader_, sb, vars_.depth, "m");

    ArrayDims dimsS = makeArrayDims(adaptSliceBuilder(cdm, vars_.s, sb));
    ArrayDims dimsC = makeArrayDims(adaptSliceBuilder(cdm, vars_.C, sb));
    ArrayDims dimsDepth = makeArrayDims(adaptSliceBuilder(cdm, vars_.depth, sb));
    ArrayDims dimsZ = makeArrayDims(cdm, getShape());
    ArrayDims dimsEta;

    forceUnLimDimLength1(cdm, dimsS, dimsC, dimsDepth, dimsZ);

    enum { S, C, DEPTH, Z, ETA }; // ETA must be last as it is optional
    ArrayGroup group = ArrayGroup().add(dimsS).add(dimsC).add(dimsDepth).add(dimsZ);

    boost::shared_array<double> etaValues;
    if (!vars_.eta.empty()) {
        etaValues = getSliceDoubles(reader_, sb, vars_.eta, "m");

        dimsEta = makeArrayDims(adaptSliceBuilder(cdm, vars_.eta, sb));
        forceUnLimDimLength1(cdm, dimsEta);
        group.add(dimsEta);
    }

    boost::shared_array<double> zValues(new double[dimsZ.volume()]);

    Loop loop(group);
    do {
        const double eta = etaValues ? etaValues[loop[ETA]] : 0;
        func_(group.sharedVolume(), depthValues[loop[DEPTH]], depth_c, eta, &sValues[loop[S]], &CValues[loop[C]], &zValues[loop[Z]]);
    } while (loop.next());

    /* z as calculated by formulas is negative down, but we want positive down */
    std::transform(zValues.get(), zValues.get() + dimsZ.volume(), zValues.get(), std::bind1st(std::multiplies<double>(),-1.));

    // TODO transform invalid -> nan, nan -> bad?

    return createData(dimsZ.volume(), zValues);
}

std::vector<std::string> OceanSCoordinateGToDepthConverter::getValidityShape(const std::string& verticalDim) const
{
    const CDM& cdm = reader_->getCDM();
    std::vector<std::string> shd = cdm.getVariable(vars_.depth).getShape();
    std::replace(shd.begin(), shd.end(), cs_->getGeoZAxis()->getName(), verticalDim);
    if (std::find(shd.begin(), shd.end(), verticalDim) == shd.end())
        shd.push_back(verticalDim);
    return shd;
}

DataPtr OceanSCoordinateGToDepthConverter::getValiditySlice(const SliceBuilder& sb, const std::vector<double>& verticalValues) const
{
    const CDM& cdm = reader_->getCDM();
    const std::string& verticalDim = cs_->getGeoZAxis()->getName();

    const boost::shared_array<double> depthValues = getSliceDoubles(reader_, sb, vars_.depth, "m");
    ArrayDims dimsDepth = makeArrayDims(adaptSliceBuilder(cdm, vars_.depth, sb));

    ArrayDims dimsValid = makeArrayDims(cdm, getValidityShape(verticalDim));
    dimsValid.set_length(verticalDim, verticalValues.size());

    ArrayDims dimsVertical = ArrayDims().add(verticalDim, verticalValues.size());

    enum { DEPTH, VERTICAL, VALID };
    ArrayGroup group = ArrayGroup().add(dimsDepth).add(dimsVertical).add(dimsValid);
    group.minimizeShared(0);

    boost::shared_array<unsigned char> validValues(new unsigned char[dimsValid.volume()]);

    Loop loop(group);
    do {
        validValues[loop[VALID]] = (depthValues[loop[DEPTH]] >= verticalValues[loop[VERTICAL]]) ? 1 : 0;
    } while (loop.next());

    return createData(dimsValid.volume(), validValues);
}

} // namespace MetNoFimex
