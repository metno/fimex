/*
 * Fimex, ToVLevelConverter.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Aug 12, 2011
 *      Author: Heiko Klein
 */

#include "ToVLevelConverter.h"
#include <map>
#include <functional>
#include <algorithm>
#include <boost/regex.hpp>
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"
#include "fimex/Logger.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"
#include <fimex/coordSys/verticalTransform/VerticalTransformation.h>
#include <fimex/coordSys/verticalTransform/AtmosphereSigma.h>
#include <fimex/coordSys/verticalTransform/Height.h>
#include <fimex/coordSys/verticalTransform/HybridSigmaPressure1.h>
#include <fimex/coordSys/verticalTransform/HybridSigmaPressure2.h>
#include <fimex/coordSys/verticalTransform/LnPressure.h>
#include <fimex/coordSys/verticalTransform/OceanSG1.h>
#include <fimex/coordSys/verticalTransform/OceanSG2.h>
#include <fimex/coordSys/verticalTransform/Pressure.h>

namespace MetNoFimex {

using namespace std;

static LoggerPtr logger = getLogger("fimex.ToVLevelConverter");

// fetch a parameter-value from the formula_terms attribute
static string getTerm(const CDMAttribute& formulaTerms, string parameter)
{
    boost::smatch what;
    if (boost::regex_match(formulaTerms.getStringValue(), what, boost::regex(".*\\Q"+parameter+"\\E:\\s*(\\S+).*"))) {
        return what[1];
    }
    return "";
}

static const vector<double> getDataSliceInUnit(const boost::shared_ptr<CDMReader>& reader, const string& var, const string& unit, int unLimDimPos)
{
    DataPtr data = reader->getScaledDataSliceInUnit(var, unit, unLimDimPos);
    boost::shared_array<double> array = data->asDouble();
    return vector<double>(&array[0], &array[0] + data->size());
}



boost::shared_ptr<ToVLevelConverter> ToVLevelConverter::getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt)
{
    boost::shared_ptr<ToVLevelConverter> presConv;
    assert(cs.get() != 0);
    if (!cs->hasVerticalTransformation()) {
        throw CDMException("undefined vertical coordinate transformation for " + type2string(cs));
    }
    boost::shared_ptr<const VerticalTransformation> vtran = cs->getVerticalTransformation();
    assert(vtran.get() != 0);
    if (!vtran->isComplete()) {
        throw CDMException(type2string(cs) + " not complete for transformation");
    }
    if (vtran->getName() == "pressure") {
        const Pressure* pres = dynamic_cast<const Pressure*>(vtran.get());
        assert(pres != 0);
        DataPtr p = reader->getScaledDataSliceInUnit(pres->pressure, "hPa", unLimDimPos);
        boost::shared_array<double> pa = p->asDouble();
        presConv = boost::shared_ptr<ToVLevelConverter>(new IdentityToVLevelConverter(vector<double> (&pa[0], &pa[0] + p->size())));
    } else if (vtran->getName() == "height") {
        const Height* height = dynamic_cast<const Height*>(vtran.get());
        assert(height != 0);
        DataPtr h = reader->getScaledDataSliceInUnit(height->height, "m", unLimDimPos);
        boost::shared_array<double> ha = h->asDouble();
        presConv = boost::shared_ptr<ToVLevelConverter>(new HeightStandardToPressureConverter(vector<double> (&ha[0],&ha[0] + h->size())));
    } else if (vtran->getName() == "atmosphere_hybrid_sigma_pressure_coordinate_1") {
        const HybridSigmaPressure1* hyb1 = dynamic_cast<const HybridSigmaPressure1*>(vtran.get());
        assert(hyb1 != 0);
        const vector<double> apVec = getDataSliceInUnit(reader, hyb1->ap, "hPa", unLimDimPos);
        const vector<double> bVec = getDataSliceInUnit(reader, hyb1->b, "", unLimDimPos);
        DataPtr psData = reader->getScaledDataSliceInUnit(hyb1->ps, "hPa", unLimDimPos);
        if (nx * ny * nt != psData->size()) {
            throw CDMException("unexpected size of pressure " + hyb1->ps + "(" + type2string(unLimDimPos) +
                                           "), should be " + type2string(nx * ny * nt) + " != " + type2string(psData->size()));
         }
         presConv = boost::shared_ptr<ToVLevelConverter>(new HybridSigmaApToPressureConverter(apVec, bVec, psData->asDouble(), nx, ny, nt));
    } else if (vtran->getName() == "atmosphere_hybrid_sigma_pressure_coordinate_2") {
        const HybridSigmaPressure2* hyb2 = dynamic_cast<const HybridSigmaPressure2*>(vtran.get());
        assert(hyb2 != 0);
        const vector<double> aVec = getDataSliceInUnit(reader, hyb2->a, "", unLimDimPos);
        const vector<double> bVec = getDataSliceInUnit(reader, hyb2->b, "", unLimDimPos);
        const vector<double> p0Vec = getDataSliceInUnit(reader, hyb2->p0, "hPa", unLimDimPos);
        DataPtr psData = reader->getScaledDataSliceInUnit(hyb2->ps, "hPa", unLimDimPos);
        if (nx * ny * nt != psData->size()) {
            throw CDMException("unexpected size of pressure " + hyb2->ps + "(" + type2string(unLimDimPos) +
                               "), should be " + type2string(nx * ny * nt) + " != " + type2string(psData->size()));
        }
        presConv = boost::shared_ptr<ToVLevelConverter>(new HybridSigmaToPressureConverter(aVec, bVec, p0Vec.at(0), psData->asDouble(), nx, ny, nt));
    } else if (vtran->getName() == "atmosphere_ln_pressure_coordinate") {
        const LnPressure* lnpres = dynamic_cast<const LnPressure*>(vtran.get());
        assert(lnpres != 0);
        const vector<double> levVec = getDataSliceInUnit(reader, lnpres->lev, "", unLimDimPos);
        const vector<double> p0Vec = getDataSliceInUnit(reader, lnpres->p0, "hPa", unLimDimPos);
        presConv = boost::shared_ptr<ToVLevelConverter>(new LnPressureToPressureConverter(p0Vec.at(0), levVec));
    } else if (vtran->getName() == "atmosphere_sigma_coordinate") {
        const AtmosphereSigma* asigma = dynamic_cast<const AtmosphereSigma*>(vtran.get());
        assert(asigma != 0);
        const vector<double> sigmaVec = getDataSliceInUnit(reader, asigma->sigma, "", unLimDimPos);
        const vector<double> ptopVec = getDataSliceInUnit(reader, asigma->ptop, "hPa", unLimDimPos);
        DataPtr psData = reader->getScaledDataSliceInUnit(asigma->ps, "hPa", unLimDimPos);
        if (nx * ny * nt != psData->size()) {
            throw CDMException("unexpected size of pressure " + asigma->ps + "(" + type2string(unLimDimPos) +
                    "), should be " + type2string(nx * ny * nt) + " != " + type2string(psData->size()));
        }
        presConv = boost::shared_ptr<ToVLevelConverter>(new SigmaToPressureConverter(sigmaVec, ptopVec[0], psData->asDouble(), nx, ny, nt));
    } else {
        throw CDMException("unimplemented vertical axis with standard_name: " + vtran->getName());
    }
    return presConv;
}

boost::shared_ptr<ToVLevelConverter> ToVLevelConverter::getHeightConverter(
        const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos,
        const boost::shared_ptr<const CoordinateSystem> cs,
        size_t nx, size_t ny, size_t nz, size_t nt)
{
    boost::shared_ptr<ToVLevelConverter> heightConv;
    assert(cs.get() != 0);
    if (!cs->hasVerticalTransformation()) {
        throw CDMException("undefined vertical coordinate transformation for " + type2string(cs));
    }
    boost::shared_ptr<const VerticalTransformation> vtran = cs->getVerticalTransformation();
    assert(vtran.get() != 0);
    if (!vtran->isComplete()) {
        throw CDMException(type2string(cs) + " not complete for transformation");
    }
    if (vtran->getName() == "height") {
        const Height* h = dynamic_cast<const Height*>(vtran.get());
        assert(h != 0);
        DataPtr hd = reader->getScaledDataSliceInUnit(h->height, "m", unLimDimPos);
        const boost::shared_array<double> ha = hd->asDouble();
        heightConv = boost::shared_ptr<ToVLevelConverter>(new IdentityToVLevelConverter(vector<double> (&ha[0], &ha[0] + hd->size())));
    } else if (vtran->getName() == "pressure") {
        heightConv = boost::shared_ptr<ToVLevelConverter>(new PressureToStandardHeightConverter(getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt)));
    } else if (vtran->getName() == "ocean_s_coordinate_g1" || vtran->getName() == "ocean_s_coordinate_g2") {
        vector<double> sVec;
        vector<double> CVec;
        vector<double> depth_cVec;
        IndexedData depthDI;
        // default eta: single value, 0
        IndexedData etaDI(createData(CDM_DOUBLE, 1, 0.), vector<size_t>(1,1));
        if (vtran->getName() == "ocean_s_coordinate_g1") {
            const OceanSG1* osg = dynamic_cast<const OceanSG1*>(vtran.get());
            assert(osg != 0);
            sVec = getDataSliceInUnit(reader, osg->s, "", unLimDimPos); // size k
            CVec = getDataSliceInUnit(reader, osg->C, "", unLimDimPos); // size k
            depth_cVec = getDataSliceInUnit(reader, osg->depth_c, "m", unLimDimPos); // size 1
            DataPtr depthD = reader->getScaledDataSliceInUnit(osg->depth, "m", unLimDimPos);
            depthDI = IndexedData(depthD, reader->getDimsSlice(osg->depth));
            size_t dSize = (depthDI.idx().getDims().size() == 3) ? (nx*ny*nt) : (nx*ny);
            if (depthD->size() < dSize) {
                throw CDMException("unexpected size of depth " + osg->depth + "(" + type2string(unLimDimPos) +
                        "), should be " + type2string(nx * ny * nt) + " != " + type2string(depthD->size()));
                // I allow depthD to be larger than dSize for staggered grids (grids with +-1 cell)
            }
            if (osg->eta != "") {
                DataPtr etaD = reader->getScaledDataSliceInUnit(osg->eta, "m", unLimDimPos);
                if (etaD->size() < (nx * ny * nt)) {
                    throw CDMException("unexpected size of eta " + osg->eta + "(" + type2string(unLimDimPos) +
                            "), should be " + type2string(nx * ny * nt) + " != " + type2string(etaD->size()));
                }
                etaDI = IndexedData(etaD, reader->getDimsSlice(osg->eta));
            }
            heightConv = boost::shared_ptr<ToVLevelConverter>(new OceanSCoordinateGToDepthConverter(sVec, CVec, depth_cVec[0], etaDI, depthDI,
                    nx, ny, nz, nt, mifi_ocean_s_g1_z));
        } else { // "ocean_s_coordinate_g2") {
            const OceanSG2* osg = dynamic_cast<const OceanSG2*>(vtran.get());
            assert(osg != 0);
            sVec = getDataSliceInUnit(reader, osg->s, "", unLimDimPos); // size k
            CVec = getDataSliceInUnit(reader, osg->C, "", unLimDimPos); // size k
            depth_cVec = getDataSliceInUnit(reader, osg->depth_c, "m", unLimDimPos); // size 1
            DataPtr depthD = reader->getScaledDataSliceInUnit(osg->depth, "m", unLimDimPos);
            depthDI = IndexedData(depthD, reader->getDimsSlice(osg->depth));
            size_t dSize = (depthDI.idx().getDims().size() == 3) ? (nx*ny*nt) : (nx*ny);
            if (depthD->size() < dSize) {
                throw CDMException("unexpected size of depth " + osg->depth + "(" + type2string(unLimDimPos) +
                        "), should be " + type2string(nx * ny * nt) + " != " + type2string(depthD->size()));
                // I allow depthD to be larger than dSize for staggered grids (grids with +-1 cell)
            }
            if (osg->eta != "") {
                DataPtr etaD = reader->getScaledDataSliceInUnit(osg->eta, "m", unLimDimPos);
                if (etaD->size() < (nx * ny * nt)) {
                    throw CDMException("unexpected size of eta " + osg->eta + "(" + type2string(unLimDimPos) +
                            "), should be " + type2string(nx * ny * nt) + " != " + type2string(etaD->size()));
                }
                etaDI = IndexedData(etaD, reader->getDimsSlice(osg->eta));
            }
            heightConv = boost::shared_ptr<ToVLevelConverter>(new OceanSCoordinateGToDepthConverter(sVec, CVec, depth_cVec[0], etaDI, depthDI,
                        nx, ny, nz, nt, mifi_ocean_s_g2_z));
        }
    } else {
        // try geopotential_height or fall back to pressure
        map<string, string> attrs;
        vector<string> dims;
        const CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
        const CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
        const CoordinateSystem::ConstAxisPtr zAxis = cs->getGeoZAxis();
        if (xAxis.get() != 0 && yAxis.get() != 0 && zAxis.get() != 0) {
            dims.push_back(xAxis->getShape()[0]);
            dims.push_back(yAxis->getShape()[0]);
            dims.push_back(zAxis->getShape()[0]);
            attrs["standard_name"] = "geopotential_height";
            vector<string> geoVars = reader->getCDM().findVariables(attrs, dims);
            dims.clear();
            dims.push_back(xAxis->getShape()[0]);
            dims.push_back(yAxis->getShape()[0]);
            attrs["standard_name"] = "altitude";
            vector<string> altVars = reader->getCDM().findVariables(attrs, dims);
            if (geoVars.size() > 0 && altVars.size() > 0) {
                LOG4FIMEX(logger, Logger::INFO, "using geopotential height "<<geoVars[0]<<" to retrieve height");
                DataPtr geoPotData = reader->getScaledDataSliceInUnit(geoVars[0], "m", unLimDimPos);
                if (geoPotData->size() != (nx * ny * nz * nt)) {
                    throw CDMException("geopotential height '" + geoVars[0] + "' has strange size: " + type2string(geoPotData->size()) + " != " + type2string(nx * ny * nz * nt));
                }
                LOG4FIMEX(logger, Logger::INFO, "using altitude "<<altVars[0]<<" to retrieve height");
                DataPtr altData = reader->getScaledDataSliceInUnit(altVars[0], "m", unLimDimPos);
                if (altData->size() != (nx * ny)) {
                    throw CDMException("altitude '" + altVars[0] + "' has strange size: " + type2string(altData->size()) + " != " + type2string(nx * ny));
                }
                heightConv = boost::shared_ptr<ToVLevelConverter>(new GeopotentialToHeightConverter(geoPotData->asFloat(), altData->asFloat(), nx, ny, nz, nt));
                vector<string> altVars = reader->getCDM().findVariables(attrs, dims);
            } else {
                LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate height levels");
                boost::shared_ptr<ToVLevelConverter> presConv = getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt);
                heightConv = boost::shared_ptr<ToVLevelConverter>(new PressureToStandardHeightConverter(presConv));
            }
        } else {
            LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate height levels");
            boost::shared_ptr<ToVLevelConverter> presConv = getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt);
            heightConv = boost::shared_ptr<ToVLevelConverter>(new PressureToStandardHeightConverter(presConv));
        }
    }
    return heightConv;
}

LnPressureToPressureConverter::LnPressureToPressureConverter(double p0, const vector<double>& lnP) : pres_(lnP.size())
{
    mifi_atmosphere_ln_pressure(lnP.size(), p0, &lnP[0], &pres_[0]);
}

HeightStandardToPressureConverter::HeightStandardToPressureConverter(const vector<double>& h) : pres_(h.size())
{
    mifi_barometric_standard_pressure(h.size(), &h[0], &pres_[0]);
}

boost::shared_ptr<ToVLevelConverter> ToVLevelConverter::getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nz, size_t nt)
{
    switch (verticalType) {
    case MIFI_VINT_PRESSURE: return getPressureConverter(reader, unLimDimPos, cs, nx, ny, nt);
    case MIFI_VINT_HEIGHT: return getHeightConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    case MIFI_VINT_DEPTH: return getHeightConverter(reader, unLimDimPos, cs, nx, ny, nz, nt);
    default: throw CDMException("unknown vertical type");
    }
}

const vector<double> SigmaToPressureConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> p(sigma_.size());
    mifi_atmosphere_sigma_pressure(sigma_.size(), ptop_, ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &sigma_[0], &p[0]);
    return p;
}

const vector<double> HybridSigmaApToPressureConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> p(ap_.size());
    mifi_atmosphere_hybrid_sigma_ap_pressure(ap_.size(), ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &ap_[0], &b_[0], &p[0]);
    return p;
}


const vector<double> HybridSigmaToPressureConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> p(a_.size());
    mifi_atmosphere_hybrid_sigma_pressure(a_.size(), p0_, ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &a_[0], &b_[0], &p[0]);
    return p;
}


const vector<double> PressureToStandardHeightConverter::operator()(size_t x, size_t y, size_t t) {
    const vector<double> p((*presConv_)(x,y,t));
    vector<double> h(p.size());
    mifi_barometric_standard_height(p.size(), &p[0], &h[0]);
    return h;
}

const vector<double> GeopotentialToHeightConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> h(nz_);
    for (size_t z = 0; z < nz_; z++) {
        float hg = geopot_[((t*nz_ + z)*ny_ + y)*nx_ +x];
        h.at(z) =  static_cast<double>(hg - alti_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)]);
    }
    return h;
}

const vector<double> OceanSCoordinateGToDepthConverter::operator()(size_t x, size_t y, size_t t) {
    vector<double> z(nz_);
    float depth, eta;
    if (timeDependentDepth_) {
        depth = depth_.getDouble(x,y,t);
    } else {
        depth = depth_.getDouble(x,y);
    }
    eta = eta_.getDouble(x,y,t);
    func_(nz_, depth, depth_c_, eta, &s_[0], &C_[0], &z[0]);
    /* z as calculated by formulars is negative down, but we want positive down */
    transform(z.begin(), z.end(), z.begin(), bind1st(multiplies<double>(),-1.));
    return z;
}
bool OceanSCoordinateGToDepthConverter::isValid(double vVal, size_t x, size_t y, size_t t) {
    float depth;
    if (timeDependentDepth_) {
        depth = depth_.getDouble(x,y,t);
    } else {
        depth = depth_.getDouble(x,y);
    }
    return (depth >= vVal);
}


}

