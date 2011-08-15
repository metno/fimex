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
#include <boost/regex.hpp>
#include "fimex/CDMReader.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"
#include "fimex/Logger.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"

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
    boost::shared_ptr<Data> data = reader->getScaledDataSliceInUnit(var, unit, unLimDimPos);
    const boost::shared_array<double> array = data->asConstDouble();
    return vector<double>(&array[0], &array[0] + data->size());
}



boost::shared_ptr<ToVLevelConverter> ToVLevelConverter::getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, const CoordinateSystem::ConstAxisPtr zAxis, size_t nx, size_t ny, size_t nt)
{
    boost::shared_ptr<ToVLevelConverter> presConv;
    switch (zAxis->getAxisType()) {
    case CoordinateAxis::Pressure: {
        boost::shared_ptr<Data> p = reader->getScaledDataSliceInUnit(zAxis->getName(), "hPa", unLimDimPos);
        const boost::shared_array<double> pa = p->asConstDouble();
        presConv = boost::shared_ptr<ToVLevelConverter>(new IdentityToVLevelConverter(vector<double> (&pa[0], &pa[0] + p->size())));
    }
        break;
    case CoordinateAxis::Height: {
        boost::shared_ptr<Data> h = reader->getScaledDataSliceInUnit(zAxis->getName(), "m", unLimDimPos);
        const boost::shared_array<double> ha = h->asConstDouble();
        presConv = boost::shared_ptr<ToVLevelConverter>(new HeightStandardToPressureConverter(vector<double> (&ha[0],&ha[0] + h->size())));
    }
        break;
    case CoordinateAxis::GeoZ: {
        CDMAttribute standardName, formulaTerms;
        if (reader->getCDM().getAttribute(zAxis->getName(), "standard_name", standardName)
                && reader->getCDM().getAttribute(zAxis->getName(), "formula_terms", formulaTerms)) {
            if (standardName.getStringValue() == "atmosphere_hybrid_sigma_pressure_coordinate") {
                // require ap, b, ps(x,y,t)
                string ap = getTerm(formulaTerms, "ap");
                string a = getTerm(formulaTerms, "a");
                string b = getTerm(formulaTerms, "b");
                string ps = getTerm(formulaTerms, "ps");
                if (ap != "") {
                    // formular with ap,b,ps
                    if (b == "" || ps == "")
                        throw CDMException("atmosphere_hybrid_sigma_pressure formula_terms (ap,b,ps) not found in " + formulaTerms.getStringValue());
                    const vector<double> apVec = getDataSliceInUnit(reader, ap, "hPa", unLimDimPos);
                    const vector<double> bVec = getDataSliceInUnit(reader, b, "", unLimDimPos);
                    boost::shared_ptr<Data> psData = reader->getScaledDataSliceInUnit(ps, "hPa", unLimDimPos);
                    if (nx * ny * nt != psData->size()) {
                        throw CDMException("unexpected size of pressure " + ps + "(" + type2string(unLimDimPos) +
                                           "), should be " + type2string(nx * ny * nt) + " != " + type2string(psData->size()));
                    }
                    presConv = boost::shared_ptr<ToVLevelConverter>(new HybridSigmaApToPressureConverter(apVec, bVec, psData->asConstDouble(), nx, ny, nt));
                } else if (a != "") {
                    //formular with a,p0,b,ps
                    string p0 = getTerm(formulaTerms, "p0");
                    if (b == "" || ps == "" || p0 == "")
                        throw CDMException("atmosphere_hybrid_sigma_pressure formula_terms (a,b,p0,ps) not found in " + formulaTerms.getStringValue());
                    const vector<double> aVec = getDataSliceInUnit(reader, a, "", unLimDimPos);
                    const vector<double> bVec = getDataSliceInUnit(reader, b, "", unLimDimPos);
                    const vector<double> p0Vec = getDataSliceInUnit(reader, p0, "hPa", unLimDimPos);
                    boost::shared_ptr<Data> psData = reader->getScaledDataSliceInUnit(ps, "hPa", unLimDimPos);
                    if (nx * ny * nt != psData->size()) {
                        throw CDMException("unexpected size of pressure " + ps + "(" + type2string(unLimDimPos) +
                                           "), should be " + type2string(nx * ny * nt) + " != " + type2string(psData->size()));
                    }
                    presConv = boost::shared_ptr<ToVLevelConverter>(new HybridSigmaToPressureConverter(aVec, bVec, p0Vec.at(0), psData->asConstDouble(), nx, ny, nt));
                } else {
                    throw CDMException("atmosphere_hybrid_sigma_pressure formular-term with 'a' or 'ap' not found in " + formulaTerms.getStringValue());
                }
            } else if (standardName.getStringValue() == "atmosphere_ln_pressure_coordinate") {
                string p0 = getTerm(formulaTerms, "p0");
                string lev = getTerm(formulaTerms, "lev");
                if (p0 == "" || lev == "")
                    throw CDMException("atmosphere_ln_pressure_coordinate formula_term (p0,lev) not found in " + formulaTerms.getStringValue());
                const vector<double> levVec = getDataSliceInUnit(reader, lev, "", unLimDimPos);
                const vector<double> p0Vec = getDataSliceInUnit(reader, p0, "hPa", unLimDimPos);
                presConv = boost::shared_ptr<ToVLevelConverter>(new LnPressureToPressureConverter(p0Vec.at(0), levVec));
            } else if (standardName.getStringValue() == "atmosphere_sigma_coordinate") {
                string sigma = getTerm(formulaTerms, "sigma");
                string ptop = getTerm(formulaTerms, "ptop");
                string ps = getTerm(formulaTerms, "ps");
                if (ptop == "" || sigma == "" || ps == "")
                    throw CDMException("atmosphere_sigma_coordinate formula_terms (ptop,sigma,ps) not all found in " + formulaTerms.getStringValue());
                const vector<double> sigmaVec = getDataSliceInUnit(reader, sigma, "", unLimDimPos);
                const vector<double> ptopVec = getDataSliceInUnit(reader, ptop, "hPa", unLimDimPos);
                boost::shared_ptr<Data> psData = reader->getScaledDataSliceInUnit(ps, "hPa", unLimDimPos);
                if (nx * ny * nt != psData->size()) {
                    throw CDMException("unexpected size of pressure " + ps + "(" + type2string(unLimDimPos) +
                                       "), should be " + type2string(nx * ny * nt) + " != " + type2string(psData->size()));
                }
                presConv = boost::shared_ptr<ToVLevelConverter>(new SigmaToPressureConverter(sigmaVec, ptopVec[0], psData->asConstDouble(), nx, ny, nt));
            } else {
                throw CDMException("unimplemented vertical axis with standard_name: " + standardName.getStringValue());
            }

        } else {
            throw CDMException("no standard_name or formular_terms for vertical axis: " + zAxis->getName());
        }
    }
        break;
    default:
        throw CDMException("unknown vertical coordinate type:" + type2string(zAxis->getAxisType()));
    }
    return presConv;
}

boost::shared_ptr<ToVLevelConverter> ToVLevelConverter::getHeightConverter(
        const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos,
        const CoordinateSystem::ConstAxisPtr xAxis,
        const CoordinateSystem::ConstAxisPtr yAxis,
        const CoordinateSystem::ConstAxisPtr zAxis, size_t nx, size_t ny,
        size_t nz, size_t nt)
{
    boost::shared_ptr<ToVLevelConverter> heightConv;
    switch (zAxis->getAxisType()) {
        case CoordinateAxis::GeoZ:
        {
            boost::shared_ptr<Data> hd = reader->getScaledDataSliceInUnit(zAxis->getName(), "m", unLimDimPos);
            const boost::shared_array<double> ha = hd->asConstDouble();
            heightConv = boost::shared_ptr<ToVLevelConverter>(new IdentityToVLevelConverter(vector<double> (&ha[0], &ha[0] + hd->size())));
        }
        break;
    default:
        { // try geopotential_height or fall back to pressure
            map<string, string> attrs;
            vector<string> dims;
            dims.push_back(xAxis->getShape()[0]);
            dims.push_back(yAxis->getShape()[0]);
            dims.push_back(zAxis->getShape()[0]);
            attrs["standard_name"] = "geopotential_height";
            vector<string> geoVars = reader->getCDM().findVariables(attrs, dims);
            if (geoVars.size() > 0) {
                LOG4FIMEX(logger, Logger::INFO, "using geopotential height "<<geoVars[0]<<" to retrieve height");
                boost::shared_ptr<Data> geoPotData = reader->getScaledDataSliceInUnit(geoVars[0], "m", unLimDimPos);
                if (geoPotData->size() != (nx * ny * nz * nt))
                    throw CDMException("geopotential height '" + geoVars[0] + "' has strange size: " + type2string(geoPotData->size()) + " != " + type2string(nx * ny * nz * nt));
                heightConv = boost::shared_ptr<ToVLevelConverter>(new GeopotentialToHeightConverter(geoPotData->asConstDouble(), nx, ny, nz, nt));
            } else {
                LOG4FIMEX(logger, Logger::INFO, "using pressure and standard atmosphere to estimate height levels");
                boost::shared_ptr<ToVLevelConverter> presConv = getPressureConverter(reader, unLimDimPos, zAxis, nx, ny, nt);
                heightConv = boost::shared_ptr<ToVLevelConverter>(new PressureToStandardHeightConverter(presConv));
            }
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

boost::shared_ptr<ToVLevelConverter> ToVLevelConverter::getConverter(const boost::shared_ptr<CDMReader>& reader, int verticalType, size_t unLimDimPos, const CoordinateSystem::ConstAxisPtr xAxis, const CoordinateSystem::ConstAxisPtr yAxis, const CoordinateSystem::ConstAxisPtr zAxis, size_t nx, size_t ny, size_t nz, size_t nt)
{
    switch (verticalType) {
    case MIFI_VINT_PRESSURE: return getPressureConverter(reader, unLimDimPos, zAxis, nx, ny, nt);
    case MIFI_VINT_HEIGHT: return getHeightConverter(reader, unLimDimPos, xAxis, yAxis, zAxis, nx, ny, nz, nt);
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
        double hg = geopot_[((t*nz_ + z)*ny_ + y)*nx_ +x];
        // correction for high levels (1/r^2 dependency of g => 1/r dependency of hg (integral of 1/r^2))
        h.at(z) =  hg * (MIFI_EARTH_RADIUS_M  / (MIFI_EARTH_RADIUS_M - hg));
//        if (x == 0 && y == 0 && t == 0)
//            cerr << z << ": " << hg << " ... " << h.at(z) << "\n";
    }
    return h;
}


}
