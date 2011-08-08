/*
 * Fimex, CDMVerticalInterpolator.cc
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
 *  Created on: Aug 1, 2011
 *      Author: Heiko Klein
 */

#include "fimex/CDMVerticalInterpolator.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CDM.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include <boost/regex.hpp>
#include <iterator>
#include <algorithm>
#include <string>
#include <vector>

namespace MetNoFimex
{
static LoggerPtr logger = getLogger("fimex.CDMVerticalInterpolator");

using namespace std;

typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
struct VIntPimpl {
    int verticalType;
    int verticalInterpolationMethod;
    vector<double> level1;
    vector<double> level2;
    // name of the generated vertical axis
    string vAxis;
    // variable-names with vertical information to change
    vector<CoordSysPtr> changeCoordSys;
};

string findUniqueDimVarName(const CDM& cdm, string baseVar)
{
    // find a unique variable name
    if (!(cdm.hasVariable(baseVar) || cdm.hasDimension(baseVar))) {
        return baseVar;
    } else {
        for (size_t i = 0; i < 9; i++) {
            string varName = baseVar + type2string(i);
            if (!(cdm.hasVariable(varName) || cdm.hasDimension(varName))) {
                return varName;
            }
        }
    }
    throw CDMException("unable to generate new dimension/variable name starting with "+ baseVar);
}

CDMVerticalInterpolator::CDMVerticalInterpolator(boost::shared_ptr<CDMReader> dataReader, string verticalType, string verticalInterpolationMethod, const std::vector<double> level1, const std::vector<double> level2)
: dataReader_(dataReader), pimpl_(new VIntPimpl())
{
    *cdm_ = dataReader->getCDM();
    const CDM::VarVec& variables = cdm_->getVariables();
    // remove all data associated with this cdm - either it will be set below
    // or it can be retrieved from the dataReader
    for (CDM::VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
        cdm_->getVariable(it->getName()).setData(boost::shared_ptr<Data>());
    }

    if (verticalType == "pressure") {
        pimpl_->verticalType = MIFI_VINT_PRESSURE;
    } else {
        throw CDMException("unknown vertical type: " + verticalType);
    }
    if (verticalInterpolationMethod == "linear") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LIN;
    } else if (verticalInterpolationMethod == "log") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LOG;
    } else if (verticalInterpolationMethod == "loglog") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LOGLOG;
    } else {
        throw CDMException("unknown vertical interpolation method: " +verticalInterpolationMethod);
    }
    copy(level1.begin(), level1.end(), back_inserter(pimpl_->level1));
    copy(level2.begin(), level2.end(), back_inserter(pimpl_->level2));

    // allocate a new vertical axis
    switch (pimpl_->verticalType)
    {
        case MIFI_VINT_PRESSURE:
            pimpl_->vAxis = findUniqueDimVarName(dataReader->getCDM(), "pressure");
            {
                cdm_->addDimension(CDMDimension(pimpl_->vAxis, level1.size()));
                CDMVariable var(pimpl_->vAxis, CDM_DOUBLE, vector<string>(1, pimpl_->vAxis));
                boost::shared_array<double> pres(new double[level1.size()]);
                copy(level1.begin(), level1.end(), &pres[0]);
                var.setData(createData(level1.size(), pres));
                cdm_->addVariable(var);
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "hPa"));
            }
            break;
        default:
            LOG4FIMEX(logger, Logger::ERROR, "undefined verticalType: " << verticalType);
            throw CDMException("undefined vertical type: "+type2string(verticalType));
    }

    typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
    // get all coordinate systems from file
    vector<CoordSysPtr> coordSys = listCoordinateSystems(dataReader_->getCDM());
    for (size_t i = 0; i < coordSys.size(); i++) {
        CoordinateSystem::ConstAxisPtr xAxis = coordSys[i]->getGeoXAxis();
        CoordinateSystem::ConstAxisPtr yAxis = coordSys[i]->getGeoYAxis();
        CoordinateSystem::ConstAxisPtr zAxis = coordSys[i]->getGeoZAxis();
        // require x and y axis (ps(x,y)) and obviously zAxis
        if (xAxis != 0 && yAxis != 0 && zAxis != 0) {
            pimpl_->changeCoordSys.push_back(coordSys[i]);
            // change the shape of the variable: remove the old axis, add the new one
            const CDM::VarVec vars = dataReader_->getCDM().getVariables();
            for (CDM::VarVec::const_iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
                if (coordSys[i]->isCSFor(varIt->getName()) && coordSys[i]->isComplete(varIt->getName())) {
                    CDMVariable& var = cdm_->getVariable(varIt->getName());
                    vector<string> shape = var.getShape();
                    vector<string>::iterator axisPos = find(shape.begin(), shape.end(), zAxis->getName());
                    if (axisPos != shape.end()) {
                        // change to new axis
                        *axisPos = pimpl_->vAxis;
                        var.setShape(shape);
                        // some people set explicit axis to implicit axis (coordinates in CF)
                        // remove those
                        CDMAttribute coord;
                        if (cdm_->getAttribute(varIt->getName(), "coordinates", coord)) {
                            string coords = coord.getData()->asString();
                            coords = boost::regex_replace(coords, boost::regex("\\b\\Q"+zAxis->getName()+"\\E\\b"), "");
                            cdm_->removeAttribute(varIt->getName(), "coordinates");
                            cdm_->addAttribute(varIt->getName(), CDMAttribute("coordinates", coords));
                        }
                    } else {
                        throw CDMException("axis in complete coordinate system for variable '"+ varIt->getName() +"' but not in shape");
                    }
                }
            }

        }
    }




}

CDMVerticalInterpolator::~CDMVerticalInterpolator()
{
    // auto-destruction
}

boost::shared_ptr<Data> CDMVerticalInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(pimpl_->changeCoordSys.begin(), pimpl_->changeCoordSys.end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt == pimpl_->changeCoordSys.end()) {
        // no level to change, propagate to the dataReader_
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }

    switch (pimpl_->verticalType) {
    case MIFI_VINT_PRESSURE: return getPressureDataSlice(*varSysIt, varName, unLimDimPos);
    default: throw CDMException("unknown vertical type");
    }
    // should never go below this
    assert(false);
}


// TODO: move those classes to header-file
// TODO: implement those classe for all vertical coordinate transformations
class ToPressureConverter {
public:
    virtual ~ToPressureConverter() {}
    /**
     * @param x
     * @param y
     * @param t
     * @return pressure-levels in hPa at position (x,y,t)
     */
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) = 0;
};
class PressureToPressureConverter : public ToPressureConverter {
    const vector<double> pres_;
public:
    PressureToPressureConverter(const vector<double>& pressure) : pres_(pressure.begin(), pressure.end()) {}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {return pres_;}
};
class HybridSigmaApToPressureConverter : public ToPressureConverter {
    const vector<double> ap_;
    const vector<double> b_;
    const boost::shared_array<double> ps_;
    size_t nx_;
    size_t ny_;
    size_t nt_;
public:
    HybridSigmaApToPressureConverter(const vector<double>& ap, const vector<double>& b, const boost::shared_array<double> ps, size_t nx, size_t ny, size_t nt)
    : ap_(ap.begin(), ap.end()), b_(b.begin(), b.end()), ps_(ps), nx_(nx), ny_(ny), nt_(nt) {}
    virtual const vector<double> operator()(size_t x, size_t y, size_t t) {
        vector<double> p(ap_.size());
        mifi_atmosphere_hybrid_sigma_ap_pressure(ap_.size(), ps_[mifi_3d_array_position(x,y,t,nx_,ny_,nt_)], &ap_[0], &b_[0], &p[0]);
        return p;
    }
};


boost::shared_ptr<Data> CDMVerticalInterpolator::getPressureDataSlice(boost::shared_ptr<const CoordinateSystem> cs, const std::string& varName, size_t unLimDimPos)
{
    assert(cs->isCSFor(varName) && cs->isComplete(varName));
    // get all axes
    CoordinateSystem::ConstAxisPtr zAxis = cs->getGeoZAxis();
    assert(zAxis.get() != 0); // defined by construction of cs
    size_t nz = 1;
    {
        const vector<string>& shape = zAxis->getShape();
        if (shape.size() == 1) {
            nz = cdm_->getDimension(shape.at(0)).getLength();
        } else {
            throw CDMException("vertical interpolation not possible with 2d z-Axis");
        }
    }
    // detect x and y axis
    size_t nx = 1;
    CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
    if (xAxis.get() != 0) {
        const vector<string>& shape = xAxis->getShape();
        if (shape.size() == 1) {
            nx = cdm_->getDimension(shape.at(0)).getLength();
        } else {
            throw CDMException("vertical interpolation not possible with 2d x-Axis");
        }
    }
    size_t ny = 1;
    CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
    if (yAxis.get() != 0) {
        const vector<string>& shape = yAxis->getShape();
        if (shape.size() == 1) {
            ny = cdm_->getDimension(shape.at(0)).getLength();
        } else {
            throw CDMException("vertical interpolation not possible with 2d y-Axis");
        }
    }

    // detect time axis
    size_t nt = 1;
    size_t startT = 0;
    CoordinateSystem::ConstAxisPtr tAxis = cs->getTimeAxis();
    if (tAxis.get() != 0) {
        const vector<string>& shape = tAxis->getShape();
        if (shape.size() == 1) {
            const CDMDimension& tDim = cdm_->getDimension(shape.at(0));
            if (tDim.isUnlimited()) {
                // artifically creating a loop of size 1 at correct position
                nt = unLimDimPos + 1;
                startT = unLimDimPos;
            } else {
                nt = tDim.getLength();
                startT = 0;
            }
        } else {
            throw CDMException(
                    "vertical interpolation not possible with 2d time-axis");
        }
    }

    boost::shared_ptr<ToPressureConverter> presConv;
    switch (zAxis->getAxisType()) {
    // TODO get the different conversions to pressure
    case CoordinateAxis::Pressure:
        {
            boost::shared_ptr<Data> p = dataReader_->getScaledDataSliceInUnit(zAxis->getName(), "hPa", unLimDimPos);
            const boost::shared_array<double> pa = p->asConstDouble();
            presConv = boost::shared_ptr<ToPressureConverter>(new PressureToPressureConverter(vector<double>(&pa[0], &pa[0]+p->size())));
        }
        break;
    case CoordinateAxis::Height: assert(false); break; // TODO
    case CoordinateAxis::GeoZ:
        {
            CDMAttribute standardName;
            if (cdm_->getAttribute(zAxis->getName(), "standard_name", standardName)) {
                CDMAttribute formulaTerms;
                if (cdm_->getAttribute(zAxis->getName(), "formula_terms", formulaTerms)) {
                    if (standardName.getStringValue() == "atmosphere_hybrid_sigma_pressure_coordinate") {
                        // require ap, b, ps(x,y,t)
                        boost::smatch what;
                        string ap, b, ps;
                        if (boost::regex_match(formulaTerms.getStringValue(), what, boost::regex(".*b:\\s*(\\S+).*"))) {
                            b = what[1];
                        } else {
                            throw CDMException("atmosphere_hybrid_sigma_pressure formular-term b not found in " + formulaTerms.getStringValue());
                        }
                        if (boost::regex_match(formulaTerms.getStringValue(), what, boost::regex(".*ps:\\s*(\\S+).*"))) {
                            ps = what[1];
                        } else {
                            throw CDMException("atmosphere_hybrid_sigma_pressure formular-term ps not found in " + formulaTerms.getStringValue());
                        }
                        if (boost::regex_match(formulaTerms.getStringValue(), what, boost::regex(".*ap:\\s*(\\S+).*"))) {
                            ap = what[1];
                        } else {
                            throw CDMException("atmosphere_hybrid_sigma_pressure formular-term ap not found in " + formulaTerms.getStringValue());
                        }
                        boost::shared_ptr<Data> apData = dataReader_->getScaledDataInUnit(ap, "hPa");
                        const boost::shared_array<double> apAry = apData->asConstDouble();
                        vector<double> apVec(&apAry[0], &apAry[0]+apData->size());
                        boost::shared_ptr<Data> bData = dataReader_->getScaledData(b);
                        const boost::shared_array<double> bAry = bData->asConstDouble();
                        vector<double> bVec(&bAry[0], &bAry[0]+bData->size());
                        boost::shared_ptr<Data> psData = dataReader_->getScaledDataSliceInUnit(ps, "hPa", unLimDimPos);
                        if (nx*ny*(nt-startT) != psData->size()) {
                            throw CDMException("unexpected size of pressure "+ps+"("+type2string(unLimDimPos)+") for variable "+varName+", should be "+type2string(nx*ny*(nt-startT))+ " != " +type2string(psData->size()));
                        }
                        presConv = boost::shared_ptr<ToPressureConverter>(new HybridSigmaApToPressureConverter(apVec, bVec, psData->asConstDouble(), nx, ny, nt-startT));
                        vector<double> test = (*presConv)(0,0,0);
                        for (int i = 0; i < test.size(); ++i) {
                            cerr << i << ": " << test[i] << endl;
                        }
                        // or require a, b, ps(x,y,t), p0
                    } else {
                        throw CDMException("unimplemented vertical axis with standard_name: " + standardName.getStringValue());
                    }

                } else {
                    throw CDMException("no formular_terms for vertical axis: " + zAxis->getName());
                }
            } else {
                throw CDMException("no standard_name for vertical axis: " + zAxis->getName());
            }
        }
        break;
    default: throw CDMException("unknown vertical coordinate type:" + type2string(zAxis->getAxisType()));// TODO: dimensionless coordinate axis
    }

    int (*intFunc)(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x) = 0;
    switch (pimpl_->verticalInterpolationMethod) {
    case MIFI_VINT_METHOD_LIN: intFunc = &mifi_get_values_linear_f; break;
    case MIFI_VINT_METHOD_LOG: intFunc = &mifi_get_values_log_f; break;
    case MIFI_VINT_METHOD_LOGLOG: intFunc = &mifi_get_values_log_log_f; break;
    default: assert(false);
    }

    vector<double>& pOut = pimpl_->level1;
    boost::shared_ptr<Data> data = dataReader_->getDataSlice(varName, unLimDimPos);
    if (data->size() != (nx*ny*nz*(nt-startT))) {
        throw CDMException("unexpected dataslice of variable " + varName +": (nx*ny*nz*nt) = (" +
                           type2string(nx)+"*"+type2string(ny)+"*"+type2string(nz)+"*"+type2string(nt-startT)+
                           ") != " + type2string(data->size()));
    }
    const boost::shared_array<float> iData = data->asConstFloat();
    boost::shared_array<float> oData(new float[nx*ny*pOut.size()*(nt-startT)]);

    for (size_t t = startT; t < nt; ++t) {
        float* inData = &iData[0];
        float* outData = &oData[0];
        size_t pTimePos = 1; // unLimited, just one xy-slice
        if ((nt-startT) > 1) {
            // move to start of time slice
            inData = &iData[t*(nx*ny*nz)];
            outData = &oData[t*(nx*ny*pOut.size())];
            pTimePos = t; // move to correct slice
        }
        for (size_t y = 0; y < ny; ++y) {
            for (size_t x = 0; x < nx; ++x) {
                // interpolate in between the pressure values
                vector<double> pIn = (*presConv)(x, y, pTimePos);
                if (pIn.size() != nz) {
                    throw CDMException("input pressure level size: "
                            + type2string(pIn.size()) + " must be " + type2string(nz));
                }
                // pIn should be growing (top (e.g. pres=10 to bottom pres=1000)
                bool reversePIn = false;
                if ((pIn[1] - pIn[0]) < 0) {
                    reversePIn = true;
                    reverse(pIn.begin(), pIn.end());
                }
                for (size_t k = 0; k < pOut.size(); k++) {
                    // find element
                    vector<double>::iterator ub = upper_bound(pIn.begin(),
                            pIn.end(), pOut[k]);
                    size_t ubPos = distance(pIn.begin(), ub);
                    if (ubPos <= 1) {
                        // possibly ekstrapolation before pIn[0]
                        ubPos = 1;
                    } else if (ub == pIn.end()) {
                        // surely ekstrapolation to pIn[pIn.size()-1]
                        ubPos = pIn.size() - 1;
                    } else {
                        // intrapolation
                    }
                    size_t elUbPos = ubPos;
                    size_t elUbPosM1 = ubPos - 1;
                    if (reversePIn) {
                        elUbPos = pIn.size() - 1 - ubPos;
                        elUbPosM1 = pIn.size() - 1 - (ubPos - 1);
                    }
                    size_t inPos = mifi_3d_array_position(x, y, elUbPos, nx,
                            ny, nz);
                    size_t inPosM1 = mifi_3d_array_position(x, y, elUbPosM1,
                            nx, ny, nz);
                    size_t outPos = mifi_3d_array_position(x, y, k, nx, ny,
                            pOut.size());
                    (*intFunc)(&inData[inPos], &inData[inPosM1], &outData[outPos],
                            1, pIn.at(ubPos), pIn.at(ubPos - 1), pOut.at(k));
                }
            }
        }
    }
    return createData(nx*ny*pOut.size()*(nt-startT), oData);
}

}
