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
#include "fimex/CDM.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "coordSys/CoordSysUtils.h"
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


/* TODO: see CDMInterpolator.cc, deduplicate */
static boost::shared_array<float> data2InterpolationArray(const DataPtr& inData, double badValue) {
    boost::shared_array<float> array = inData->asFloat();
    mifi_bad2nanf(&array[0], &array[inData->size()], badValue);
    return array;
}

// for performance reasons, the iData-reference will be modified and used within the return data
static DataPtr interpolationArray2Data(boost::shared_array<float> iData, size_t size, double badValue) {
    mifi_nanf2bad(&iData[0], &iData[size], badValue);
    return createData(size, iData);
}


CDMVerticalInterpolator::CDMVerticalInterpolator(boost::shared_ptr<CDMReader> dataReader, const string& verticalType, const string& verticalInterpolationMethod, const std::vector<double>& level1, const std::vector<double>& level2)
: dataReader_(dataReader), pimpl_(new VIntPimpl())
{
    typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
    // get all coordinate systems from file before changing this cdm
    vector<CoordSysPtr> coordSys = listCoordinateSystems(dataReader_);

    *cdm_ = dataReader->getCDM();
    const CDM::VarVec& variables = cdm_->getVariables();
    // remove all data associated with this cdm - either it will be set below
    // or it can be retrieved from the dataReader
    for (CDM::VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
        cdm_->getVariable(it->getName()).setData(DataPtr());
    }

    if (verticalType == "pressure") {
        pimpl_->verticalType = MIFI_VINT_PRESSURE;
    } else if (verticalType == "height") {
        pimpl_->verticalType = MIFI_VINT_HEIGHT;
    } else if (verticalType == "depth") {
        pimpl_->verticalType = MIFI_VINT_DEPTH;
    } else {
        throw CDMException("unknown vertical type: " + verticalType);
    }
    if (verticalInterpolationMethod == "linear") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LIN;
    } else if (verticalInterpolationMethod == "log") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LOG;
    } else if (verticalInterpolationMethod == "loglog") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LOGLOG;
    } else if (verticalInterpolationMethod == "nearestneighbor") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_NN;
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
        case MIFI_VINT_HEIGHT:
            pimpl_->vAxis = findUniqueDimVarName(dataReader->getCDM(), "height");
            {
                cdm_->addDimension(CDMDimension(pimpl_->vAxis, level1.size()));
                CDMVariable var(pimpl_->vAxis, CDM_DOUBLE, vector<string>(1, pimpl_->vAxis));
                boost::shared_array<double> height(new double[level1.size()]);
                copy(level1.begin(), level1.end(), &height[0]);
                var.setData(createData(level1.size(), height));
                cdm_->addVariable(var);
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "m"));
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("positive", "up"));
            }
            break;
        case MIFI_VINT_DEPTH:
            pimpl_->vAxis = findUniqueDimVarName(dataReader->getCDM(), "depth");
            {
                cdm_->addDimension(CDMDimension(pimpl_->vAxis, level1.size()));
                CDMVariable var(pimpl_->vAxis, CDM_DOUBLE, vector<string>(1, pimpl_->vAxis));
                boost::shared_array<double> height(new double[level1.size()]);
                copy(level1.begin(), level1.end(), &height[0]);
                var.setData(createData(level1.size(), height));
                cdm_->addVariable(var);
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "m"));
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("positive", "down"));
            }
            break;
        default:
            LOG4FIMEX(logger, Logger::ERROR, "undefined verticalType: " << verticalType);
            throw CDMException("undefined vertical type: "+type2string(verticalType));
    }

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
                        // some people set explicit axis to implicit axis (coordinates in CF) - remove those!
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
            // remove the old zAxis
            cdm_->removeVariable(zAxis->getName());
        }
    }
}

CDMVerticalInterpolator::~CDMVerticalInterpolator()
{
    // auto-destruction
}

DataPtr CDMVerticalInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos)
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

    return getLevelDataSlice(*varSysIt, varName, unLimDimPos);
}

DataPtr CDMVerticalInterpolator::getLevelDataSlice(CoordSysPtr cs, const std::string& varName, size_t unLimDimPos)
{
    assert(cs->isCSFor(varName) && cs->isComplete(varName));
    if (! cs->hasVerticalTransformation()) {
        throw CDMException(varName + " has no vertical transformation");
    }
    // get all axes
    CoordinateSystem::ConstAxisPtr xAxis, yAxis, zAxis, tAxis;
    size_t nx, ny, nz, nt, startT;
    getSimpleAxes(cs, dataReader_->getCDM(),
            xAxis, yAxis, zAxis, tAxis,
            nx, ny, nz, startT, nt, unLimDimPos);
    boost::shared_ptr<ToVLevelConverter> levConv = cs->getVerticalTransformation()->getConverter(dataReader_, pimpl_->verticalType, unLimDimPos, cs, nx, ny, nz, (nt-startT));

    int (*intFunc)(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x) = 0;
    switch (pimpl_->verticalInterpolationMethod) {
    case MIFI_VINT_METHOD_LIN: intFunc = &mifi_get_values_linear_f; break;
    case MIFI_VINT_METHOD_LOG: intFunc = &mifi_get_values_log_f; break;
    case MIFI_VINT_METHOD_LOGLOG: intFunc = &mifi_get_values_log_log_f; break;
    case MIFI_VINT_METHOD_NN: intFunc = &mifi_get_values_nearest_f; break;
    default: assert(false); break;
    }

    vector<double>& pOut = pimpl_->level1;
    DataPtr data = dataReader_->getDataSlice(varName, unLimDimPos);
    if (data->size() != (nx*ny*nz*(nt-startT))) {
        throw CDMException("unexpected dataslice of variable " + varName +": (nx*ny*nz*nt) = (" +
                           type2string(nx)+"*"+type2string(ny)+"*"+type2string(nz)+"*"+type2string(nt-startT)+
                           ") != " + type2string(data->size()));
    }
    double badValue = cdm_->getFillValue(varName);
    boost::shared_array<float> iData = data2InterpolationArray(data, badValue);
    size_t oSize = nx*ny*pOut.size()*(nt-startT);
    boost::shared_array<float> oData(new float[oSize]);

    // loop over data-array, interpolating cell for cell
    for (size_t t = startT; t < nt; ++t) {
        size_t timePos = ((nt-startT) > 1) ? t : 0; // multi-time (t) or one time slice (0)
        const float* inData = &iData[timePos*(nx*ny*nz)];
        float* outData = &oData[timePos*(nx*ny*pOut.size())];
        for (size_t y = 0; y < ny; ++y) {
            for (size_t x = 0; x < nx; ++x) {
                // interpolate in between the pressure values
                vector<double> pIn = (*levConv)(x, y, timePos);
                if (pIn.size() != nz) {
                    throw CDMException("input level size: "
                            + type2string(pIn.size()) + " must be " + type2string(nz));
                }
                for (size_t k = 0; k < pOut.size(); k++) {
                    size_t outPos = mifi_3d_array_position(x, y, k, nx, ny, pOut.size());
                    if (levConv->isValid(pOut[k], x, y, t)) {
                        pair<size_t, size_t> pos = find_closest_neighbor_distinct_elements(pIn.begin(), pIn.end(), pOut[k]);
                        size_t inPos1 = mifi_3d_array_position(x, y, pos.first, nx, ny, nz);
                        size_t inPos2 = mifi_3d_array_position(x, y, pos.second, nx, ny, nz);
                        intFunc(&inData[inPos1], &inData[inPos2], &outData[outPos],
                                1, pIn.at(pos.first), pIn.at(pos.second), pOut.at(k));
                    } else {
                        outData[outPos] = MIFI_UNDEFINED_D;
                    }
#if 0
                    // bad input or extrapolation
                    if (varName == "relative_humidity" && outData[outPos] > 100) {
                        cerr << pIn.at(pos.first) << "," << pIn.at(pos.second) << ": " << pOut.at(k) << endl;
                        cerr << inData[inPos1] << "," << inData[inPos2] << ": " << outData[outPos] << endl;
                        if (pos.first > 0) {
                            cerr << pIn.at(pos.first - 1) << endl;
                        }
                    }
#endif
                }
            }
        }
    }

    // correct data going out of bounds
    if (!isnan(cdm_->getValidMin(varName))) {
        float minVal = static_cast<float>(cdm_->getValidMin(varName));
        replace_if(&oData[0], &oData[0]+oSize, bind2nd(less<float>(), minVal), minVal);
    }
    if (!isnan(cdm_->getValidMax(varName))) {
        float maxVal = static_cast<float>(cdm_->getValidMax(varName));
        replace_if(&oData[0], &oData[0]+oSize, bind2nd(greater<float>(), maxVal), maxVal);
    }

    return interpolationArray2Data(oData, nx*ny*pOut.size()*(nt-startT), badValue);
}


}
