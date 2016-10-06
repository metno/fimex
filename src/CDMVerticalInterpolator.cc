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
#include "fimex/CDMInterpolator.h" // for data <-> interpolationArray
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "coordSys/CoordSysUtils.h"

#include "ArrayLoop.h"
#include "leap_iterator.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

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
    mifi_vertical_interpol_method verticalInterpolationMethod;
    vector<double> level1;
    vector<double> level2;
    // name of the generated vertical axis
    string vAxis;
    // variable-names with vertical information to change
    vector<CoordSysPtr> changeCoordSys;
};

CDMVerticalInterpolator::CDMVerticalInterpolator(boost::shared_ptr<CDMReader> dataReader, const string& verticalType, const string& verticalInterpolationMethod,
                                                 const std::vector<double>& level1, const std::vector<double>& level2)
    : dataReader_(dataReader), pimpl_(new VIntPimpl())
{
    typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
    // get all coordinate systems from file before changing this cdm
    const vector<CoordSysPtr> coordSys = listCoordinateSystems(dataReader_);

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
    } else if (verticalType == "altitude") {
        pimpl_->verticalType = MIFI_VINT_ALTITUDE;
    } else if (verticalType == "depth") {
        pimpl_->verticalType = MIFI_VINT_DEPTH;
    } else {
        throw CDMException("unknown vertical type: " + verticalType);
    }
    if (verticalInterpolationMethod == "linear") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LIN;
    } else if (verticalInterpolationMethod == "linear_weak_extra") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LIN_WEAK_EXTRA;
    } else if (verticalInterpolationMethod == "linear_no_extra") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LIN_NO_EXTRA;
    } else if (verticalInterpolationMethod == "linear_const_extra") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LIN_CONST_EXTRA;
    } else if (verticalInterpolationMethod == "log") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LOG;
    } else if (verticalInterpolationMethod == "loglog") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_LOGLOG;
    } else if (verticalInterpolationMethod == "nearestneighbor") {
        pimpl_->verticalInterpolationMethod = MIFI_VINT_METHOD_NN;
    } else {
        throw CDMException("unknown vertical interpolation method: " +verticalInterpolationMethod);
    }
    pimpl_->level1 = level1;
    pimpl_->level2 = level2;

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
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("long_name", "height above ground"));
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "m"));
                cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("positive", "up"));
            }
            break;
        case MIFI_VINT_ALTITUDE:
            pimpl_->vAxis = findUniqueDimVarName(dataReader->getCDM(), "height_above_msl");
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
            // ignore strange and/or size=1 zAxes
            if (zAxis->getShape().size() == 1 && zAxis->isExplicit() &&
                    cdm_->hasDimension(zAxis->getShape().at(0)) &&
                    cdm_->getDimension(zAxis->getShape().at(0)).getLength() > 1)
            {
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
    CoordSysPtr cs = findCompleteCoordinateSystemFor(pimpl_->changeCoordSys, varName);
    if (cs.get() == 0) {
        // no level to change, propagate to the dataReader_
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }

    return getLevelDataSlice(cs, varName, unLimDimPos);
}

DataPtr CDMVerticalInterpolator::getLevelDataSlice(CoordSysPtr cs, const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getLevelDataSlice(.. '" << varName << "' ..)");
    assert(cs->isCSAndCompleteFor(varName));
    if (!cs->hasVerticalTransformation()) {
        throw CDMException(varName + " has no vertical transformation");
    }

    VerticalConverterPtr converter = verticalConverter(cs, dataReader_, pimpl_->verticalType);
    DataPtr verticalData = verticalData4D(converter, dataReader_->getCDM(), unLimDimPos);

    const vector<double>& pOut = pimpl_->level1;

    int (*intFunc)(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x) = 0;
    switch (pimpl_->verticalInterpolationMethod) {
    case MIFI_VINT_METHOD_LIN: intFunc = &mifi_get_values_linear_f; break;
    case MIFI_VINT_METHOD_LIN_WEAK_EXTRA: intFunc = &mifi_get_values_linear_weak_extrapol_f; break;
    case MIFI_VINT_METHOD_LIN_NO_EXTRA: intFunc = &mifi_get_values_linear_no_extrapol_f; break;
    case MIFI_VINT_METHOD_LIN_CONST_EXTRA: intFunc = &mifi_get_values_linear_const_extrapol_f; break;
    case MIFI_VINT_METHOD_LOG: intFunc = &mifi_get_values_log_f; break;
    case MIFI_VINT_METHOD_LOGLOG: intFunc = &mifi_get_values_log_log_f; break;
    case MIFI_VINT_METHOD_NN: intFunc = &mifi_get_values_nearest_f; break;
    default: assert(false); break;
    }

    const std::string& geoZi = cs->getGeoZAxis()->getName();
    const std::string& geoZo = pimpl_->vAxis;

    ArrayDims siData = makeArrayDims(dataReader_->getCDM(), varName);
    ArrayDims siVertical = makeArrayDims(dataReader_->getCDM(), converter);
    ArrayDims soData = makeArrayDims(getCDM(), varName);
    ArrayDims svData;

    set_not_shared(geoZi, siData, siVertical);
    set_not_shared(geoZo, soData);
    forceUnLimDimLength1(getCDM(), siData, siVertical, soData);

    enum { IN, VERTICAL, OUT, VALID }; // VALID must be last, it is optional
    ArrayGroup group = ArrayGroup().add(siData).add(siVertical).add(soData);
    group.minimizeShared(0); // we have to treat each value separately

    boost::shared_array<unsigned char> validValues;
    {
        const std::vector<std::string> validShape = converter->getValidityShape(geoZo);
        const SliceBuilder sbValid = createSliceBuilder(getCDM(), validShape);
        DataPtr validData = converter->getValiditySlice(sbValid, pOut);
        if (validData) {
            svData = makeArrayDims(getCDM(), validShape);
            set_not_shared(geoZo, svData);
            forceUnLimDimLength1(getCDM(), svData);

            group.add(svData);

            validValues = validData->asUChar();
        }
    }

    const size_t nzi = siData.length(geoZi);

    const size_t idataZdelta = siData.delta(geoZi);
    const size_t iverticalZdelta = siVertical.delta(geoZi);
    const size_t odataZdelta = soData.delta(geoZo);
    const size_t vdataZdelta = svData.delta(geoZo);

    DataPtr data = dataReader_->getDataSlice(varName, unLimDimPos);
    const double badValue = cdm_->getFillValue(varName);
    boost::shared_array<float> iData = data2InterpolationArray(data, badValue);
    const size_t oSize = soData.volume();
    boost::shared_array<float> oData(new float[oSize]);
    boost::shared_array<float> verticalValues = verticalData->asFloat();

#ifdef _OPENMP
#pragma omp parallel for default(shared)
#endif
    for (size_t k = 0; k < pOut.size(); k++) {
        const double verticalOut = pOut[k];
        Loop loop(group);
        do { // sharedVolume() == 1 because we called minimizeShared before
            float* interpolated = &oData[loop[OUT] + k*odataZdelta];

            if (!validValues || validValues[loop[VALID] + k*vdataZdelta]) {
                const leap_iterator<const float*> vertical0(&verticalValues[loop[VERTICAL]], iverticalZdelta);
                const leap_iterator<const float*> vertical1 = vertical0 + nzi;
                const pair<size_t, size_t> pos = find_closest_neighbor_distinct_elements(vertical0, vertical1, verticalOut);

                const size_t idataZ0  = loop[IN] + idataZdelta * pos.first;
                const size_t idataZ1  = loop[IN] + idataZdelta * pos.second;
                intFunc(&iData[idataZ0], &iData[idataZ1], interpolated,
                        1, *(vertical0 + pos.first), *(vertical0 + pos.second), verticalOut);
            } else {
                *interpolated = MIFI_UNDEFINED_F;
            }
        } while (loop.next());
    }

    // correct data going out of bounds
    const double valid_min = cdm_->getValidMin(varName);
    const double valid_max = cdm_->getValidMax(varName);
    if (!isnan(valid_min)) {
        float minVal = static_cast<float>(valid_min);
        replace_if(&oData[0], &oData[0]+oSize, bind2nd(less<float>(), minVal), minVal);
    }
    if (!isnan(valid_max)) {
        float maxVal = static_cast<float>(valid_max);
        replace_if(&oData[0], &oData[0]+oSize, bind2nd(greater<float>(), maxVal), maxVal);
    }

    CDMDataType oType = getCDM().getVariable(varName).getDataType();
    return interpolationArray2Data(oType, oData, soData.volume(), badValue);
}

}
