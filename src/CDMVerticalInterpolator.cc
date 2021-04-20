/*
 * Fimex, CDMVerticalInterpolator.cc
 *
 * (C) Copyright 2011-2021, met.no
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

#include "fimex/CDM.h"
#include "fimex/CDMInterpolator.h" // for data <-> interpolationArray
#include "fimex/CDMReaderUtils.h"
#include "fimex/Data.h"
#include "fimex/FindNeighborElements.h"
#include "fimex/Logger.h"
#include "fimex/StringUtils.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"

#include "coordSys/CoordSysUtils.h"

#include "fimex/ArrayLoop.h"
#include "leap_iterator.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

#include <algorithm>
#include <iterator>
#include <regex>
#include <string>
#include <vector>

// define to enable debug logging statements inside loops in getLevelDataSlice (slow)
// #define ENABLE_LOG_DEBUG_IN_LOOPS 1

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.CDMVerticalInterpolator");

bool isSimpleZAxis(const CDM& cdm, CoordinateSystem_cp cs)
{
    LOG4FIMEX(logger, Logger::DEBUG, "simple z? " << cs->id());
    CoordinateAxis_cp zAxis = cs->getGeoZAxis();
    if (!zAxis || !cs->getGeoXAxis() || !cs->getGeoYAxis()) {
        LOG4FIMEX(logger, Logger::DEBUG, "no z/x/y axis: " << cs->id());
        return false;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "z type: " << zAxis->getAxisTypeStr());
    if (!zAxis->isExplicit()) {
        LOG4FIMEX(logger, Logger::DEBUG, "z not explicit: " << cs->id());
        return false;
    }
    const std::vector<std::string>& zShape = zAxis->getShape();
    if (zShape.size() != 1) {
        LOG4FIMEX(logger, Logger::DEBUG, "z shape: " << join(zShape.begin(), zShape.end()));
        return false;
    }
    const std::string& zShape0 = zShape.at(0);
    if (!cdm.hasDimension(zShape0) || cdm.getDimension(zShape0).getLength() <= 1) {
        LOG4FIMEX(logger, Logger::DEBUG, "z dim0 '" << zShape0 << "' missing or bad length "
                  << cdm.getDimension(zShape0).getLength());
        return false;
    }
    return true;
}

void replaceZaxis(CDM& cdm, CoordinateSystem_cp cs, const std::string& varName, const std::string& zAxisName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "replace z? " << cs->id() << " var='" << varName << "' zaxis='" << zAxisName);
    // change the shape of the variables with this cs: remove the old z axis, add the new one
    if (!cs->isCSAndCompleteFor(varName)) {
        LOG4FIMEX(logger, Logger::DEBUG, "cs not complete for var='" << varName << "'");
        return;
    }

    const std::string& zAxisShape0 = cs->getGeoZAxis()->getShape().at(0);

    CDMVariable& var = cdm.getVariable(varName);
    std::vector<std::string> shape = var.getShape();
    std::vector<std::string>::iterator axisPos = find(shape.begin(), shape.end(), zAxisShape0);
    if (axisPos == shape.end())
        throw CDMException("axis in complete coordinate system for variable '" + varName + "' but not in shape");

    // change to new axis
    *axisPos = zAxisName;
    var.setShape(shape);
    LOG4FIMEX(logger, Logger::DEBUG, "new shape: " << join(shape.begin(), shape.end(), ","));

    // some people set explicit axis to implicit axis (coordinates in CF) - remove those!
    CDMAttribute coord;
    if (cdm.getAttribute(varName, "coordinates", coord)) {
        std::string coords = coord.getData()->asString();
        coords = std::regex_replace(coords, std::regex("\\b" + regex_escape(cs->getGeoZAxis()->getName()) + "\\b"), "");
        cdm.removeAttribute(varName, "coordinates");
        cdm.addAttribute(varName, CDMAttribute("coordinates", coords));
    }
}

} // namespace

using namespace std;

struct CDMVerticalInterpolator::Impl
{
    int verticalType;
    mifi_vertical_interpol_method verticalInterpolationMethod;
    vector<double> level1;
    // name of the generated/final vertical axis
    string vAxis;
    // variable-names with vertical information to change
    CoordinateSystem_cp_v changeCoordSys;

    bool interpolateToAxis;

    CoordinateSystem_cp templateCS;

    bool ignoreValidityMin;
    bool ignoreValidityMax;

    Impl();
};

CDMVerticalInterpolator::Impl::Impl()
    : interpolateToAxis(false)
    , ignoreValidityMin(false)
    , ignoreValidityMax(false)
{
}

CDMVerticalInterpolator::CDMVerticalInterpolator(CDMReader_p dataReader, const string& verticalType, const string& verticalInterpolationMethod)
    : dataReader_(dataReader)
    , pimpl_(new Impl())
{
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
        LOG4FIMEX(logger, Logger::ERROR, "unknown verticalType: " << verticalType);
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
}

CDMVerticalInterpolator::~CDMVerticalInterpolator()
{
}

void CDMVerticalInterpolator::ignoreValidityMin(bool ignore)
{
    pimpl_->ignoreValidityMin = ignore;
}

void CDMVerticalInterpolator::ignoreValidityMax(bool ignore)
{
    pimpl_->ignoreValidityMax = ignore;
}

void CDMVerticalInterpolator::interpolateToFixed(const std::vector<double>& level1)
{
    pimpl_->level1 = level1;

    // get all coordinate systems from file before changing this cdm
    const CoordinateSystem_cp_v coordSys = listCoordinateSystems(dataReader_);

    // allocate a new vertical axis
    const CDM& rcdm = dataReader_->getCDM();
    switch (pimpl_->verticalType) {
    case MIFI_VINT_PRESSURE:
        pimpl_->vAxis = findUniqueDimVarName(rcdm, "pressure");
        break;
    case MIFI_VINT_HEIGHT:
        pimpl_->vAxis = findUniqueDimVarName(rcdm, "height");
        break;
    case MIFI_VINT_ALTITUDE:
        pimpl_->vAxis = findUniqueDimVarName(rcdm, "height_above_msl");
        break;
    case MIFI_VINT_DEPTH:
        pimpl_->vAxis = findUniqueDimVarName(rcdm, "depth");
        break;
    }

    cdm_->addDimension(CDMDimension(pimpl_->vAxis, level1.size()));
    CDMVariable var(pimpl_->vAxis, CDM_DOUBLE, vector<string>(1, pimpl_->vAxis));
    shared_array<double> level1d(new double[level1.size()]);
    copy(level1.begin(), level1.end(), &level1d[0]);
    var.setData(createData(level1.size(), level1d));
    cdm_->addVariable(var);

    switch (pimpl_->verticalType) {
    case MIFI_VINT_PRESSURE:
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "hPa"));
        break;
    case MIFI_VINT_HEIGHT:
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "m"));
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("positive", "up"));
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("long_name", "height above ground"));
        break;
    case MIFI_VINT_ALTITUDE:
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "m"));
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("positive", "up"));
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("standard_name", "height_above_msl"));
        break;
    case MIFI_VINT_DEPTH:
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("units", "m"));
        cdm_->addAttribute(pimpl_->vAxis, CDMAttribute("positive", "down"));
        break;
    }

    for (const auto cs : coordSys) {
        if (isSimpleZAxis(*cdm_, cs)) {
            pimpl_->changeCoordSys.push_back(cs);

            // change the shape of the variables with this cs: remove the old z axis, add the new one
            for (const auto& var : dataReader_->getCDM().getVariables()) {
                replaceZaxis(*cdm_, cs, var.getName(), pimpl_->vAxis);
            }

            // remove the old zAxis
            cdm_->removeVariable(cs->getGeoZAxis()->getName());
        }
    }
}

void CDMVerticalInterpolator::interpolateToAxis(const std::string& vAxis)
{
    if (vAxis.empty())
        throw CDMException("vertical interpolation to '" + vAxis + "' not possible");

    pimpl_->interpolateToAxis = true;
    pimpl_->vAxis = vAxis;

    // get all coordinate systems from file before changing this cdm
    const CoordinateSystem_cp_v coordSys = listCoordinateSystems(dataReader_);

    std::set<std::string> removeZ;

    for (const auto cs : coordSys) {
        CoordinateAxis_cp zAxis = cs->getGeoZAxis();
        if (!zAxis || zAxis->getName() == vAxis || !isSimpleZAxis(*cdm_, cs))
            continue;

        pimpl_->changeCoordSys.push_back(cs);
        removeZ.insert(zAxis->getName());

        // change the shape of the variable: remove the old axis, add the new one
        for (const auto& var : dataReader_->getCDM().getVariables()) {
            replaceZaxis(*cdm_, cs, var.getName(), vAxis);
        }
    }

    // remove the old zAxis
    for (const auto& zName : removeZ)
        cdm_->removeVariable(zName);
}

void CDMVerticalInterpolator::interpolateByTemplateVariable(const std::string& tv)
{
    // get all coordinate systems from file before changing this cdm
    const CoordinateSystem_cp_v coordSys = listCoordinateSystems(dataReader_);

    pimpl_->templateCS = findCompleteCoordinateSystemFor(coordSys, tv);

    if (!pimpl_->templateCS)
        throw CDMException("vertical interpolation template variable '" + tv + "' without coordinate system");
    if (!isSimpleZAxis(*cdm_, pimpl_->templateCS))
        throw CDMException("vertical interpolation template variable '" + tv + "' has unsupported z axis");
    if (!pimpl_->templateCS->getVerticalTransformation())
        throw CDMException("vertical interpolation template variable '" + tv + "' without vertical transformation");

    CoordinateAxis_cp zAxisTemplate = pimpl_->templateCS->getGeoZAxis();
    const std::string& zAxisTemplateShape0 = zAxisTemplate->getShape().at(0);

    for (size_t i = 0; i < coordSys.size(); i++) {
        CoordinateSystem_cp cs = coordSys[i];
        CoordinateAxis_cp zAxis = cs->getGeoZAxis();
        if (zAxis == zAxisTemplate)
            continue;
        if (!isSimpleZAxis(*cdm_, cs))
            continue;

        pimpl_->changeCoordSys.push_back(cs);

        // change the shape of the variable: remove the old axis, add the new one
        for (const auto& var : dataReader_->getCDM().getVariables()) {
            replaceZaxis(*cdm_, cs, var.getName(), zAxisTemplateShape0);
        }
    }
}

DataPtr CDMVerticalInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    if (CoordinateSystem_cp csI = findCompleteCoordinateSystemFor(pimpl_->changeCoordSys, varName)) {
        return getLevelDataSlice(csI, varName, unLimDimPos);
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "no cs change for var='" << varName << "'");
        // no level to change, propagate to the dataReader_
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }
}

DataPtr CDMVerticalInterpolator::getLevelDataSlice(CoordinateSystem_cp csI, const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getLevelDataSlice(.. '" << varName << "' ..)");
    if (!csI->hasVerticalTransformation()) {
        throw CDMException(varName + " has no vertical transformation");
    }

    VerticalConverter_p iConverter = verticalConverter(csI, dataReader_, pimpl_->verticalType);
    DataPtr iVerticalData = verticalData4D(iConverter, dataReader_->getCDM(), unLimDimPos);

    VerticalConverter_p oConverter;
    DataPtr oVerticalData;
    if (pimpl_->interpolateToAxis) {
        CDMReader_p self(this, [](CDMReader*) { }); // FIXME hack to avoid adding enable_shared_from_this
        const CoordinateSystem_cp_v coordSys = listCoordinateSystems(self);
        const auto cs = findCompleteCoordinateSystemFor(coordSys, varName);
        oConverter = verticalConverter(cs, self, pimpl_->verticalType);
        oVerticalData = verticalData4D(oConverter, *cdm_, unLimDimPos);
    } else if (pimpl_->templateCS) {
        oConverter = verticalConverter(pimpl_->templateCS, dataReader_, pimpl_->verticalType);
        oVerticalData = verticalData4D(oConverter, dataReader_->getCDM(), unLimDimPos);
    }

    int (*intFunc)(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x) = 0;
    switch (pimpl_->verticalInterpolationMethod) {
    case MIFI_VINT_METHOD_LIN: intFunc = &mifi_get_values_linear_f; break;
    case MIFI_VINT_METHOD_LIN_WEAK_EXTRA: intFunc = &mifi_get_values_linear_weak_extrapol_f; break;
    case MIFI_VINT_METHOD_LIN_NO_EXTRA: intFunc = &mifi_get_values_linear_no_extrapol_f; break;
    case MIFI_VINT_METHOD_LIN_CONST_EXTRA: intFunc = &mifi_get_values_linear_const_extrapol_f; break;
    case MIFI_VINT_METHOD_LOG: intFunc = &mifi_get_values_log_f; break;
    case MIFI_VINT_METHOD_LOGLOG: intFunc = &mifi_get_values_log_log_f; break;
    case MIFI_VINT_METHOD_NN: intFunc = &mifi_get_values_nearest_f; break;
    }

    const std::string& geoZi = csI->getGeoZAxis()->getName();
    const std::string& geoZo = pimpl_->templateCS ? pimpl_->templateCS->getGeoZAxis()->getName() : pimpl_->vAxis;

    ArrayDims siData = makeArrayDims(dataReader_->getCDM(), varName);
    ArrayDims siVertical = makeArrayDims(dataReader_->getCDM(), iConverter);
    ArrayDims soData = makeArrayDims(getCDM(), varName);
    ArrayDims soVertical;
    ArrayDims sDataMax, sDataMin;

    if (oConverter) {
        soVertical = makeArrayDims(dataReader_->getCDM(), oConverter);
    } else {
        soVertical.add(pimpl_->vAxis /* or dim name? */, pimpl_->level1.size());
    }
    set_not_shared(geoZi, siData, siVertical);
    set_not_shared(geoZo, soData, soVertical);
    forceUnLimDimLength1(getCDM(), siData, siVertical, soData, soVertical);

    enum { IN, IN_VERTICAL, OUT, OUT_VERTICAL };
    ArrayGroup group = ArrayGroup().add(siData).add(siVertical).add(soData).add(soVertical);
    group.minimizeShared(0); // we have to treat each value separately

    shared_array<double> valueMin, valueMax;
    size_t VALID_MIN = 0, VALID_MAX = 0;
    if (oConverter) {
        if (!pimpl_->ignoreValidityMax) {
            const std::vector<std::string> oValidMaxShape = oConverter->getValidityMaxShape();
            LOG4FIMEX(logger, Logger::DEBUG, "o valid max shape: " << join(oValidMaxShape.begin(), oValidMaxShape.end()));
            const SliceBuilder sbValidMax = createSliceBuilder(dataReader_->getCDM(), oValidMaxShape);
            if (DataPtr oValuesMax = oConverter->getValidityMax(sbValidMax)) {
                sDataMax = makeArrayDims(getCDM(), oValidMaxShape);
                forceUnLimDimLength1(getCDM(), sDataMax);
                VALID_MAX = group.arrayCount();
                group.add(sDataMax);
                valueMax = oValuesMax->asDouble();
            }
        }
        if (!pimpl_->ignoreValidityMin) {
            const std::vector<std::string> oValidMinShape = oConverter->getValidityMinShape();
            LOG4FIMEX(logger, Logger::DEBUG, "o valid min shape: " << join(oValidMinShape.begin(), oValidMinShape.end()));
            const SliceBuilder sbValidMin = createSliceBuilder(dataReader_->getCDM(), oValidMinShape);
            if (DataPtr oValuesMin = oConverter->getValidityMin(sbValidMin)) {
                sDataMin = makeArrayDims(getCDM(), oValidMinShape);
                forceUnLimDimLength1(getCDM(), sDataMin);
                VALID_MIN = group.arrayCount();
                group.add(sDataMin);
                valueMin = oValuesMin->asDouble();
            }
        }
    }
    if (VALID_MIN == 0 && VALID_MAX == 0) {
        if (!pimpl_->ignoreValidityMax) {
            const std::vector<std::string> iValidMaxShape = iConverter->getValidityMaxShape();
            LOG4FIMEX(logger, Logger::DEBUG, "i valid max shape: " << join(iValidMaxShape.begin(), iValidMaxShape.end()));
            const SliceBuilder sbValidMax = createSliceBuilder(dataReader_->getCDM(), iValidMaxShape);
            if (DataPtr iValuesMax = iConverter->getValidityMax(sbValidMax)) {
                sDataMax = makeArrayDims(getCDM(), iValidMaxShape);
                forceUnLimDimLength1(getCDM(), sDataMax);
                VALID_MAX = group.arrayCount();
                LOG4FIMEX(logger, Logger::DEBUG, "VALID_MAX=" << VALID_MAX);
                group.add(sDataMax);
                valueMax = iValuesMax->asDouble();
            }
        }
        if (!pimpl_->ignoreValidityMin) {
            const std::vector<std::string> iValidMinShape = iConverter->getValidityMinShape();
            LOG4FIMEX(logger, Logger::DEBUG, "i valid min shape: " << join(iValidMinShape.begin(), iValidMinShape.end()));
            const SliceBuilder sbValidMin = createSliceBuilder(dataReader_->getCDM(), iValidMinShape);
            if (DataPtr iValuesMin = iConverter->getValidityMin(sbValidMin)) {
                sDataMin = makeArrayDims(getCDM(), iValidMinShape);
                forceUnLimDimLength1(getCDM(), sDataMin);
                VALID_MIN = group.arrayCount();
                LOG4FIMEX(logger, Logger::DEBUG, "VALID_MIN=" << VALID_MIN);
                group.add(sDataMin);
                valueMin = iValuesMin->asDouble();
            }
        }
    }

    const size_t nzi = siData.length(geoZi);
    const size_t nzo = soData.length(geoZo);
    LOG4FIMEX(logger, Logger::DEBUG, "nzi=" << nzi << " nzo=" << nzo);

    const size_t idataZdelta = siData.delta(geoZi);
    const size_t iverticalZdelta = siVertical.delta(geoZi);
    const size_t odataZdelta = soData.delta(geoZo);
    const size_t overticalZdelta = soVertical.delta(geoZo);

    DataPtr data = dataReader_->getDataSlice(varName, unLimDimPos);
    const double badValue = cdm_->getFillValue(varName);
    shared_array<float> iData = data2InterpolationArray(data, badValue);
    const size_t oSize = soData.volume();
    shared_array<float> oData(new float[oSize]);
    shared_array<float> iVerticalValues = iVerticalData->asFloat();
    shared_array<float> oVerticalValues;
    if (oVerticalData)
        oVerticalValues = oVerticalData->asFloat();

#ifdef _OPENMP
#pragma omp parallel for default(shared)
#endif
    for (size_t k = 0; k < nzo; k++) {
#ifdef ENABLE_LOG_DEBUG_IN_LOOPS
        LOG4FIMEX(logger, Logger::DEBUG, "k=" << k);
        size_t loopi = 0;
#endif
        Loop loop(group);
        do { // sharedVolume() == 1 because we called minimizeShared before
#ifdef ENABLE_LOG_DEBUG_IN_LOOPS
            LOG4FIMEX(logger, Logger::DEBUG, "loopi=" << loopi++);
#endif
            const size_t verticalOutIdx = loop[OUT_VERTICAL] + k * overticalZdelta;
            const double verticalOut = oVerticalValues ? oVerticalValues[verticalOutIdx] : pimpl_->level1[verticalOutIdx];
            float* interpolated = &oData[loop[OUT] + k*odataZdelta];

            bool range = true;
            if (valueMin && valueMax) {
                range = (verticalOut >= valueMin[loop[VALID_MIN]]) && (verticalOut <= valueMax[loop[VALID_MAX]]);
#ifdef ENABLE_LOG_DEBUG_IN_LOOPS
                LOG4FIMEX(logger, Logger::DEBUG, "verticalOut= " << verticalOut
                          << " min[" << loop[VALID_MIN] << "]=" << valueMin[loop[VALID_MIN]]
                          << " max[" << loop[VALID_MAX] << "]=" << valueMax[loop[VALID_MAX]]
                          << " range=" << range);
#endif
            } else if (valueMin) {
                range = (verticalOut >= valueMin[loop[VALID_MIN]]);
#ifdef ENABLE_LOG_DEBUG_IN_LOOPS
                LOG4FIMEX(logger, Logger::DEBUG, "verticalOut= " << verticalOut
                          << " min[" << loop[VALID_MIN] << "]=" << valueMin[loop[VALID_MIN]]
                          << " range=" << range);
#endif
            } else if (valueMax) {
                range = (verticalOut <= valueMax[loop[VALID_MAX]]);
#ifdef ENABLE_LOG_DEBUG_IN_LOOPS
                LOG4FIMEX(logger, Logger::DEBUG, "verticalOut= " << verticalOut
                          << " max[" << loop[VALID_MAX] << "]=" << valueMax[loop[VALID_MAX]]
                          << " range=" << range);
#endif
            }

            if (range) {
                const leap_iterator<const float*> ivertical0(&iVerticalValues[loop[IN_VERTICAL]], iverticalZdelta);
                const leap_iterator<const float*> ivertical1 = ivertical0 + nzi;
                const pair<size_t, size_t> pos = find_closest_neighbor_distinct_elements(ivertical0, ivertical1, verticalOut);
                if (pos.first != pos.second) {
                    const size_t idataZ0  = loop[IN] + idataZdelta * pos.first;
                    const size_t idataZ1  = loop[IN] + idataZdelta * pos.second;
                    const float valueI0 = iData[idataZ0], valueI1 = iData[idataZ1];
                    const float verticalI0 = *(ivertical0 + pos.first), verticalI1 = *(ivertical0 + pos.second);
                    intFunc(&valueI0, &valueI1, interpolated, 1, verticalI0, verticalI1, verticalOut);
                } else {
                    // find_closest_neighbor_distinct_elements failed
                    *interpolated = MIFI_UNDEFINED_F;
                }
            } else {
                // not a valid z
                *interpolated = MIFI_UNDEFINED_F;
            }
        } while (loop.next());
    }

    // correct data going out of bounds
    const double valid_min = cdm_->getValidMin(varName);
    const double valid_max = cdm_->getValidMax(varName);
    if (!std::isnan(valid_min)) {
        float minVal = static_cast<float>(valid_min);
        replace_if(&oData[0], &oData[0]+oSize, bind2nd(less<float>(), minVal), minVal);
    }
    if (!std::isnan(valid_max)) {
        float maxVal = static_cast<float>(valid_max);
        replace_if(&oData[0], &oData[0]+oSize, bind2nd(greater<float>(), maxVal), maxVal);
    }

    CDMDataType oType = getCDM().getVariable(varName).getDataType();
    return interpolationArray2Data(oType, oData, soData.volume(), badValue);
}

} // namespace MetNoFimex
