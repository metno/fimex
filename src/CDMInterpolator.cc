/*
 * Fimex
 *
 * (C) Copyright 2008-2019, met.no
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
 */

#ifdef _OPENMP
#include <omp.h>
#endif

// fimex
//
#include "CachedForwardInterpolation.h"
#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CachedInterpolation.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/SpatialAxisSpec.h"
#include "fimex/StringUtils.h"
#include "fimex/Type2String.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/interpolation.h"
#include "fimex/min_max.h"

#include "nanoflann/nanoflann.hpp"

// standard
#include <algorithm>
#include <cassert>
#include <ctime>
#include <fstream>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <regex>
#include <set>
#include <string>

namespace MetNoFimex {

using namespace std;

typedef std::shared_ptr<CachedInterpolationInterface> CachedInterpolationInterface_p;
typedef std::shared_ptr<CachedVectorReprojection> CachedVectorReprojection_p;

InterpolatorProcess2d::~InterpolatorProcess2d() {}

void InterpolatorFill2d::operator()(float* array, size_t nx, size_t ny)
{
    size_t nChanged;
    mifi_fill2d_f(nx, ny, array, relaxCrit_, corrEff_, maxLoop_, &nChanged);
}

void InterpolatorCreepFill2d::operator()(float* array, size_t nx, size_t ny)
{
    size_t nChanged;
    mifi_creepfill2d_f(nx, ny, array, repeat_, setWeight_, &nChanged);
}

void InterpolatorCreepFillVal2d::operator()(float* array, size_t nx, size_t ny)
{
    size_t nChanged;
    mifi_creepfillval2d_f(nx, ny, array, defVal_, repeat_, setWeight_, &nChanged);
}

struct CDMInterpolator::Impl
{
    CDMReader_p dataReader;
    double maxDistance; // negative = undefined
    std::string latitudeName;
    std::string longitudeName;
    std::vector<InterpolatorProcess2d_p> preprocesses;
    std::vector<InterpolatorProcess2d_p> postprocesses;
    // variableName, horizontalId
    typedef std::map<std::string, std::string> projectionVariables_t;
    projectionVariables_t projectionVariables;
    // horizontalId, cachedInterpolation
    typedef map<string, CachedInterpolationInterface_p> cachedInterpolation_t;
    cachedInterpolation_t cachedInterpolation;
    // horizontalId, cachedVectorReprojection
    typedef map<string, CachedVectorReprojection_p> cachedVectorReprojection_t;
    cachedVectorReprojection_t cachedVectorReprojection;
};

namespace {
const std::string LAT_LON_PROJSTR = MIFI_WGS84_LATLON_PROJ4;
Logger_p logger = getLogger("fimex.CDMInterpolator");
} // namespace

CDMInterpolator::CDMInterpolator(CDMReader_p dataReader)
    : p_(new Impl())
{
    p_->dataReader = dataReader;
    p_->maxDistance = -1;
    p_->latitudeName = "lat";
    p_->longitudeName = "lon";
    enhanceVectorProperties(p_->dataReader); // set spatial-vectors
    listCoordinateSystems(p_->dataReader); // add eventually needed information to cdm (e.g. Time-axis in WRF)
    *cdm_ = p_->dataReader->getCDM();
}

CDMInterpolator::~CDMInterpolator()
{
}

shared_array<float> data2InterpolationArray(const DataPtr& inData, double badValue)
{
    shared_array<float> array = inData->asFloat();
    mifi_bad2nanf(&array[0], &array[inData->size()], badValue);
    return array;
}

DataPtr interpolationArray2Data(CDMDataType newType, shared_array<float> iData, size_t size, double badValue)
{
    DataPtr d = createData(size, iData);
    return d->convertDataType(MIFI_UNDEFINED_F, 1., 0., newType, badValue, 1., 0.);
}

namespace {
/**
 * run all processes 2d-slice by 2d-slice on the array of size
 *
 * @param processes list of processes
 * @param array the data
 * @param size size of the data (must be N * nx * ny)
 * @param nx size in x-direction
 * @param ny size in y-direction
 */
void processArray_(vector<InterpolatorProcess2d_p> processes, float* array, size_t size, size_t nx, size_t ny)
{
    if (processes.size() == 0) return; // nothing to do

    int nz = static_cast<int>(size / (nx*ny));
    assert((nz*nx*ny) == size);

#ifdef _OPENMP
#pragma omp parallel default(shared) if (nz >= 4)
    {
#pragma omp for nowait
#endif
    for (int z = 0; z < nz; z++) { // using int instead of size_t because of openMP < 3.0
        // find the start of the slice
        float* arrayPos = array + (z*nx*ny);
        for (size_t i = 0; i < processes.size(); i++) {
            processes[i]->operator()(arrayPos, nx, ny);
        }
    }
#ifdef _OPENMP
    }
#endif
    return;
}
} // namespace

DataPtr CDMInterpolator::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "interpolating '"<< varName << "' with sliceBuilder" );
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData())
        return getDataSliceFromMemory(variable, sb);

    Impl::projectionVariables_t::const_iterator itP = p_->projectionVariables.find(varName);
    if (itP == p_->projectionVariables.end()) {
        // no projection, just forward
        return p_->dataReader->getDataSlice(varName, sb);
    }

    const string& horizontalId = itP->second;
    Impl::cachedInterpolation_t::iterator itCI = p_->cachedInterpolation.find(horizontalId);
    if (itCI == p_->cachedInterpolation.end())
        throw CDMException("no cached interpolation for " + varName + "(" + horizontalId + ")");

    CachedInterpolationInterface_p ci = itCI->second;
    DataPtr data = ci->getInputDataSlice(p_->dataReader, varName, sb);
    if (data->size() == 0)
        return data;

    const double badValue = cdm_->getFillValue(varName);
    shared_array<float> array = data2InterpolationArray(data, badValue);
    processArray_(p_->preprocesses, array.get(), data->size(), ci->getInX(), ci->getInY());

    size_t newSize = 0;
    LOG4FIMEX(logger, Logger::DEBUG, "interpolateValues for: " << varName << "(slicebuilder)");
    shared_array<float> iArray = ci->interpolateValues(array, data->size(), newSize);

    if (variable.isSpatialVector()) {
        // vector in x/y direction
        const CDMVariable::SpatialVectorDirection dir = variable.getSpatialVectorDirection();
        if (dir == CDMVariable::SPATIAL_VECTOR_X || dir == CDMVariable::SPATIAL_VECTOR_Y) {
            bool can_reproject = false;
            const std::string& counterpart = variable.getSpatialVectorCounterpart();
            Impl::projectionVariables_t::const_iterator itC = p_->projectionVariables.find(counterpart);
            if (itC != p_->projectionVariables.end() && horizontalId == itC->second) {
                Impl::cachedVectorReprojection_t::iterator itV = p_->cachedVectorReprojection.find(horizontalId);
                if (itV != p_->cachedVectorReprojection.end()) {
                    CachedVectorReprojection_p cvr = itV->second;
                    // fetch and transpose vector-data
                    // transposing needed once for each direction (or caching, but that needs to much memory)
                    shared_array<float> counterPartArray =
                        data2InterpolationArray(ci->getInputDataSlice(p_->dataReader, counterpart, sb), cdm_->getFillValue(counterpart));
                    processArray_(p_->preprocesses, counterPartArray.get(), data->size(), ci->getInX(), ci->getInY());
                    LOG4FIMEX(logger, Logger::DEBUG, "implicit interpolateValues for: " << counterpart << "(slicebuilder)");
                    shared_array<float> counterpartiArray = ci->interpolateValues(counterPartArray, data->size(), newSize);
                    if (dir == CDMVariable::SPATIAL_VECTOR_X)
                        cvr->reprojectValues(iArray, counterpartiArray, newSize);
                    else
                        cvr->reprojectValues(counterpartiArray, iArray, newSize);
                    can_reproject = true;
                }
            }
            if (!can_reproject)
                LOG4FIMEX(logger, Logger::WARN, "Cannot reproject vector " << variable.getName());
        }
    }

    processArray_(p_->postprocesses, iArray.get(), newSize, ci->getOutX(), ci->getOutY());

    return ci->getOutputDataSlice(interpolationArray2Data(variable.getDataType(), iArray, newSize, badValue), sb);
}

DataPtr CDMInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData())
        return getDataSliceFromMemory(variable, unLimDimPos);

    SliceBuilder sb(*cdm_, varName);
    if (const CDMDimension* unlimDim = cdm_->getUnlimitedDim()) {
        if (cdm_->hasUnlimitedDim(variable))
            sb.setStartAndSize(unlimDim->getName(), unLimDimPos, 1);
    }
    return getDataSlice(varName, sb);
}

void CDMInterpolator::setLatitudeName(const std::string& latName) {
    p_->latitudeName = latName;
}
const std::string& CDMInterpolator::getLatitudeName() const {
    return p_->latitudeName;
}
void CDMInterpolator::setLongitudeName(const std::string& lonName) {
    p_->longitudeName = lonName;
}
const std::string& CDMInterpolator::getLongitudeName() const {
    return p_->longitudeName;
}
void CDMInterpolator::setDistanceOfInterest(double dist) {
    p_->maxDistance = dist;
}
double CDMInterpolator::getMaxDistanceOfInterest(const vector<double>& out_x_axis, const vector<double>& out_y_axis, bool isMetric) const
{
    if (p_->maxDistance > 0) return p_->maxDistance;
    // find the max distance of two neighboring points in the output
    // this is the region of influence for a cell
    double factor = 1.;
    if (!isMetric) {
        factor = MIFI_EARTH_RADIUS_M;
    }
    double maxDist = 0;
    {
        double maxX = 0;
        for (size_t i = 0; i < out_x_axis.size() - 1; ++i) {
            maxX = max(factor*abs(out_x_axis[i+1]-out_x_axis[i]), maxX);
        }
        double maxY = 0;
        for (size_t j = 0; j < out_y_axis.size()-1; ++j) {
            maxY = max(factor*abs(out_y_axis[j+1] - out_y_axis[j]), maxY);
        }
        maxDist = max(maxX, maxY);
    }
    return maxDist;
}

namespace {
string getProjectionName(const string& proj_input)
{
    // get the new projection
    std::string newProj;
    std::smatch what;
    if (std::regex_search(proj_input, what, std::regex("\\+proj=(\\S+)"))) {
        newProj = what[1].str();
    } else {
        throw CDMException("cannot find +proj=... in proj-string: " + proj_input);
    }
    // unify name for geographic coordinates
    if (newProj == "latlon" || newProj == "latlong" || newProj == "lonlat" || newProj == "longlat") return "latlong";
    return newProj;
}
} // namespace

void CDMInterpolator::changeProjection(int method, const string& proj_input, const string& out_x_axis, const string& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit, const string& out_x_axis_type, const string& out_y_axis_type)
{
    SpatialAxisSpec xAxisSpec(out_x_axis);
    SpatialAxisSpec yAxisSpec(out_y_axis);
    if (xAxisSpec.requireStartEnd() || yAxisSpec.requireStartEnd()) {
        // detect the bounding box in the final projection
        map<string, CoordinateSystem_cp> coordSysMap = findBestCoordinateSystemsAndProjectionVars(false);
        if (p_->projectionVariables.size() < 1) throw CDMException("could not find variables with coordinate system");
        // just using the bounding box of the first coordinate-system found
        CoordinateSystem_cp coordSys = coordSysMap.begin()->second;
        string longitude = coordSys->findAxisOfType(CoordinateAxis::Lon)->getName();
        string latitude = coordSys->findAxisOfType(CoordinateAxis::Lat)->getName();
        if (latitude == "" || longitude == "") throw CDMException("could not find lat/long variables");
        const vector<string> dims = cdm_->getVariable(latitude).getShape();
        DataPtr lonData = p_->dataReader->getScaledData(longitude);
        DataPtr latData = p_->dataReader->getScaledData(latitude);
        shared_array<double> latVals = latData->asDouble();
        shared_array<double> lonVals = lonData->asDouble();
        size_t latSize = latData->size();
        size_t lonSize = lonData->size();
        if (latSize != lonSize) {
            // latData/lonData are not 2d-coordinate-variable, but 1d axes
            // making latVals/lonVals 2d with size latSize*lonSize
            shared_array<double> lxVals(new double[latSize * lonSize]);
            shared_array<double> lyVals(new double[latSize * lonSize]);
            for (size_t i=0; i < lonSize; i++) {
                for (size_t j=0; j<latSize; j++) {
                    lxVals[i+lonSize*j] = lonVals[i];
                    lyVals[i+lonSize*j] = latVals[j];
                }
            }
            lonVals = lxVals;
            latVals = lyVals;
            latSize *= lonSize;
        }
        transform_deg_to_rad(latVals.get(), latSize);
        transform_deg_to_rad(lonVals.get(), latSize);
        if (getProjectionName(proj_input) != "latlong") {
            std::string orgProjStr = LAT_LON_PROJSTR;
            if (MIFI_OK != mifi_project_values(orgProjStr.c_str(), proj_input.c_str(), &lonVals[0], &latVals[0], latSize)) {
                throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input);
            }
            // lonVals contains now all x-values, latVals all y-values
            // get bounding box:
            double xMin = *(min_element(&lonVals[0], &lonVals[latSize]));
            double yMin = *(min_element(&latVals[0], &latVals[latSize]));
            double xMax = *(max_element(&lonVals[0], &lonVals[latSize]));
            double yMax = *(max_element(&latVals[0], &latVals[latSize]));
            if (getProjectionName(proj_input) == "ob_tran") {
                xMin = rad_to_deg(xMin);
                yMin = rad_to_deg(yMin);
                xMax = rad_to_deg(xMax);
                yMax = rad_to_deg(yMax);
            }
            xAxisSpec.setStartEnd(xMin, xMax);
            yAxisSpec.setStartEnd(yMin, yMax);
            LOG4FIMEX(logger, Logger::INFO, "changeProjection, boundingbox: (" <<  xMin << "," << yMin << "), (" << xMax << "," << yMax << ")");
        } else {
            throw CDMException("changeProjection with autotuning axes only implemented for projections in m, not degree yet");
        }
    }

    CDMDataType xType = string2datatype(out_x_axis_type);
    CDMDataType yType = string2datatype(out_y_axis_type);
    changeProjection(method, proj_input, xAxisSpec.getAxisSteps(), yAxisSpec.getAxisSteps(), out_x_axis_unit, out_y_axis_unit, xType, yType);
}

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit)
{
    changeProjection(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, CDM_DOUBLE, CDM_DOUBLE);
}

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit, CDMDataType out_x_axis_type, CDMDataType out_y_axis_type)
{
    LOG4FIMEX(logger, Logger::DEBUG, "changing projection to new axes");
    if (out_x_axis_type == CDM_NAT ||
            out_y_axis_type == CDM_NAT) {
        throw CDMException("axis type of interpolation not well defined");
    }
    *cdm_ = p_->dataReader->getCDM(); // reset previous changes
    p_->projectionVariables.clear(); // reset variables
    switch (method) {
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
    case MIFI_INTERPOL_BILINEAR:
    case MIFI_INTERPOL_BICUBIC:
        changeProjectionByProjectionParameters(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, out_x_axis_type, out_y_axis_type); break;
    case MIFI_INTERPOL_COORD_NN:
    case MIFI_INTERPOL_COORD_NN_KD:
        changeProjectionByCoordinates(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, out_x_axis_type, out_y_axis_type); break;
    case MIFI_INTERPOL_FORWARD_SUM:
    case MIFI_INTERPOL_FORWARD_MEAN:
    case MIFI_INTERPOL_FORWARD_MEDIAN:
    case MIFI_INTERPOL_FORWARD_MAX:
    case MIFI_INTERPOL_FORWARD_MIN:
    case MIFI_INTERPOL_FORWARD_UNDEF_SUM:
    case MIFI_INTERPOL_FORWARD_UNDEF_MEAN:
    case MIFI_INTERPOL_FORWARD_UNDEF_MEDIAN:
    case MIFI_INTERPOL_FORWARD_UNDEF_MAX:
    case MIFI_INTERPOL_FORWARD_UNDEF_MIN:
        changeProjectionByForwardInterpolation(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit, out_x_axis_type, out_y_axis_type); break;
    default: throw CDMException("unknown projection method: " + type2string(method));
    }
}


namespace {
struct double_to_float_cast
{
  float operator()(const double& x) const { return static_cast<float>(x); }
};
} // namespace

void CDMInterpolator::changeProjection(int method,
        const vector<double>& lonVals, const vector<double>& latVals)
{
    LOG4FIMEX(logger, Logger::DEBUG,
            "changing projection to latitude/longitude values");
    if (lonVals.size() != latVals.size()) {
        LOG4FIMEX(logger, Logger::ERROR,
                "changeProjection, number of longitude and latitude values differs: " << lonVals.size() << " != " << latVals.size());
        return;
    }

    switch (method) {
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
    case MIFI_INTERPOL_BILINEAR:
    case MIFI_INTERPOL_BICUBIC: {
        shared_array<float> tmplLatVals(new float[latVals.size()]);
        shared_array<float> tmplLonVals(new float[lonVals.size()]);

        std::transform(latVals.begin(), latVals.end(), &tmplLatVals[0],
                double_to_float_cast());
        std::transform(lonVals.begin(), lonVals.end(), &tmplLonVals[0],
                double_to_float_cast());

        vector<double> yVals(1);
        yVals.at(0) = 0;
        vector<double> xVals(lonVals.size());
        // creating a squared array with all possibilities
        for (size_t i = 0; i < lonVals.size(); i++) {
            xVals.at(i) = i;
        }
        changeProjectionByProjectionParametersToLatLonTemplate(method,
                LAT_LON_PROJSTR, xVals, yVals, "1", "1", CDM_DOUBLE, CDM_DOUBLE,
                createData(latVals.size(), tmplLatVals),
                createData(lonVals.size(), tmplLonVals));
        break;
    }
    case MIFI_INTERPOL_COORD_NN:
    case MIFI_INTERPOL_COORD_NN_KD:
    case MIFI_INTERPOL_FORWARD_SUM:
    case MIFI_INTERPOL_FORWARD_MEAN:
    case MIFI_INTERPOL_FORWARD_MEDIAN:
    case MIFI_INTERPOL_FORWARD_MAX:
    case MIFI_INTERPOL_FORWARD_MIN:
        throw CDMException(
                "projection method: " + type2string(method)
                        + ", not supported");
        break;
    default:
        throw CDMException("unknown projection method: " + type2string(method));
    }
}

void CDMInterpolator::changeProjectionToCrossSections(int method, const std::vector<CrossSectionDefinition>& crossSections)
{
    LOG4FIMEX(logger, Logger::DEBUG, "changing projection to crossSections");

    // find the used original projection
    map<string, CoordinateSystem_cp> css = findBestCoordinateSystemsAndProjectionVars(true);
    assert(css.size() > 0);
    Projection_cp proj = css.begin()->second->getProjection();
    CoordinateAxis_cp xAxis = css.begin()->second->getGeoXAxis();
    assert(xAxis.get() != 0);
    CoordinateAxis_cp yAxis = css.begin()->second->getGeoYAxis();
    assert(yAxis.get() != 0);

    DataPtr xData, yData;
    if (proj->isDegree()) {
        xData = p_->dataReader->getScaledDataInUnit(xAxis->getName(), "degree");
        yData = p_->dataReader->getScaledDataInUnit(yAxis->getName(), "degree");
    } else {
        xData = p_->dataReader->getScaledDataInUnit(xAxis->getName(), "m");
        yData = p_->dataReader->getScaledDataInUnit(yAxis->getName(), "m");
    }
    if (xData->size() < 2 || yData->size() < 2) {
        throw CDMException("x- or y-axis sizes < 2 elements, not possible to interpolate");
    }
    shared_array<double> d = xData->asDouble();
    double dx = d[1] - d[0];
    d = yData->asDouble();
    double dy = d[1] - d[0];
    if (dx == 0 || dy == 0) {
        throw CDMException("cross-section calculation: dx or dy derived from first two elements == 0");
    }

    vector<double> lonVals, latVals;
    vector<size_t> startPositions;
    vector<string> csNames;
    size_t maxNameLen = 80;
    for (vector<CrossSectionDefinition>::const_iterator csIt = crossSections.begin(); csIt != crossSections.end(); ++csIt) {
        if (csIt->lonLatCoordinates.size() > 0) {
            csNames.push_back(csIt->name);
            maximize(maxNameLen, csIt->name.length() + 1);
            startPositions.push_back(lonVals.size());
            if (csIt->lonLatCoordinates.size() == 1) {
                lonVals.push_back(csIt->lonLatCoordinates.at(0).first);
                latVals.push_back(csIt->lonLatCoordinates.at(0).second);
            } else {
                vector<double> xLon(2);
                vector<double> yLat(2);
                for (size_t i = 1; i < csIt->lonLatCoordinates.size(); ++i) {
                    xLon.at(0) = csIt->lonLatCoordinates.at(i-1).first;
                    yLat.at(0) = csIt->lonLatCoordinates.at(i-1).second;
                    xLon.at(1) = csIt->lonLatCoordinates.at(i).first;
                    yLat.at(1) = csIt->lonLatCoordinates.at(i).second;
                    proj->convertFromLonLat(xLon, yLat);
                    double xLonD = xLon.at(1) - xLon.at(0);
                    double yLatD = yLat.at(1) - yLat.at(0);
                    // number of gridpoints to select between two coordinates
                    size_t num = static_cast<size_t>(floor(max(fabs(xLonD/dx), fabs(yLatD/dy))));
                    vector<double> xLonPart, yLatPart;
                    if (i == 1) {
                        // first point is part only for the very first
                        // sub-crossection (otherwise it would be
                        // repeated, as it is also the last point of
                        // the next sub-crossection)
                        xLonPart.push_back(xLon.at(0));
                        yLatPart.push_back(yLat.at(0));
                    }
                    for (size_t j = 1; j < num; ++j) {
                        xLonPart.push_back(xLon.at(0) + j*xLonD/num);
                        yLatPart.push_back(yLat.at(0) + j*yLatD/num);
                    }
                    // first and last point will always be part
                    xLonPart.push_back(xLon.at(1));
                    yLatPart.push_back(yLat.at(1));
                    proj->convertToLonLat(xLonPart, yLatPart);
                    assert(xLonPart.size() == yLatPart.size());
                    // add all points to the lat/lonVals vectors
                    copy(xLonPart.begin(), xLonPart.end(), back_inserter(lonVals));
                    copy(yLatPart.begin(), yLatPart.end(), back_inserter(latVals));
                }
            }
        }
    }
    // add the additional information to separate the different cross-sections
    size_t nvcross = csNames.size();
    cdm_->addDimension(CDMDimension("two", 2));
    cdm_->addDimension(CDMDimension("nvcross", nvcross));
    cdm_->addDimension(CDMDimension("nvcross_strlen", maxNameLen));
    // vcross-names
    vector<string> shape;
    shape.push_back("nvcross_strlen");
    shape.push_back("nvcross");
    CDMVariable vcross("vcross_name", CDM_STRING, shape);
    std::string vcrossNames(maxNameLen * nvcross, 0);
    for (size_t i = 0; i < nvcross; ++i) {
        const std::string& name = csNames[i];
        const size_t nameLen = std::min(maxNameLen - 1, name.size());
        vcrossNames.replace(i * maxNameLen, nameLen, name);
    }
    vcross.setData(createData(vcrossNames));
    cdm_->addVariable(vcross);
    cdm_->addAttribute(vcross.getName(), CDMAttribute("bounds", "vcross_bnds"));

    // vcross_bnds
    shape.clear();
    shape.push_back("two");
    shape.push_back("nvcross");
    CDMVariable vcrossBnds("vcross_bnds", CDM_INT, shape);
    assert(nvcross == startPositions.size());
    shared_array<int> vcrossBndsAry(new int[2 * nvcross]);
    for (size_t i = 0; i < (nvcross-1); ++i) {
        vcrossBndsAry[i*2] = startPositions.at(i);
        vcrossBndsAry[i*2+1] = startPositions.at(i+1)-1;
    }
    vcrossBndsAry[(nvcross-1)*2] = startPositions.at(nvcross-1);
    vcrossBndsAry[(nvcross-1)*2+1] = lonVals.size()-1;
    vcrossBnds.setData(createData(2*nvcross, vcrossBndsAry));
    cdm_->addVariable(vcrossBnds);
    cdm_->addAttribute(vcrossBnds.getName(), CDMAttribute("description", "start- and end-position (included) in lat- and lon-dimensions for each vert. cross-section"));

    // do the real work of reprojection
    changeProjection(method, lonVals, latVals);
}


void CDMInterpolator::changeProjection(int method, const std::string& netcdf_template_file)
{
    LOG4FIMEX(logger, Logger::DEBUG, "changing projection to template");
    CDMReader_p tmplReader = CDMFileReaderFactory::create("netcdf", netcdf_template_file);
    changeProjection(method, tmplReader, "referenceVariable");
}

void CDMInterpolator::changeProjection(int method, CDMReader_p tmplReader, const std::string& tmplRefVarName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "changing projection to template");

    *cdm_ = p_->dataReader->getCDM(); // reset previous changes
    p_->projectionVariables.clear();  // reset variables
    p_->cachedInterpolation.clear();
    p_->cachedVectorReprojection.clear();

    switch (method) {
        case MIFI_INTERPOL_NEAREST_NEIGHBOR:
        case MIFI_INTERPOL_BILINEAR:
        case MIFI_INTERPOL_BICUBIC:
        {
           // compile template CDM modell
           const CDM& tmplCdmRef = tmplReader->getCDM();
           if (! tmplCdmRef.hasVariable(tmplRefVarName))
               throw CDMException("template reader CDM does not contain reference Variable '" + tmplRefVarName + "'");

           // get lat / lon info
           std::string tmplLatName;
           std::string tmplLonName;

           if(!tmplCdmRef.getLatitudeLongitude(tmplRefVarName, tmplLatName, tmplLonName)) {
               MetNoFimex::CDMAttribute coordsAtt;
               if(tmplCdmRef.getAttribute(tmplRefVarName, "coordinates", coordsAtt)) {
                   std::vector<std::string> coords = split_any(coordsAtt.getStringValue(), " ");
                   if(coords.size() == 2){
                       tmplLonName = trim(coords[0], " ");
                       tmplLatName = trim(coords[1], " ");
                   }
               }
           }

           // force things
           if(tmplLonName.empty())
               tmplLonName = "longitude";
           if(tmplLatName.empty())
               tmplLatName = "latitude";

           DataPtr tmplLatVals = tmplReader->getScaledData(tmplLatName);
           DataPtr tmplLonVals = tmplReader->getScaledData(tmplLonName);
           // get X / Y info
           std::string tmplXName = tmplCdmRef.getHorizontalXAxis(tmplRefVarName);
           std::string tmplYName = tmplCdmRef.getHorizontalYAxis(tmplRefVarName);
           DataPtr tmplXData = tmplReader->getScaledData(tmplXName);
           DataPtr tmplYData = tmplReader->getScaledData(tmplYName);
           shared_array<double> tmplXArray = tmplXData->asDouble();
           shared_array<double> tmplYArray = tmplYData->asDouble();
           vector<double> tmplXAxisVec(tmplXArray.get(), tmplXArray.get()+tmplXData->size());
           vector<double> tmplYAxisVec(tmplYArray.get(), tmplYArray.get()+tmplYData->size());

           changeProjectionByProjectionParametersToLatLonTemplate(method, LAT_LON_PROJSTR, tmplXAxisVec, tmplYAxisVec, tmplCdmRef.getUnits(tmplXName),
                                                                  tmplCdmRef.getUnits(tmplYName), tmplCdmRef.getVariable(tmplXName).getDataType(),
                                                                  tmplCdmRef.getVariable(tmplYName).getDataType(), tmplLatVals, tmplLonVals);
           break;
        }
        case MIFI_INTERPOL_COORD_NN:
        case MIFI_INTERPOL_COORD_NN_KD:
        case MIFI_INTERPOL_FORWARD_SUM:
        case MIFI_INTERPOL_FORWARD_MEAN:
        case MIFI_INTERPOL_FORWARD_MEDIAN:
        case MIFI_INTERPOL_FORWARD_MAX:
        case MIFI_INTERPOL_FORWARD_MIN:
            throw CDMException("projection method: " + type2string(method) + ", not supported");
            break;
        default:
            throw CDMException("unknown projection method: " + type2string(method));
    }
}

map<string, CoordinateSystem_cp> CDMInterpolator::findBestCoordinateSystemsAndProjectionVars(bool withProjection)
{
    if (!withProjection) {
        // make sure lat/lon points exist for all projections
        generateProjectionCoordinates(p_->dataReader);
    }

    typedef map<string, CoordinateSystem_cp> CoordSysMap;
    CoordSysMap coordSysMap;
    p_->projectionVariables.clear();
    vector<string> incompatibleVariables;
    if (0 == findBestHorizontalCoordinateSystems(withProjection, p_->dataReader, coordSysMap, p_->projectionVariables, incompatibleVariables)) {
        LOG4FIMEX(logger, Logger::ERROR, "no coordinate-systems" << (withProjection ? " with projection found, maybe you should try coordinate interpolation" : " found"));
        throw CDMException("no coordinate-systems found");
    }
    for (vector<string>::iterator iv = incompatibleVariables.begin(); iv != incompatibleVariables.end(); ++iv) {
        LOG4FIMEX(logger, Logger::WARN, "removing variable " << *iv << " since it is not compatible with the reprojected coordinates");
        cdm_->removeVariable(*iv);
    }
    if (logger->isEnabledFor(Logger::DEBUG)) {
        vector<string> pVars;
        pVars.reserve(p_->projectionVariables.size());
        for (map<string, string>::iterator pvi = p_->projectionVariables.begin(); pvi != p_->projectionVariables.end(); ++pvi) {
            pVars.push_back(pvi->first);
        }
        LOG4FIMEX(logger, Logger::DEBUG, "projection variables: " << join(pVars.begin(), pVars.end(), ","));
    }

    return coordSysMap;
}

namespace {
/**
 * make changes in the CDM structure to reflect the new projection (attributes, coordinates, projection-variable, dimensions)
 *
 * @param cmd
 * @param proj_input
 * @param orgProjection
 * @param orgXAxis
 * @param orgYAxis
 */
void changeCDM(CDM& cdm, const string& proj_input, const map<string, CoordinateSystem_cp>& csMap, map<string, string>& projectionVariables,
               const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit,
               CDMDataType xAxisType, CDMDataType yAxisType, const string& longitudeName, const string& latitudeName)
{
    string newProj = getProjectionName(proj_input);
    string newXAxis;
    string newYAxis;
    for (map<string, CoordinateSystem_cp>::const_iterator csi = csMap.begin(); csi != csMap.end(); ++csi) {
        CoordinateSystem_cp cs = csi->second;
        if (cs->hasProjection()) {
            // remove all projections, those might confuse other programs, i.e. IDV
            std::vector<std::string> gridMappings = cdm.findVariables("grid_mapping_name", ".*");
            for (const string& gm : gridMappings) {
                LOG4FIMEX(logger, Logger::DEBUG, "removing projection-variable " << gm);
                cdm.removeVariable(gm);
                projectionVariables.erase(gm);
            }
        }

        string orgXAxis = cs->getGeoXAxis()->getName();
        string orgYAxis = cs->getGeoYAxis()->getName();
        if (cs->hasAxisType(CoordinateAxis::Lon) && (orgXAxis == cs->findAxisOfType(CoordinateAxis::Lon)->getName())) {
            // x and y axis not properly defined, guessing
            vector<string> lonShape = cdm.getVariable(orgXAxis).getShape();
            if (lonShape.size() == 2) {
                orgXAxis = lonShape[0];
                orgYAxis = lonShape[1];
                LOG4FIMEX(logger, Logger::WARN, "need to guess x and y axis: " << lonShape[0] << " " << lonShape[1]);
            }
        }
        if (newXAxis.empty()) {
            newXAxis = orgXAxis;
            newYAxis = orgYAxis;
        } else {
            // use the new x/y-axis in shape for all variables
            for (const auto& pv : projectionVariables) {
                const std::string& v = pv.first;
                LOG4FIMEX(logger, Logger::DEBUG, "changing shape for newX/YAxis for : " << v);
                vector<string> shape = cdm.getVariable(v).getShape();
                replace(shape.begin(), shape.end(), orgXAxis, newXAxis);
                replace(shape.begin(), shape.end(), orgYAxis, newYAxis);
                cdm.getVariable(v).setShape(shape);
            }
            cdm.removeVariable(orgXAxis);
            cdm.removeVariable(orgYAxis);
            projectionVariables.erase(orgXAxis);
            projectionVariables.erase(orgYAxis);
        }

        // remove projection and coordinates (lon lat)
        if (cs->hasAxisType(CoordinateAxis::Lat) && cs->hasAxisType(CoordinateAxis::Lon)) {
            CoordinateAxis_cp latAxis = cs->findAxisOfType(CoordinateAxis::Lat);
            CoordinateAxis_cp lonAxis = cs->findAxisOfType(CoordinateAxis::Lon);
            LOG4FIMEX(logger, Logger::DEBUG, "removing old coordinate axes " << latAxis->getName() << " and " << lonAxis->getName());
            cdm.removeVariable(latAxis->getName());
            cdm.removeVariable(lonAxis->getName());
            projectionVariables.erase(latAxis->getName());
            projectionVariables.erase(lonAxis->getName());
        }
        if (orgXAxis != newXAxis) {
            cdm.removeDimension(orgXAxis);
            cdm.removeDimension(orgYAxis);
        }

    }
    assert(newXAxis.size() != 0);
    assert(newYAxis.size() != 0);

    // add new projection and parameters
    std::string newProjection = "latlong";
    if (newProj != "latlong") {
        newProjection = "projection_"+newProj;
        int i = 0;
        while (cdm.hasVariable(newProjection)) newProjection = "projection_"+newProj+type2string(++i);
        CDMVariable projVar(newProjection, CDM_INT, std::vector<std::string>());
        projVar.setData(createData(CDM_INT, 0)); // define empty data
        cdm.addVariable(projVar);
        std::vector<CDMAttribute> projAttrs = Projection::createByProj4(proj_input)->getParameters();
        for (std::vector<CDMAttribute>::iterator it = projAttrs.begin(); it != projAttrs.end(); ++it) {
            cdm.addAttribute(newProjection, *it);
        }
    }

    LOG4FIMEX(logger, Logger::DEBUG, "X, Y: " << newXAxis << ", "<< newYAxis);
    LOG4FIMEX(logger, Logger::DEBUG, "new projection: " << newProjection);
    LOG4FIMEX(logger, Logger::DEBUG, "new proj: " << newProj);


    // change/add new axes
    // don't change the name of the dimension, even if this might look strange if e.g. lon is a projection_x_coordinate
    cdm.removeAttribute(newXAxis, "long_name");
    cdm.removeAttribute(newYAxis, "long_name");
    std::string xStandardName;
    std::string yStandardName;
    std::string xUnit = out_x_axis_unit;
    std::string yUnit = out_y_axis_unit;
    if (newProj == "latlong") {
        xStandardName = "longitude";
        yStandardName = "latitude";
        if (xUnit == "degree") {
            xUnit = "degrees_east";
        }
        if (yUnit == "degree") {
            yUnit = "degrees_north";
        }
    } else if (newProjection == "projection_rotated_latitude_longitude") {
        xStandardName = "grid_longitude";
        yStandardName = "grid_latitude";
    } else {
        xStandardName = "projection_x_coordinate";
        yStandardName = "projection_y_coordinate";
    }
    if (!cdm.hasVariable(newXAxis)) {
        // create dimension-variable
        vector<string> shape;
        shape.push_back(newXAxis);
        cdm.addVariable(CDMVariable(newXAxis, xAxisType, shape));
    } else {
        cdm.getVariable(newXAxis).setDataType(xAxisType);
    }
    if (!cdm.hasVariable(newYAxis)) {
        // create dimension-variable
        vector<string> shape;
        shape.push_back(newYAxis);
        cdm.addVariable(CDMVariable(newYAxis, yAxisType, shape));
    } else {
        cdm.getVariable(newYAxis).setDataType(yAxisType);
    }
    cdm.addOrReplaceAttribute(newXAxis, CDMAttribute("standard_name", xStandardName));
    cdm.addOrReplaceAttribute(newYAxis, CDMAttribute("standard_name", yStandardName));
    cdm.addOrReplaceAttribute(newXAxis, CDMAttribute("units", xUnit));
    cdm.addOrReplaceAttribute(newYAxis, CDMAttribute("units", yUnit));
    cdm.getVariable(newXAxis).setData(createData(xAxisType, out_x_axis.begin(), out_x_axis.end()));
    cdm.getVariable(newYAxis).setData(createData(yAxisType, out_y_axis.begin(), out_y_axis.end()));

    cdm.getDimension(newXAxis).setLength(out_x_axis.size());
    cdm.getDimension(newYAxis).setLength(out_y_axis.size());

    std::string lat(latitudeName);
    std::string lon(longitudeName);
    if (newProj != "latlong") {
        int i = 0;
        while (cdm.hasVariable(lon)) {
            lon = longitudeName + type2string(++i);
        }
        i = 0;
        while (cdm.hasVariable(lat)) {
            lat = latitudeName + type2string(++i);
        }
        cdm.generateProjectionCoordinates(newProjection, newXAxis, newYAxis, lon, lat);
    }

    // find all reprojectable variables and change variable attributes grid_mapping and coordinates
    {
        for (map<string, string>::const_iterator varIt = projectionVariables.begin(); varIt != projectionVariables.end(); ++varIt) {
            if (newProj != "latlong") {
                cdm.addOrReplaceAttribute(varIt->first, CDMAttribute("coordinates", lon + " " + lat));
                cdm.addOrReplaceAttribute(varIt->first, CDMAttribute("grid_mapping", newProjection));
            } else {
                cdm.removeAttribute(varIt->first, "coordinates");
                cdm.removeAttribute(varIt->first, "grid_mapping");
            }
        }
    }
    // add Conventions unless exists
    CDMAttribute convAttr;
    if (cdm.getAttribute(CDM::globalAttributeNS(), "Conventions", convAttr)) {
        string conv = convAttr.getStringValue();
        if (conv.find("CF-") == string::npos) {
            conv += " CF-1.4";
            convAttr = CDMAttribute("Conventions", conv);
        }
    } else {
        convAttr = CDMAttribute("Conventions", "CF-1.4");
    }
    cdm.addOrReplaceAttribute(CDM::globalAttributeNS(), convAttr);
    // remove old (WRF) grid_mapping attribute MAP_PROJ
    CDMAttribute mapProjAttr;
    if (cdm.getAttribute(CDM::globalAttributeNS(), "MAP_PROJ", mapProjAttr)) {
        cdm.removeAttribute(CDM::globalAttributeNS(), "MAP_PROJ");
    }
}

// internal setup for nanoflann kd-tree
template <typename T>
struct PointCloud
{
        struct Point
        {
                T  x,y,z;
        };

        std::vector<Point>  pts;

        // Must return the number of data points
        inline size_t kdtree_get_point_count() const { return pts.size(); }

        // Returns the distance between the vector "p1[0:size-1]" and the data point with index "idx_p2" stored in the class:
        inline T kdtree_distance(const T* p1, const size_t idx_p2, size_t) const
        {
                T d0=p1[0]-pts[idx_p2].x;
                T d1=p1[1]-pts[idx_p2].y;
                T d2=p1[2]-pts[idx_p2].z;
                return d0*d0+d1*d1+d2*d2;
        }

        // Returns the dim'th component of the idx'th point in the class:
        // Since this is inlined and the "dim" argument is typically an immediate value, the
        //  "if/else's" are actually solved at compile time.
        inline T kdtree_get_pt(const size_t idx, int dim) const
        {
                if (dim==0) return pts[idx].x;
                else if (dim==1) return pts[idx].y;
                else return pts[idx].z;
        }

        // Optional bounding-box computation: return false to default to a standard bbox computation loop.
        //   Return true if the BBOX was already computed by the class and returned in "bb" so it can be avoided to redo it again.
        //   Look at bb.size() to find out the expected dimensionality (e.g. 2 or 3 for point clouds)
        template <class BBOX>
        bool kdtree_get_bbox(BBOX&) const
        {
            return false;
        }
};

void flannTranslatePointsToClosestInputCell(double maxDist, vector<double>& pointsOnXAxis, vector<double>& pointsOnYAxis, size_t xAxisSize, size_t yAxisSize, double* lonVals, double* latVals, size_t orgXDimSize, size_t orgYDimSize)
{
    // pointsOnXAxis and pointsOnYAxis as well as lonVals and latVals are now represented in rad

    using namespace nanoflann;
    LOG4FIMEX(logger, Logger::DEBUG, "maximum allowed distance from cell-center: " << maxDist);
    assert(maxDist != 0);

    // all calculations on a sphere with unit 1
    maxDist /= MIFI_EARTH_RADIUS_M;


    time_t start = time(0);
    PointCloud<double> cloud;
    cloud.pts.resize(orgXDimSize*orgYDimSize);
    for (size_t ix = 0; ix < orgXDimSize; ix++) {
        for (size_t iy = 0; iy < orgYDimSize; iy++) {
            size_t pos = ix+iy*orgXDimSize;
            if (!(std::isnan(latVals[pos]) || std::isnan(lonVals[pos]))) {
                double sinLat = sin(latVals[pos]);
                double cosLat = cos(latVals[pos]);
                double sinLon = sin(lonVals[pos]);
                double cosLon = cos(lonVals[pos]);
                cloud.pts[pos].x = cosLat * cosLon;
                cloud.pts[pos].y = cosLat * sinLon;
                cloud.pts[pos].z = sinLat;
            } else {
                cloud.pts[pos].x = MIFI_UNDEFINED_D;
                cloud.pts[pos].y = MIFI_UNDEFINED_D;
                cloud.pts[pos].z = MIFI_UNDEFINED_D;
            }
        }
    }
    // construct a kd-tree index:
    typedef KDTreeSingleIndexAdaptor<L2_Simple_Adaptor<double, PointCloud<double> > ,
                                     PointCloud<double>,
                                     3 /* dim */> my_kd_tree_t;
    my_kd_tree_t index(3 /*dim*/, cloud, KDTreeSingleIndexAdaptorParams(12 /* max leaf */) );
    index.buildIndex();
    LOG4FIMEX(logger, Logger::DEBUG, "finished loading kdTree after " << (time(0) - start) << "s");

    // using square since distance is not sqrt
    const double search_radius = maxDist * maxDist;
    nanoflann::SearchParams params;
    params.sorted = true;
    for (size_t i = 0; i < pointsOnXAxis.size(); i++) {
        double sinLat = sin(pointsOnYAxis[i]);
        double cosLat = cos(pointsOnYAxis[i]);
        double sinLon = sin(pointsOnXAxis[i]);
        double cosLon = cos(pointsOnXAxis[i]);
        const double query_pt[3] = { cosLat * cosLon,
                                     cosLat * sinLon,
                                     sinLat };

        std::vector<std::pair<size_t,double> > ret_matches;
        const size_t nMatches = index.radiusSearch(&query_pt[0], search_radius, ret_matches, params);
        if (nMatches > 0) {
            size_t pos = ret_matches.at(0).first; // pos = ix+orgXDimSize*iy =>
            size_t ix = pos % orgXDimSize;
            size_t iy = pos / orgXDimSize;
            //LOG4FIMEX(logger, Logger::DEBUG, "found (" << RAD_TO_DEG*pointsOnXAxis[i] << "," << RAD_TO_DEG*pointsOnYAxis[i] << ") at (" << ix << "," << iy << ") dist: " << ret_matches.at(0).second);
            pointsOnXAxis[i] = ix;
            pointsOnYAxis[i] = iy;
        } else {
            // set to any value outside the axes (0 - x/y-size)
            pointsOnXAxis[i] = -1000;
            pointsOnYAxis[i] = -1000;
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "finished flannKDTranslatePointsToClosestInputCell");
}

double getGridDistance(vector<double>& pointsOnXAxis, vector<double>& pointsOnYAxis, double* lonVals, double* latVals, size_t orgXDimSize, size_t orgYDimSize) {
    // try to determine a average grid-distance, take some example points, evaluate the max,
    // multiply that with a number slightly bigger than 1 (i use 1.414
    // and define that as grid distance
    vector<double> samples;
    int steps;
    size_t stepSize;
    if (orgXDimSize * orgYDimSize > 1000) {
        steps = 53; // unusual grid-dimension
        stepSize = orgXDimSize * orgYDimSize / steps;
    } else {
        stepSize = 1;
        steps = orgXDimSize * orgYDimSize;
    }
#ifdef _OPENMP
#pragma omp parallel default(none) firstprivate(steps, stepSize, orgXDimSize, orgYDimSize) shared(samples, pointsOnXAxis, pointsOnYAxis, lonVals, latVals) if (steps > 4)
    {
#pragma omp for nowait
#endif
    for (int ik = 0; ik < steps; ik++) { // using int instead of size_t because of openMP < 3.0
        size_t samplePos = ik * stepSize;
        double lon0 = lonVals[samplePos];
        double lat0 = latVals[samplePos];
        if (!(std::isnan(lon0) || std::isnan(lat0))) {
            double min_cos_d = -2; // max possible distance on unit-sphere has cos_d -1 -> d= pi * r
            for (size_t ix = 0; ix < orgXDimSize; ix++) {
                for (size_t iy = 0; iy < orgYDimSize; iy++) {
                    // find smallest distance (= max cosinus value): http://en.wikipedia.org/wiki/Great-circle_distance
                    size_t pos = ix+iy*orgXDimSize;
                    if (pos != samplePos) {
                        double lon1 = lonVals[pos];
                        double lat1 = latVals[pos];
                        if (!(std::isnan(lon1) || std::isnan(lat1))) {

                            double dlon = lon0 - lon1;

                            double cos_d = cos(lat0) * cos(lat1) * cos(dlon) + sin(lat0) * sin(lat1);
                            if (cos_d > min_cos_d) {
                                min_cos_d = cos_d;
                            }
                        }
                    }
                }
            }
#ifdef _OPENMP
#pragma omp critical (cdminterpolator_getgriddistance)
            {
#endif
            samples.push_back(min_cos_d);
#ifdef _OPENMP
            }
#endif
        }
    }
#ifdef _OPENMP
    }
#endif
    double max_grid_d = acos(*(min_element(samples.begin(), samples.end())));
    max_grid_d *= 1.414; // allow a bit larger extrapolation (diagonal = sqrt(2))
    if (max_grid_d > MIFI_PI) max_grid_d = MIFI_PI;
    return max_grid_d;
}

// internal setup for binary search lat/long
class LL_POINT {
public:
    double lat;
    double lon;
    double x;
    double y;
    LL_POINT() : lat(0), lon(0), x(-1.), y(-1.) {}
    LL_POINT(double lat, double lon, double x, double y) : lat(lat), lon(lon), x(x), y(y) {}
    bool operator<(const LL_POINT& rhs) const { return (this->lat < rhs.lat); }
};

// translate all degree-values of pointsOnX/YAxis to the nearest index
// in lonVals/latVals using a binary search in latitude-direction, otherwise brute-force
void fastTranslatePointsToClosestInputCell(vector<double>& pointsOnXAxis, vector<double>& pointsOnYAxis, double* lonVals, double* latVals, size_t orgXDimSize, size_t orgYDimSize)
{
    LOG4FIMEX(logger, Logger::DEBUG, "estimation of ROI of input-data");
    time_t start = time(0);
    double max_grid_d = getGridDistance(pointsOnXAxis, pointsOnYAxis, &lonVals[0], &latVals[0], orgXDimSize, orgYDimSize);
    LOG4FIMEX(logger, Logger::DEBUG, "assuming a ROI of input-data as: "<< (max_grid_d*180/MIFI_PI) << "deg after " << (time(0) - start) << "s");
    double min_grid_cos_d = cos(max_grid_d);

    // 1. order lat/lon after latitude
    // 2. search for closest latitude
    // 3. go up and down in list as long as dlat < min_dist
    std::vector<LL_POINT> latlons;
    latlons.reserve(orgXDimSize*orgYDimSize);
    for (size_t ix = 0; ix < orgXDimSize; ix++) {
        for (size_t iy = 0; iy < orgYDimSize; iy++) {
            size_t pos = ix+iy*orgXDimSize;
            if (!(std::isnan(lonVals[pos]) || std::isnan(latVals[pos]))) {
                latlons.push_back(LL_POINT(latVals[pos],lonVals[pos], ix, iy));
            }
        }
    }
    // sort latlons by latitudes
    sort(latlons.begin(), latlons.end());
#ifdef _OPENMP
#pragma omp parallel default(none) shared(pointsOnXAxis,pointsOnYAxis,latlons, min_grid_cos_d)
    {
#pragma omp for nowait
#endif
    for (int i = 0; i < static_cast<int>(pointsOnXAxis.size()); i++) { // using int instead of size_t because of openMP < 3.0
        //                   lat                   lon
        LL_POINT p(pointsOnYAxis[i], pointsOnXAxis[i], -1., -1.);
        size_t steps = 0;
        double min_cos_d = min_grid_cos_d; // max allowed distance
        double min_d = acos(min_cos_d);
        vector<LL_POINT>::const_iterator it = lower_bound(latlons.begin(), latlons.end(), p);
        vector<LL_POINT>::const_iterator it2 = it;
        // loop until end
        while (it != latlons.end()) {
            steps++;
            double dlon = it->lon - p.lon;
            if (fabs(it->lat - p.lat) > min_d) {
                it = latlons.end(); // all successing
            } else {
                double cos_d = cos(it->lat) * cos(p.lat) * cos(dlon) + sin(it->lat) * sin(p.lat);
                if (cos_d > min_cos_d) { // closer distance
                    min_cos_d = cos_d;
                    min_d = acos(min_cos_d);
                    p.x = it->x;
                    p.y = it->y;
                }
                it++;
            }
        }
        // loop until beginning
        if (it2 != latlons.begin()) {
            do {
                steps++;
                it2--;
                double dlon = it2->lon - p.lon;
                if (fabs(it2->lat - p.lat) > min_d) {
                    it2 = latlons.begin(); // all successing
                } else {
                    double cos_d = cos(it2->lat) * cos(p.lat) * cos(dlon) + sin(it2->lat) * sin(p.lat);
                    if (cos_d > min_cos_d) { // closer distance
                        min_cos_d = cos_d;
                        min_d = acos(min_cos_d);
                        p.x = it2->x;
                        p.y = it2->y;
                    }
                }
            } while (it2 != latlons.begin());
        }

        pointsOnYAxis[i] = p.y;
        pointsOnXAxis[i] = p.x;
    }
#ifdef _OPENMP
    }
#endif
}

/**
 * convert latVals and lonVals to matrices
 * @return lonVals in size (lonSize*latSize)
 */
void lonLatVals2Matrix(shared_array<double>& lonVals, shared_array<double>& latVals, size_t lonSize, size_t latSize)
{
    shared_array<double> matrixLatVals(new double[lonSize * latSize]);
    shared_array<double> matrixLonVals(new double[lonSize * latSize]);
    for (size_t ix = 0; ix < lonSize; ix++) {
        for (size_t iy = 0; iy < latSize; iy++) {
            size_t pos = ix+iy*lonSize;
            matrixLonVals[pos] = lonVals[ix];
            matrixLatVals[pos] = latVals[iy];
        }
    }
    lonVals = matrixLonVals;
    latVals = matrixLatVals;
}

void extractValues(DataPtr data, shared_array<double>& values, size_t& size)
{
    values = data->asDouble();
    size = data->size();
}

} // namespace

void CDMInterpolator::changeProjectionByForwardInterpolation(int method, const string& proj_input, const vector<double>& out_x_axis,
                                                             const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit,
                                                             CDMDataType out_x_axis_type, CDMDataType out_y_axis_type)
{
    // store projection changes to be used in data-section
    // translate temporary new axes from deg2rad if required
    int miupXAxis = MIFI_PROJ_AXIS;
    int miupYAxis = MIFI_PROJ_AXIS;
    vector<double> outXAxisRad, outYAxisRad;
    const vector<double>* outXAxis = &out_x_axis;
    const vector<double>* outYAxis = &out_y_axis;
    std::regex degree(".*degree.*");
    if (std::regex_match(out_x_axis_unit, degree)) {
        outXAxisRad.resize(out_x_axis.size());
        outXAxis = &outXAxisRad;
        transform_deg_to_rad(&out_x_axis[0], out_x_axis.size(), &outXAxisRad[0]);
        miupXAxis = MIFI_LONGITUDE;
        LOG4FIMEX(logger, Logger::DEBUG, "done with outx axis deg->rad");
    }
    if (std::regex_match(out_y_axis_unit, degree)) {
        outYAxisRad.resize(out_y_axis.size());
        outYAxis = &outYAxisRad;
        transform_deg_to_rad(&out_y_axis[0], out_y_axis.size(), &outYAxisRad[0]);
        miupYAxis = MIFI_LATITUDE;
        LOG4FIMEX(logger, Logger::DEBUG, "done with outy axis deg->rad");
    }

    map<string, CoordinateSystem_cp> csMap = findBestCoordinateSystemsAndProjectionVars(false);

    changeCDM(*cdm_.get(), proj_input, csMap, p_->projectionVariables,
              out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit,
              out_x_axis_type, out_y_axis_type,
              getLongitudeName(), getLatitudeName());

    for (map<string, CoordinateSystem_cp>::iterator csIt = csMap.begin(); csIt != csMap.end(); ++csIt) {
        CoordinateSystem_cp cs = csIt->second;
        const string& latitude = cs->findAxisOfType(CoordinateAxis::Lat)->getName();
        const string& longitude = cs->findAxisOfType(CoordinateAxis::Lon)->getName();

        shared_array<double> orgLonVals, orgLatVals;
        size_t orgLatSize, orgLonSize;
        extractValues(p_->dataReader->getScaledData(longitude), orgLonVals, orgLonSize);
        extractValues(p_->dataReader->getScaledData(latitude), orgLatVals, orgLatSize);
        transform_deg_to_rad(orgLatVals.get(), orgLatSize);
        transform_deg_to_rad(orgLonVals.get(), orgLonSize);

        // FIXME the following part also appears in "changeProjectionByCoordinates"
        string orgXDimName, orgYDimName;
        bool latLonProj = (cs->hasProjection() && (cs->getProjection()->getName() == "latitude_longitude"));
        if (!latLonProj && cs->getGeoYAxis()->getName() == latitude) {
            // x and y axis not properly defined, guessing
            vector<string> latShape = p_->dataReader->getCDM().getVariable(latitude).getShape();
            if (latShape.size() != 2) {
                throw CDMException("latitude needs 2 dims for forward interpolation");
            }
            orgXDimName = latShape[0];
            orgYDimName = latShape[1];
        } else {
            orgXDimName = cs->getGeoXAxis()->getName();
            orgYDimName = cs->getGeoYAxis()->getName();
        }
        LOG4FIMEX(logger, Logger::DEBUG, "x and y axis: " << orgXDimName << "," << orgYDimName);
        const size_t orgXDimSize = p_->dataReader->getCDM().getDimension(orgXDimName).getLength();
        const size_t orgYDimSize = p_->dataReader->getCDM().getDimension(orgYDimName).getLength();
        size_t orgXYSize;
        if (latLonProj) {
            // create new latVals and lonVals as a matrix
            lonLatVals2Matrix(orgLonVals, orgLatVals, orgXDimSize, orgYDimSize);
            orgXYSize = orgLonSize * orgLatSize;
        } else if (orgLatSize != orgYDimSize * orgXDimSize || orgLonSize != orgLatSize) {
            throw CDMException("bad org lon/lat size");
        } else {
            orgXYSize = orgLonSize;
        }
        // FIXME end of identical part

        // translate all input points to output-coordinates, stored in lonVals and latVals
        LOG4FIMEX(logger, Logger::DEBUG, "start reprojection of coordinates");
        std::string orgProjStr = LAT_LON_PROJSTR;
        if (MIFI_OK != mifi_project_values(orgProjStr.c_str(), proj_input.c_str(), &orgLonVals[0], &orgLatVals[0], orgXYSize)) {
            throw CDMException("unable to project axes from "+proj_input+ " to " +orgProjStr);
        }

        // translate the converted input-coordinates (lonvals and latvals) to cell-positions in output
        LOG4FIMEX(logger, Logger::DEBUG, "start calculating positions");
        mifi_points2position(&orgLonVals[0], orgXYSize, &(*outXAxis)[0], outXAxis->size(), miupXAxis);
        mifi_points2position(&orgLatVals[0], orgXYSize, &(*outYAxis)[0], outYAxis->size(), miupYAxis);

        // store the interpolation
        LOG4FIMEX(logger, Logger::DEBUG, "creating cached forward interpolation matrix " << orgXDimSize << "x" << orgYDimSize << " => " << out_x_axis.size() << "x" << out_y_axis.size());
        p_->cachedInterpolation[csIt->first] = std::make_shared<CachedForwardInterpolation>(orgXDimName, orgYDimName, method, orgLonVals, orgLatVals,
                                                                                            orgXDimSize, orgYDimSize, out_x_axis.size(), out_y_axis.size());
    }
    if (hasXYSpatialVectors()) {
        LOG4FIMEX(logger, Logger::WARN, "vector data found, but not possible to interpolate with forward-interpolation");
    }
}

void CDMInterpolator::changeProjectionByCoordinates(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis,
                                                    const string& out_x_axis_unit, const string& out_y_axis_unit, CDMDataType out_x_axis_type,
                                                    CDMDataType out_y_axis_type)
{
    map<string, CoordinateSystem_cp> csMap = findBestCoordinateSystemsAndProjectionVars(false);

    changeCDM(*cdm_.get(), proj_input, csMap, p_->projectionVariables,
              out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit,
              out_x_axis_type, out_y_axis_type,
              getLongitudeName(), getLatitudeName());

    // store projection changes to be used in data-section
    // translate temporary new axes from deg2rad if required
    vector<double> outXAxis = out_x_axis;
    vector<double> outYAxis = out_y_axis;
    std::regex degree(".*degree.*");
    bool isMetric = true;
    if (std::regex_match(out_x_axis_unit, degree)) {
        isMetric = false;
        transform_deg_to_rad(outXAxis);
    }
    if (std::regex_match(out_y_axis_unit, degree)) {
        transform_deg_to_rad(outYAxis);
    }

    for (map<string, CoordinateSystem_cp>::iterator csIt = csMap.begin(); csIt != csMap.end(); ++csIt) {
        CoordinateSystem_cp cs = csIt->second;
        const std::string& latitude = cs->findAxisOfType(CoordinateAxis::Lat)->getName();
        const std::string& longitude = cs->findAxisOfType(CoordinateAxis::Lon)->getName();

        shared_array<double> latVals, lonVals;
        size_t latSize, lonSize;
        extractValues(p_->dataReader->getScaledData(latitude), latVals, latSize);
        extractValues(p_->dataReader->getScaledData(longitude), lonVals, lonSize);
        transform_deg_to_rad(&latVals[0], latSize);
        transform_deg_to_rad(&lonVals[0], lonSize);

        string orgXDimName, orgYDimName;
        const bool latLonProj = (cs->hasProjection() && (cs->getProjection()->getName() == "latitude_longitude"));
        if (!latLonProj && cs->getGeoYAxis()->getName() == latitude) {
            // x and y axis not properly defined, guessing
            const vector<string>& latShape = p_->dataReader->getCDM().getVariable(latitude).getShape();
            if (latShape.size() != 2) {
                throw CDMException("latitude needs 2 dims for forward interpolation");
            }
            orgXDimName = latShape[0];
            orgYDimName = latShape[1];
        } else {
            orgXDimName = cs->getGeoXAxis()->getName();
            orgYDimName = cs->getGeoYAxis()->getName();
        }
        LOG4FIMEX(logger, Logger::DEBUG, "x and y axis: " << orgXDimName << "," << orgYDimName);
        const size_t orgXDimSize = p_->dataReader->getCDM().getDimension(orgXDimName).getLength();
        const size_t orgYDimSize = p_->dataReader->getCDM().getDimension(orgYDimName).getLength();
        if (latLonProj) {
            // create new latVals and lonVals as a matrix
            lonLatVals2Matrix(lonVals, latVals, orgXDimSize, orgYDimSize);
        }

        // get output axes expressed in latitude, longitude
        const size_t fieldSize = outXAxis.size() * outYAxis.size();
        vector<double> pointsOnXAxis(fieldSize);
        vector<double> pointsOnYAxis(fieldSize);
        if (MIFI_OK != mifi_project_axes(proj_input.c_str(), LAT_LON_PROJSTR.c_str(), &outXAxis[0], &outYAxis[0], outXAxis.size(), outYAxis.size(),
                                         &pointsOnXAxis[0], &pointsOnYAxis[0])) {
            throw CDMException("unable to project axes from latlon to " + proj_input);
        }
        if (method == MIFI_INTERPOL_COORD_NN) {
            fastTranslatePointsToClosestInputCell(pointsOnXAxis, pointsOnYAxis, &lonVals[0], &latVals[0], orgXDimSize, orgYDimSize);
        } else if (method == MIFI_INTERPOL_COORD_NN_KD) {
            double maxDistance = getMaxDistanceOfInterest(out_x_axis, out_y_axis, isMetric);
            flannTranslatePointsToClosestInputCell(maxDistance, pointsOnXAxis, pointsOnYAxis, outXAxis.size(), outYAxis.size(), &lonVals[0], &latVals[0],
                                                   orgXDimSize, orgYDimSize);
        } else {
            throw CDMException("unkown interpolation method for coordinates: " + type2string(method));
        }

        LOG4FIMEX(logger, Logger::DEBUG,
                  "creating cached coordinate interpolation matrix " << orgXDimSize << "x" << orgYDimSize << " => " << out_x_axis.size() << "x"
                                                                     << out_y_axis.size());
        p_->cachedInterpolation[csIt->first] = createCachedInterpolation(orgXDimName, orgYDimName, method, pointsOnXAxis, pointsOnYAxis, orgXDimSize,
                                                                         orgYDimSize, out_x_axis.size(), out_y_axis.size());
    }

    if (hasXYSpatialVectors()) {
        LOG4FIMEX(logger, Logger::WARN, "vector data found, but not possible? to interpolate with coordinate-interpolation");
    }
}

void CDMInterpolator::changeProjectionByProjectionParameters(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit, CDMDataType out_x_axis_type, CDMDataType out_y_axis_type)
{
    map<string, CoordinateSystem_cp> csMap = findBestCoordinateSystemsAndProjectionVars(true);
    changeCDM(*cdm_.get(), proj_input, csMap, p_->projectionVariables,
              out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit,
              out_x_axis_type, out_y_axis_type,
              getLongitudeName(), getLatitudeName());

    // store projection changes to be used in data-section
    // translate temporary new axes from deg2rad if required
    vector<double> outXAxis = out_x_axis;
    vector<double> outYAxis = out_y_axis;
    int outXAxisType = MIFI_PROJ_AXIS;
    int outYAxisType = MIFI_PROJ_AXIS;
    std::regex degree(".*degree.*");
    if (std::regex_match(out_x_axis_unit, degree)) {
        transform_deg_to_rad(outXAxis);
        outXAxisType = MIFI_LONGITUDE;
    }
    if (std::regex_match(out_y_axis_unit, degree)) {
        transform_deg_to_rad(outYAxis);
        outYAxisType = MIFI_LATITUDE;
    }

    for (map<string, CoordinateSystem_cp>::iterator csIt = csMap.begin(); csIt != csMap.end(); ++csIt) {
        CoordinateSystem_cp cs = csIt->second;

        // translate axes to 'm' if given in other metric units
        std::string orgUnit = cs->getProjection()->isDegree() ? "degree" : "m";
        const std::string& orgXAxisName = cs->getGeoXAxis()->getName();
        const std::string& orgYAxisName = cs->getGeoYAxis()->getName();

        shared_array<double> orgXAxisValsArray, orgYAxisValsArray;
        size_t orgXAxisSize, orgYAxisSize;
        extractValues(p_->dataReader->getScaledDataInUnit(orgXAxisName, orgUnit), orgXAxisValsArray, orgXAxisSize);
        extractValues(p_->dataReader->getScaledDataInUnit(orgYAxisName, orgUnit), orgYAxisValsArray, orgYAxisSize);

        // calculate the mapping from the new projection points to the original axes pointsOnXAxis(x_new, y_new), pointsOnYAxis(x_new, y_new)
        const size_t fieldSize = outXAxis.size() * outYAxis.size();
        vector<double> pointsOnXAxis(fieldSize);
        vector<double> pointsOnYAxis(fieldSize);
        const std::string orgProjStr = cs->getProjection()->getProj4String();
        if (MIFI_OK != mifi_project_axes(proj_input.c_str(), orgProjStr.c_str(), &outXAxis[0], &outYAxis[0], outXAxis.size(), outYAxis.size(),
                                         &pointsOnXAxis[0], &pointsOnYAxis[0])) {
            throw CDMException("unable to project axes from " + orgProjStr + " to " + proj_input);
        }
        LOG4FIMEX(logger, Logger::DEBUG,
                  "mifi_project_axes: " << proj_input << "," << orgProjStr << "," << outXAxis[0] << "," << outYAxis[0] << " => " << pointsOnXAxis[0] << ","
                                        << pointsOnYAxis[0]);

        // translate original axes from deg2rad if required
        int miupXAxis = MIFI_PROJ_AXIS;
        int miupYAxis = MIFI_PROJ_AXIS;
        if (cs->getProjection()->isDegree()) {
            miupXAxis = MIFI_LONGITUDE;
            transform_deg_to_rad(&orgXAxisValsArray[0], orgXAxisSize);
            miupYAxis = MIFI_LATITUDE;
            transform_deg_to_rad(&orgYAxisValsArray[0], orgYAxisSize);
        }
        // translate coordinates (in rad or m) to indices
        mifi_points2position(&pointsOnXAxis[0], fieldSize, orgXAxisValsArray.get(), orgXAxisSize, miupXAxis);
        mifi_points2position(&pointsOnYAxis[0], fieldSize, orgYAxisValsArray.get(), orgYAxisSize, miupYAxis);

        LOG4FIMEX(logger, Logger::DEBUG,
                  "creating cached projection interpolation matrix " << orgXAxisSize << "x" << orgYAxisSize << " => " << out_x_axis.size() << "x"
                                                                     << out_y_axis.size());
        p_->cachedInterpolation[csIt->first] = createCachedInterpolation(orgXAxisName, orgYAxisName, method, pointsOnXAxis, pointsOnYAxis, orgXAxisSize,
                                                                         orgYAxisSize, out_x_axis.size(), out_y_axis.size());
        warnUnlessAllXYSpatialVectorsHaveSameHorizontalId(csIt->first);

        if (hasXYSpatialVectors()) {
            // prepare interpolation of vectors
            LOG4FIMEX(logger, Logger::DEBUG,
                      "creating cached vector projection interpolation matrix " << orgXAxisSize << "x" << orgYAxisSize << " => " << out_x_axis.size() << "x"
                                                                                << out_y_axis.size());
            shared_array<double> matrix(new double[out_x_axis.size() * out_y_axis.size() * 4]);
            mifi_get_vector_reproject_matrix(orgProjStr.c_str(), proj_input.c_str(), &out_x_axis[0], &out_y_axis[0], outXAxisType, outYAxisType, out_x_axis.size(), out_y_axis.size(), matrix.get());
            LOG4FIMEX(logger, Logger::DEBUG, "creating vector reprojection");
            p_->cachedVectorReprojection[csIt->first] =
                std::make_shared<CachedVectorReprojection>(MIFI_VECTOR_KEEP_SIZE, matrix, out_x_axis.size(), out_y_axis.size());
        }
    }
}

namespace {
void changeCDMToLatLonTemplate(CDM& cdm, const string& tmpl_proj_input, const map<string, CoordinateSystem_cp>& csMap, map<string, string>& projectionVariables,
                               const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit,
                               CDMDataType xAxisType, CDMDataType yAxisType, DataPtr tmplLatVals, DataPtr tmplLonVals)
{
    string newProj = getProjectionName(tmpl_proj_input);

    string newXAxis("x");
    string newYAxis("y");

    // expecting latlon - test
    assert(newProj == "latlong");

    for (map<string, CoordinateSystem_cp>::const_iterator csi = csMap.begin(); csi != csMap.end(); ++csi) {
        string key = csi->first;
        CoordinateSystem_cp cs = csi->second;
        string orgProjection;
        if (cs->hasProjection()) {
            orgProjection = cs->getProjection()->getName();
            // remove all other projections, those might confuse other programs, i.e. IDV
            // this is not int the orgProjection name i.e. stereographic but in the variable name projection_stere
            std::vector<std::string> gridMappings = cdm.findVariables("grid_mapping_name", ".*");
            for (size_t i = 0; i < gridMappings.size(); i++) {
                LOG4FIMEX(logger, Logger::DEBUG, "removing projection-variable " << gridMappings[i]);
                cdm.removeVariable(gridMappings[i]);
                projectionVariables.erase(gridMappings[i]);
            }
            LOG4FIMEX(logger, Logger::DEBUG, "original projection: " << orgProjection);
        }

        string orgXAxis = cs->getGeoXAxis()->getName();
        string orgYAxis = cs->getGeoYAxis()->getName();
        if (cs->hasAxisType(CoordinateAxis::Lon) && (cs->getGeoXAxis()->getName() == cs->findAxisOfType(CoordinateAxis::Lon)->getName())) {
            // x and y axis not properly defined, guessing
            vector<string> lonShape = cdm.getVariable(orgXAxis).getShape();
            if (lonShape.size() == 2) {
                orgXAxis = lonShape[0];
                orgYAxis = lonShape[1];
                LOG4FIMEX(logger, Logger::WARN, "need to guess x and y axis: " << lonShape[0] << " " << lonShape[1]);
            }
        }

        // use the new x/y-axis in shape for all variables
        for (map<string, string>::const_iterator varIt = projectionVariables.begin(); varIt != projectionVariables.end(); ++varIt) {
            LOG4FIMEX(logger, Logger::DEBUG, "changing shape for newX/YAxis for : " << varIt->first);
            vector<string> shape = cdm.getVariable(varIt->first).getShape();
            replace(shape.begin(), shape.end(), orgXAxis, newXAxis);
            replace(shape.begin(), shape.end(), orgYAxis, newYAxis);
            cdm.getVariable(varIt->first).setShape(shape);
        }

        // remove projection and coordinates (lon lat)
        if (cs->hasAxisType(CoordinateAxis::Lat) && cs->hasAxisType(CoordinateAxis::Lon)) {
            CoordinateAxis_cp latAxis = cs->findAxisOfType(CoordinateAxis::Lat);
            CoordinateAxis_cp lonAxis = cs->findAxisOfType(CoordinateAxis::Lon);
            LOG4FIMEX(logger, Logger::DEBUG, "removing old coordinate axes " << latAxis->getName() << " and " << lonAxis->getName());
            cdm.removeVariable(latAxis->getName());
            cdm.removeVariable(lonAxis->getName());
            projectionVariables.erase(latAxis->getName());
            projectionVariables.erase(lonAxis->getName());
        }

        if(newXAxis != orgXAxis && newYAxis != orgYAxis) {
            cdm.removeVariable(orgXAxis);
            cdm.removeVariable(orgYAxis);
            projectionVariables.erase(orgXAxis);
            projectionVariables.erase(orgYAxis);
            cdm.removeDimension(orgXAxis);
            cdm.removeDimension(orgYAxis);
        }

    }

    // add new projection and parameters
    std::string newProjection = "latlong";
    if (newProj != "latlong") {
        newProjection = "projection_"+newProj;
        int i = 0;
        while (cdm.hasVariable(newProjection)) newProjection = "projection_"+newProj+type2string(++i);
        CDMVariable projVar(newProjection, CDM_NAT, std::vector<std::string>());
        projVar.setData(createData(CDM_NAT, 0)); // define empty data
        cdm.addVariable(projVar);
        std::vector<CDMAttribute> projAttrs = Projection::createByProj4(tmpl_proj_input)->getParameters();
        for (std::vector<CDMAttribute>::iterator it = projAttrs.begin(); it != projAttrs.end(); ++it) {
            cdm.addAttribute(newProjection, *it);
        }
    }

    // add new projection and parameters
    LOG4FIMEX(logger, Logger::DEBUG, "orgX, orgY: " << newXAxis << ", "<< newYAxis);
    LOG4FIMEX(logger, Logger::DEBUG, "original projection: " << newProjection);
    LOG4FIMEX(logger, Logger::DEBUG, "new projection: " << newProj);

    // change/add new axes
    // don't change the name of the dimension, even if this might look strange if e.g. lon is a projection_x_coordinate
    cdm.removeAttribute(newXAxis, "long_name");
    cdm.removeAttribute(newYAxis, "long_name");

    std::string xTemplAxis("x");
    std::string xStandardName("projection_x_coordinate");
    std::string xLongName("x-coordinate in Cartesian system");
    std::string xAxisAtt("x");
    std::string yTemplAxis("y");
    std::string yStandardName("projection_y_coordinate");
    std::string yLongName("y-coordinate in Cartesian system");
    std::string yAxisAtt("y");

    if (!cdm.hasVariable(xTemplAxis)) {
        // create dimension-variable
        vector<string> shape;
        shape.push_back(xTemplAxis);
        cdm.addVariable(CDMVariable(xTemplAxis, xAxisType, shape));
        cdm.addOrReplaceAttribute(xTemplAxis, CDMAttribute("axis", xAxisAtt));
        cdm.addOrReplaceAttribute(xTemplAxis, CDMAttribute("long_name", xLongName));
        cdm.addOrReplaceAttribute(xTemplAxis, CDMAttribute("standard_name", xStandardName));
        cdm.addOrReplaceAttribute(xTemplAxis, CDMAttribute("units", out_x_axis_unit));
        cdm.getVariable(xTemplAxis).setData(createData(xAxisType, out_x_axis.begin(), out_x_axis.end()));
        cdm.addDimension(CDMDimension(xTemplAxis, out_x_axis.size()));
    } else {
        cdm.getVariable(newXAxis).setDataType(xAxisType);
        cdm.addOrReplaceAttribute(newXAxis, CDMAttribute("axis", xAxisAtt));
        cdm.addOrReplaceAttribute(newXAxis, CDMAttribute("long_name", xLongName));
        cdm.addOrReplaceAttribute(newXAxis, CDMAttribute("standard_name", xStandardName));
        cdm.addOrReplaceAttribute(newXAxis, CDMAttribute("units", out_x_axis_unit));
        cdm.getVariable(newXAxis).setData(createData(xAxisType, out_x_axis.begin(), out_x_axis.end()));
        cdm.getDimension(newXAxis).setLength(out_x_axis.size());
    }

    if (!cdm.hasVariable(yTemplAxis)) {
        // create dimension-variable
        vector<string> shape;
        shape.push_back(yTemplAxis);
        cdm.addVariable(CDMVariable(yTemplAxis, yAxisType, shape));
        cdm.addOrReplaceAttribute(yTemplAxis, CDMAttribute("axis", yAxisAtt));
        cdm.addOrReplaceAttribute(yTemplAxis, CDMAttribute("long_name", yLongName));
        cdm.addOrReplaceAttribute(yTemplAxis, CDMAttribute("standard_name", yStandardName));
        cdm.addOrReplaceAttribute(yTemplAxis, CDMAttribute("units", out_y_axis_unit));
        cdm.getVariable(yTemplAxis).setData(createData(yAxisType, out_y_axis.begin(), out_y_axis.end()));
        cdm.addDimension(CDMDimension(yTemplAxis, out_y_axis.size()));
    } else {
        cdm.getVariable(newYAxis).setDataType(yAxisType);
        cdm.addOrReplaceAttribute(newYAxis, CDMAttribute("axis", yAxisAtt));
        cdm.addOrReplaceAttribute(newYAxis, CDMAttribute("long_name", yLongName));
        cdm.addOrReplaceAttribute(newYAxis, CDMAttribute("standard_name", yStandardName));
        cdm.addOrReplaceAttribute(newYAxis, CDMAttribute("units", out_y_axis_unit));
        cdm.getVariable(newYAxis).setData(createData(yAxisType, out_y_axis.begin(), out_y_axis.end()));
        cdm.getDimension(newYAxis).setLength(out_y_axis.size());
    }

    // adding lat & long from list
    vector<string> shape;
    shape.push_back("x");
    shape.push_back("y");

    // check if the latitude and longitude
    // were already part of the modell
    if(!cdm.hasVariable("latitude"))
        cdm.addVariable(CDMVariable("latitude", CDM_FLOAT, shape));

    cdm.addOrReplaceAttribute("latitude", CDMAttribute("standard_name", "latitude"));
    cdm.addOrReplaceAttribute("latitude", CDMAttribute("long_name", "latitude"));
    cdm.addOrReplaceAttribute("latitude", CDMAttribute("units", "degree_north"));
    cdm.getVariable("latitude").setData(createData(tmplLatVals->size(), tmplLatVals->asFloat()));

    if(!cdm.hasVariable("longitude"))
        cdm.addVariable(CDMVariable("longitude", CDM_FLOAT, shape));

    cdm.addOrReplaceAttribute("longitude", CDMAttribute("standard_name", "longitude"));
    cdm.addOrReplaceAttribute("longitude", CDMAttribute("long_name", "longitude"));
    cdm.addOrReplaceAttribute("longitude", CDMAttribute("units", "degree_east"));
    cdm.getVariable("longitude").setData(createData(tmplLonVals->size(), tmplLonVals->asFloat()));
    // find all reprojectable variables and change variable attributes grid_mapping and coordinates
    {
        for (map<string, string>::const_iterator varIt = projectionVariables.begin(); varIt != projectionVariables.end(); ++varIt) {
            CDMVariable& varRef = cdm.getVariable(varIt->first);
            vector<string> shape = varRef.getShape();
            std::replace(shape.begin(), shape.end(), newXAxis, xTemplAxis);
            std::replace(shape.begin(), shape.end(), newYAxis, yTemplAxis);
            varRef.setShape(shape);
            cdm.addOrReplaceAttribute(varIt->first, CDMAttribute("coordinates", "longitude latitude"));
            cdm.removeAttribute(varIt->first, "grid_mapping");
        }
    }
    if(cdm.hasDimension("longitude")) {
        cdm.removeDimension("longitude");
    }
    if(xTemplAxis != "latitude") {
        cdm.removeDimension("latitude");
    }
}

struct CSGridDefinition {
    std::string key;
    std::string xAxisName;
    std::string yAxisName;
    DataPtr xAxisData;
    DataPtr yAxisData;
};
} // namespace

void CDMInterpolator::changeProjectionByProjectionParametersToLatLonTemplate(int method,
                                                                             const string& tmpl_proj_input,
                                                                             const vector<double>& out_x_axis,
                                                                             const vector<double>& out_y_axis,
                                                                             const string& out_x_axis_unit,
                                                                             const string& out_y_axis_unit,
                                                                             CDMDataType out_x_axis_type,
                                                                             CDMDataType out_y_axis_type,
                                                                             DataPtr tmplLatVals,
                                                                             DataPtr tmplLonVals)
{
    map<string, CoordinateSystem_cp> csMap = findBestCoordinateSystemsAndProjectionVars(true);

    map<string, CSGridDefinition> orgGrids;

    for (map<string, CoordinateSystem_cp>::const_iterator csi = csMap.begin(); csi != csMap.end(); ++csi) {
        // copy all the data one might need before CDM model changes
        CSGridDefinition def;
        def.key = csi->first;
        def.xAxisName = csi->second->getGeoXAxis()->getName();
        def.yAxisName = csi->second->getGeoYAxis()->getName();
        if (csi->second->hasProjection()) {
            std::string orgUnit = csi->second->getProjection()->isDegree() ? "degree" : "m";
            def.xAxisData = p_->dataReader->getScaledDataInUnit(def.xAxisName, orgUnit);
            def.yAxisData = p_->dataReader->getScaledDataInUnit(def.yAxisName, orgUnit);
        } else {
            def.xAxisData = p_->dataReader->getScaledData(def.xAxisName);
            def.yAxisData = p_->dataReader->getScaledData(def.yAxisName);
        }
        orgGrids.insert(std::make_pair(def.key, def));
    }

    changeCDMToLatLonTemplate(*cdm_.get(),
                              tmpl_proj_input,
                              csMap,
                              p_->projectionVariables,
                              out_x_axis,
                              out_y_axis,
                              out_x_axis_unit,
                              out_y_axis_unit,
                              out_x_axis_type,
                              out_y_axis_type,
                              tmplLatVals,
                              tmplLonVals);

    assert(tmplLatVals->size() == tmplLonVals->size());

    for (map<string, CoordinateSystem_cp>::const_iterator csi = csMap.begin(); csi != csMap.end(); ++csi) {
        CSGridDefinition def = orgGrids[csi->first];
        Projection_cp csp = csi->second->getProjection();
        if (!csp)
            continue;

        shared_array<double> orgXAxisArray = def.xAxisData->asDouble();
        shared_array<double> orgYAxisArray = def.yAxisData->asDouble();

        const std::string& orgProjStr = csp->getProj4String();

        // store projection changes to be used in data-section
        // as template data is in degrees we have to do deg2rad
        shared_array<double> tmplLatArray = tmplLatVals->asDouble();
        shared_array<double> tmplLonArray = tmplLonVals->asDouble();
        vector<double> latY(tmplLatArray.get(), tmplLatArray.get()+tmplLatVals->size());
        vector<double> lonX(tmplLonArray.get(), tmplLonArray.get()+tmplLonVals->size());
        transform_deg_to_rad(latY);
        transform_deg_to_rad(lonX);

        // calculate the mapping from the new projection points to the original axes pointsOnXAxis(x_new, y_new), pointsOnYAxis(x_new, y_new)

        // projects lat / lon from template to axis-projection found in model file
        // we want to get template lat/long expressed in terms of the original projection
        if (MIFI_OK != mifi_project_values(tmpl_proj_input.c_str(), orgProjStr.c_str(), &lonX[0], &latY[0], tmplLatVals->size())) {
            throw CDMException("unable to project values from "+orgProjStr+ " to " +tmpl_proj_input.c_str());
        }
        LOG4FIMEX(logger, Logger::DEBUG,
                  "mifi_project_values: " << tmpl_proj_input << "," << orgProjStr << "," << out_x_axis[0] << "," << out_y_axis[0] << " => " << lonX[0] << ","
                                          << latY[0]);

        // now latVals and lonVals are given in original-input coordinates
        // check if we have to translate original axes from deg2rad
        int miupXAxis = MIFI_PROJ_AXIS;
        int miupYAxis = MIFI_PROJ_AXIS;

        if (csp->isDegree()) {
            miupXAxis = MIFI_LONGITUDE;
            transform_deg_to_rad(orgXAxisArray.get(), def.xAxisData->size());
            miupYAxis = MIFI_LATITUDE;
            transform_deg_to_rad(orgYAxisArray.get(), def.yAxisData->size());
        }

        // translate coordinates (in radians) to indices
        mifi_points2position(&latY[0], tmplLatVals->size(), orgYAxisArray.get(), def.yAxisData->size(), miupYAxis);
        mifi_points2position(&lonX[0], tmplLonVals->size(), orgXAxisArray.get(), def.xAxisData->size(), miupXAxis);

        LOG4FIMEX(logger, Logger::DEBUG, "creating cached projection interpolation matrix ("<< csi->first << ") "
                  << def.xAxisData->size() << "x" << def.yAxisData->size()
                  << " => " << out_x_axis.size() << "x" << out_y_axis.size());
        p_->cachedInterpolation[csi->first] = createCachedInterpolation(def.xAxisName, def.yAxisName, method, lonX, latY, def.xAxisData->size(),
                                                                        def.yAxisData->size(), out_x_axis.size(), out_y_axis.size());

        warnUnlessAllXYSpatialVectorsHaveSameHorizontalId(csi->first);

        if (hasXYSpatialVectors()) {
            // as template data is in degrees we have to do deg2rad
            shared_array<double> tmplLatArray = tmplLatVals->asDouble();
            shared_array<double> tmplLonArray = tmplLonVals->asDouble();
            vector<double> latY(tmplLatArray.get(), tmplLatArray.get()+tmplLatVals->size());
            vector<double> lonX(tmplLonArray.get(), tmplLonArray.get()+tmplLonVals->size());
            transform_deg_to_rad(latY);
            transform_deg_to_rad(lonX);

            // std::string orgUnit = csi->second->getProjection()->isDegree() ? "rad" : "m";
            LOG4FIMEX(logger, Logger::DEBUG, "creating cached vector projection interpolation matrix");
            size_t outSize = tmplLatVals->size();
            shared_array<double> matrix(new double[outSize * 4]);
            // prepare interpolation of vectors
            mifi_get_vector_reproject_matrix_points(orgProjStr.c_str(), MIFI_WGS84_LATLON_PROJ4, csp->isDegree() ? 0 : 1, &lonX[0], &latY[0], outSize,
                                                    matrix.get());
            p_->cachedVectorReprojection[csi->first] = std::make_shared<CachedVectorReprojection>(MIFI_VECTOR_KEEP_SIZE, matrix, outSize, 1);
        }
    }

}

void CDMInterpolator::warnUnlessAllXYSpatialVectorsHaveSameHorizontalId(const string& horizontalId) const
{
    const CDM::VarVec& variables = getCDM().getVariables();
    for (const CDMVariable& var : variables) {
        if (!var.isSpatialVector())
            continue;

        // only check x/y, not lat/lon grids
        CDMVariable::SpatialVectorDirection direction = var.getSpatialVectorDirection();
        if (direction != CDMVariable::SPATIAL_VECTOR_X && direction != CDMVariable::SPATIAL_VECTOR_Y)
            continue;

        Impl::projectionVariables_t::const_iterator itP = p_->projectionVariables.find(var.getName());
        if (itP == p_->projectionVariables.end())
            continue;
        if (horizontalId != itP->second)
            continue;

        const string& counterpart = var.getSpatialVectorCounterpart();
        Impl::projectionVariables_t::const_iterator itC = p_->projectionVariables.find(counterpart);
        if (itC != p_->projectionVariables.end() && horizontalId != itC->second) {
            LOG4FIMEX(logger, Logger::WARN, "axes mismatch for vector, e.g. (" << var.getName() << ", " << counterpart << ") might lead to wrong results");
            return;
        }
    }
}

bool CDMInterpolator::hasXYSpatialVectors() const
{
    bool retVal = false;
    const CDM::VarVec& variables = getCDM().getVariables();
    for (const CDMVariable& var : variables) {
        if (!var.isSpatialVector())
            continue;

        Impl::projectionVariables_t::const_iterator itP = p_->projectionVariables.find(var.getName());
        if (itP == p_->projectionVariables.end())
            continue;

        CDMVariable::SpatialVectorDirection direction = var.getSpatialVectorDirection();
        if (direction == CDMVariable::SPATIAL_VECTOR_X || direction == CDMVariable::SPATIAL_VECTOR_Y) {
            retVal = true;

            // check that all vectors have same unit, scale_factor and add_offset
            double scale1, scale2, offset1, offset2;
            getScaleAndOffsetOf(var.getName(), scale1, offset1);
            getScaleAndOffsetOf(var.getSpatialVectorCounterpart(), scale2, offset2);

            string unit1 = getCDM().getUnits(var.getName());
            string unit2 = getCDM().getUnits(var.getSpatialVectorCounterpart());

            if ((scale1 != scale2) || (offset1 != offset2) || (unit1 != unit2)) {
                LOG4FIMEX(logger, Logger::ERROR,
                          "vector (" << var.getName() << "," << var.getSpatialVectorCounterpart()
                                     << ") have different unit,scale_factor or add_offset - rot. vectors might be wrong");
            }
        }
    }

    return retVal;
}

void CDMInterpolator::addPreprocess(InterpolatorProcess2d_p process)
{
    LOG4FIMEX(logger, Logger::DEBUG, "adding interpolation preprocess");
    p_->preprocesses.push_back(process);
}

void CDMInterpolator::addPostprocess(InterpolatorProcess2d_p process)
{
    LOG4FIMEX(logger, Logger::DEBUG, "adding interpolation postprocess");
    p_->postprocesses.push_back(process);
}

} // namespace MetNoFimex
