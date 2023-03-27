/*
 * Fimex, CDMProcessor.cc
 *
 * (C) Copyright 2012-2022, met.no
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
 *  Created on: Mar 19, 2012
 *      Author: Heiko Klein
 */

#include "fimex/CDMProcessor.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CDMVariable.h"
#include "fimex/CachedVectorReprojection.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/HybridSigmaPressure1.h"
#include "fimex/interpolation.h"

#include "fimex/reproject.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <set>

namespace MetNoFimex
{

using namespace std;

static Logger_p logger = getLogger("fimex.CDMProcessor");

typedef map<string, CoordinateSystem_cp> CoordSysMap;

typedef std::shared_ptr<CachedVectorReprojection> CachedVectorReprojection_p;

struct SliceCache {
    string varName;
    size_t ulimDimPos;
    DataPtr data;
};

struct VerticalVelocityComps {
    string wVarName;
    string xWind;
    string yWind;
    string temp;
    string geopot;
    string ps;
    string ap;
    string b;

    size_t nx;
    size_t ny;
    size_t nz;
    double dx;
    double dy;
    shared_array<float> gridDistX;
    shared_array<float> gridDistY;
    DataPtr geopotData;
    bool tempNotDefined() {return temp == "";}
    bool geopotNotDefined() {return geopotData.get() == 0;}
};

struct CDMProcessor::CDMProcessorImpl
{
    CDMReader_p dataReader;
    set<string> deaccumulateVars;
    set<string> accumulateVars;
    // variable -> <counterPart, horizontalId>
    map<string, pair<string, string> > rotateLatLonVectorX;
    // variable -> horizontalId
    map<string, string> rotateLatLonDirection;
    // variable -> <counterPart, horizontalId> (same as above but with Y as first component)
    map<string, pair<string, string> > rotateLatLonVectorY;
    // horizontalId -> cachedVectorReprojection
    map<string, CachedVectorReprojection_p> cachedVectorReprojection;
    SliceCache sliceCache;
    VerticalVelocityComps vvComp;
};

CachedVectorReprojection_p makeCachedVectorReprojection(CDMReader_p dataReader, CoordinateSystem_cp cs, bool toLatLon)
{
    assert(cs->hasProjection());

    CoordinateAxis_cp xAxis = cs->getGeoXAxis();
    assert(xAxis.get() != 0);
    CoordinateAxis_cp yAxis = cs->getGeoYAxis();
    assert(yAxis.get() != 0);

    DataPtr xAxisData;
    DataPtr yAxisData;
    if (cs->getProjection()->isDegree()) {
        xAxisData = dataReader->getScaledDataInUnit(xAxis->getName(), "radian");
        yAxisData = dataReader->getScaledDataInUnit(yAxis->getName(), "radian");
    } else {
        xAxisData = dataReader->getScaledDataInUnit(xAxis->getName(), "m");
        yAxisData = dataReader->getScaledDataInUnit(yAxis->getName(), "m");
    }
    size_t xAxisSize = xAxisData->size();
    size_t yAxisSize = yAxisData->size();
    auto xAxisD = xAxisData->asDouble();
    auto yAxisD = yAxisData->asDouble();

    LOG4FIMEX(logger, Logger::DEBUG, "creating cached vector projection interpolation matrix");
    reproject::Matrix_cp matrix;
    if (toLatLon) {
        auto inXField = make_shared_array<double>(xAxisSize * yAxisSize);
        auto inYField = make_shared_array<double>(xAxisSize * yAxisSize);
        for (size_t i = 0; i < xAxisSize; i++) {
            for (size_t j = 0; j < yAxisSize; j++) {
                inXField[xAxisSize*j + i] = xAxisD[i];
                inYField[xAxisSize*j + i] = yAxisD[j];
            }
        }
        // prepare interpolation of vectors
        matrix = reproject::get_vector_reproject_matrix_field(cs->getProjection()->getProj4String(), MIFI_WGS84_LATLON_PROJ4, &inXField[0], &inYField[0],
                                                              xAxisSize, yAxisSize);
    } else {
        // using MIFI_PROJ_AXIS even for LAT/LON since axes already in radian
        matrix = reproject::get_vector_reproject_matrix(MIFI_WGS84_LATLON_PROJ4, cs->getProjection()->getProj4String(), &xAxisD[0], &yAxisD[0], MIFI_PROJ_AXIS,
                                                        MIFI_PROJ_AXIS, xAxisSize, yAxisSize);
    }
    LOG4FIMEX(logger, Logger::DEBUG, "creating vector reprojection");
    return std::make_shared<CachedVectorReprojection>(matrix);
}

CDMProcessor::CDMProcessor(CDMReader_p dataReader)
: p_(new CDMProcessorImpl())
{
    p_->dataReader = dataReader;
    *cdm_ = p_->dataReader->getCDM();
}

CDMProcessor::~CDMProcessor()
{
}

void CDMProcessor::addVerticalVelocity()
{
    Logger_p logger = getLogger("fimex.CDMProcessor.addVerticalVelocity");
    if (cdm_->hasVariable("upward_air_velocity_ml")) {
        LOG4FIMEX(logger, Logger::INFO, "upward_air_velocity_ml already exists, not calculating new one");
    }
    enhanceVectorProperties(p_->dataReader); // set spatial-vectors
    const CoordinateSystem_cp_v coordSys = listCoordinateSystems(p_->dataReader);

    // find x_wind in hybrid-sigma layers (complete with ap and b)
    vector<string> xWinds = cdm_->findVariables("standard_name","(x|grid_eastward)_wind");
    vector<VerticalVelocityComps> vvcs;
    for (vector<string>::iterator xw = xWinds.begin(); xw != xWinds.end(); xw++) {
        CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordSys, *xw);
        if (cs.get() && cs->hasVerticalTransformation() && cs->isSimpleSpatialGridded()) {
            CoordinateAxis_cp zAxis = cs->getGeoZAxis(); // Z or Lat
            VerticalTransformation_cp vtrans = cs->getVerticalTransformation();
            if (vtrans->getName() == HybridSigmaPressure1::NAME() && vtrans->isComplete()) {
                VerticalVelocityComps vvc;
                vvc.xWind = *xw;
                std::shared_ptr<const HybridSigmaPressure1> vt = std::dynamic_pointer_cast<const HybridSigmaPressure1>(vtrans);
                vvc.ap = vt->ap;
                vvc.ps = vt->ps;
                vvc.b = vt->b;
                vvc.nx = cdm_->getDimension(cs->getGeoXAxis()->getName()).getLength();
                vvc.ny = cdm_->getDimension(cs->getGeoYAxis()->getName()).getLength();
                vvc.nz = cdm_->getDimension(cs->getGeoZAxis()->getName()).getLength();
                auto xValues = p_->dataReader->getData(cs->getGeoXAxis()->getName())->asDouble();
                vvc.dx = fabs(xValues[1] - xValues[0]);
                auto yValues = p_->dataReader->getData(cs->getGeoYAxis()->getName())->asDouble();
                vvc.dy = fabs(yValues[1] - yValues[0]);

                vvc.yWind = p_->dataReader->getCDM().getVariable(*xw).getSpatialVectorCounterpart();
                if (vvc.yWind != "") {
                    vvcs.push_back(vvc);
                }
            }
        }
    }
    if (vvcs.size() == 0) {
        LOG4FIMEX(logger, Logger::WARN, "no x_wind with hybrid-sigma levels and corresponding y_wind found");
        return;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "found x_wind, y_wind: '" << vvcs.at(0).xWind << "','" << vvcs.at(0).yWind << "'");

    // find air_temperature with similar sized axes
    vector<string> temps = cdm_->findVariables("standard_name","air_temperature");
    for (vector<VerticalVelocityComps>::iterator vvcIt = vvcs.begin(); vvcIt != vvcs.end(); vvcIt++) {
        for (vector<string>::iterator tempIt = temps.begin(); tempIt != temps.end(); tempIt++) {
            if (compareCDMVarShapes(*cdm_, vvcIt->xWind, *cdm_, *tempIt)) {
                vvcIt->temp = *tempIt;
            }
        }
    }
    vvcs.erase(remove_if(vvcs.begin(), vvcs.end(), mem_fun_ref(&VerticalVelocityComps::tempNotDefined)), vvcs.end());
    if (vvcs.size() == 0) {
        LOG4FIMEX(logger, Logger::WARN, "no air_temperature found with correspondig x/y_wind");
        return;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "found x_wind, y_wind, temp: '" << vvcs.at(0).xWind << "','" << vvcs.at(0).yWind << "', '" << vvcs.at(0).temp << "'");

    // find geopotential, surface_geopotential or geopotential_height (units: m) (2d)
    vector<string> geopots = cdm_->findVariables("standard_name","(surface_geopotential|altitude|surface_altitude|geopotential_height|geopotential)");
    for (vector<VerticalVelocityComps>::iterator vvcIt = vvcs.begin(); vvcIt != vvcs.end(); vvcIt++) {
        for (vector<string>::iterator gpIt = geopots.begin(); gpIt != geopots.end(); gpIt++) {
            CoordinateSystem_cp gpCs = findCompleteCoordinateSystemFor(coordSys, *gpIt);
            if (gpCs.get()) {
                CoordinateAxis_cp gxAxis = gpCs->getGeoXAxis();
                CoordinateAxis_cp gyAxis = gpCs->getGeoYAxis();
                if (gxAxis != 0 && gyAxis != 0) {
                    size_t gxSize = cdm_->getDimension(gxAxis->getName()).getLength();
                    size_t gySize = cdm_->getDimension(gyAxis->getName()).getLength();
                    if (vvcIt->nx == gxSize && vvcIt->ny == gySize) {
                        SliceBuilder sb(p_->dataReader->getCDM(), *gpIt);
                        sb.setStartAndSize(gxAxis, 0, gxSize);
                        sb.setStartAndSize(gyAxis, 0, gySize);
                        vector<string> unsetv = sb.getUnsetDimensionNames();
                        for (vector<string>::iterator unset = unsetv.begin(); unset != unsetv.end(); unset++){
                            sb.setStartAndSize(*unset, 0, 1);
                        }
                        string stdName = cdm_->getAttribute(*gpIt, "standard_name").getStringValue();
                        string gpUnit;
                        if (stdName == "surface_altitude" || stdName == "altitude" || stdName == "geopotential_height") {
                            gpUnit = "m";
                        } else {
                            gpUnit = "1/9.81*m^2/s^2"; // division of gepotential by gravity
                        }
                        DataPtr gpd;
                        try {
                            gpd = p_->dataReader->getScaledDataSliceInUnit(*gpIt, gpUnit, sb);
                        } catch (CDMException& ex) {
                            LOG4FIMEX(logger, Logger::ERROR, "unable to read '" << *gpIt << "' with unit '"<<gpUnit <<"'");
                        }
                        if (gpd.get() != 0) {
                            vvcIt->geopot = *gpIt;
                            vvcIt->geopotData = gpd;
                            break;
                        }
                    }
                }
            }
        }
    }
    vvcs.erase(remove_if(vvcs.begin(), vvcs.end(), mem_fun_ref(&VerticalVelocityComps::geopotNotDefined)), vvcs.end());
    if (vvcs.size() == 0) {
        LOG4FIMEX(logger, Logger::WARN, "no geopotential/_height/altitude found for corresponding x/y_wind");
        return;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "creating upward_air_velocity_ml with xwind='" << vvcs.at(0).xWind << "', ywind='" << vvcs.at(0).yWind
              << "', temp='" << vvcs.at(0).temp << "', geopot='" << vvcs.at(0).geopot << "'");

    // calculate helper fields
    size_t nx = vvcs.at(0).nx;
    size_t ny = vvcs.at(0).ny;
    string lat, lon;
    cdm_->getLatitudeLongitude(vvcs.at(0).xWind, lat, lon);
    // get 2d coordinates
    DataPtr lonVals = p_->dataReader->getScaledDataInUnit(lon, "degree");
    DataPtr latVals = p_->dataReader->getScaledDataInUnit(lat, "degree");
    shared_array<double> lonlon;
    shared_array<double> latlat;
    if (cdm_->getVariable(lon).getShape().size() == 1) {
        lonlon = make_shared_array<double>(nx * ny);
        latlat = make_shared_array<double>(nx * ny);
        auto lat = latVals->asDouble();
        auto lon = lonVals->asDouble();
        assert(lonVals->size() == nx);
        assert(latVals->size() == ny);
        for (size_t j = 0; j < ny; j++) {
            for (size_t i = 0; i < nx; i++) {
                lonlon[i+j*nx] = lon[i];
                latlat[i+j*nx] = lat[j];
            }
        }
    } else {
        lonlon = lonVals->asDouble();
        latlat = latVals->asDouble();
    }

    auto gridDistX = make_shared_array<float>(nx * ny);
    auto gridDistY = make_shared_array<float>(nx * ny);
    if (MIFI_OK != mifi_griddistance(nx, ny, lonlon.get(), latlat.get(), gridDistX.get(), gridDistY.get())) {
        throw CDMException("addVerticalVelocity: cannot calculate griddistance");
    }
    vvcs.at(0).gridDistX = gridDistX;
    vvcs.at(0).gridDistY = gridDistY;

    // create upward_air_velocity_ml (same shape as wind)
    string uav = "upward_air_velocity_ml";
    CDMVariable uavv(uav, CDM_FLOAT, cdm_->getVariable(vvcs.at(0).xWind).getShape());
    cdm_->addVariable(uavv);
    cdm_->addAttribute(uav, CDMAttribute("units", "m/s"));
    cdm_->addAttribute(uav, CDMAttribute("standard_name", "upward_air_velocity"));
    CDMAttribute coords;
    if (cdm_->getAttribute(vvcs.at(0).xWind, "coordinates", coords)) {
        cdm_->addAttribute(uav, coords);
    }

    // create a calculation-object for selecting slices
    p_->vvComp = vvcs.at(0);
}

void CDMProcessor::accumulate(const std::string& varName)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        p_->accumulateVars.insert(varName);
    } else {
        LOG4FIMEX(logger, Logger::WARN, varName <<  " is not unlimited, ignoring accumulate");
    }
}

void CDMProcessor::deAccumulate(const std::string& varName)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        p_->deaccumulateVars.insert(varName);
    } else {
        LOG4FIMEX(logger, Logger::WARN, varName <<  " is not unlimited, ignoring deaccumulate");
    }
}

void CDMProcessor::rotateAllVectorsToLatLon(bool toLatLon)
{
    // find all vectors and rotate them to lat/lon (or x/y)
    enhanceVectorProperties(CDMReader_p(this, null_deleter()));
    vector<string> varNameX, varNameY, standardNameX, standardNameY;
    CDMVariable::SpatialVectorDirection type = toLatLon ? CDMVariable::SPATIAL_VECTOR_X : CDMVariable::SPATIAL_VECTOR_LON;
    string rot = toLatLon ? "LATLON_ROTATED_" : "GRID_ROTATED_";
    const CDM::VarVec& vars = getCDM().getVariables();
    for (CDM::VarVec::const_iterator vit = vars.begin(); vit != vars.end(); ++vit) {
        if (vit->isSpatialVector() && vit->getSpatialVectorDirection() == type) {
            varNameX.push_back(vit->getName());
            CDMAttribute attr;
            if (getCDM().getAttribute(vit->getName(), "standard_name", attr)) {
                standardNameX.push_back(rot + attr.getStringValue());
            } else {
                standardNameX.push_back(rot + vit->getName());
            }
            varNameY.push_back(vit->getSpatialVectorCounterpart());
            if (getCDM().getAttribute(vit->getSpatialVectorCounterpart(), "standard_name", attr)) {
                standardNameY.push_back(rot + attr.getStringValue());
            } else {
                standardNameY.push_back(rot + vit->getSpatialVectorCounterpart());
            }
       }
    }
    rotateVectorToLatLon(toLatLon, varNameX, varNameY, standardNameX, standardNameY);
}

void CDMProcessor::rotateVectorToLatLon(bool toLatLon, const std::vector<std::string>& varNameX, const std::vector<std::string>& varNameY,
                                        const std::vector<std::string>& stdNameX, const std::vector<std::string>& stdNameY)
{
    if (varNameX.size() != varNameY.size()) {
        throw CDMException("rotateVectorToLatLon requires same number of x as of y variableNames");
    }
    if (varNameX.empty())
        return;

    CoordSysMap coordSysMap;
    map<string, string> projectionVariables;
    vector<string> incompatibleVariables;
    if (0 == findBestHorizontalCoordinateSystems(true, p_->dataReader, coordSysMap, projectionVariables, incompatibleVariables)) {
        LOG4FIMEX(logger, Logger::ERROR, "no coordinate-systems with projection found, rotateVectorToLatLon not possible");
        throw CDMException("no coordinate-systems found");
    }

    for (size_t i = 0; i < varNameX.size(); ++i) {
        if (projectionVariables.find(varNameX[i]) == projectionVariables.end())
            throw CDMException(varNameX[i] + " not rotatable since it does not belong to a horizontal projection");
        if (projectionVariables.find(varNameY[i]) == projectionVariables.end())
            throw CDMException(varNameY[i] + " not rotatable since it does not belong to a horizontal projection");
        // change standard_name
        if (i < stdNameX.size()) {
            cdm_->addOrReplaceAttribute(varNameX[i], CDMAttribute("standard_name", stdNameX.at(i)));
        }
        if (i < stdNameY.size()) {
            cdm_->addOrReplaceAttribute(varNameY[i], CDMAttribute("standard_name", stdNameY.at(i)));
        }
        // change spatial vector properties
        if (toLatLon) {
            cdm_->getVariable(varNameX[i]).setAsSpatialVector(varNameY[i], CDMVariable::SPATIAL_VECTOR_LON);
            cdm_->getVariable(varNameY[i]).setAsSpatialVector(varNameX[i], CDMVariable::SPATIAL_VECTOR_LAT);
        } else {
            cdm_->getVariable(varNameX[i]).setAsSpatialVector(varNameY[i], CDMVariable::SPATIAL_VECTOR_X);
            cdm_->getVariable(varNameY[i]).setAsSpatialVector(varNameX[i], CDMVariable::SPATIAL_VECTOR_Y);
        }

        const string& csXId = projectionVariables[varNameX[i]];
        const string& csYId = projectionVariables[varNameY[i]];
        if (csXId != csYId)
            throw CDMException(varNameX[i] + " belongs to different horizontal CS than " + varNameY[i] + ": " + csXId + " != " +csYId);

        if (p_->cachedVectorReprojection.find(csXId) == p_->cachedVectorReprojection.end()) {
            const CoordSysMap::const_iterator itCS = coordSysMap.find(csXId);
            assert(itCS != coordSysMap.end());
            p_->cachedVectorReprojection[csXId] = makeCachedVectorReprojection(p_->dataReader, itCS->second, toLatLon);
        }
        p_->rotateLatLonVectorX[varNameX[i]] = make_pair(varNameY[i], csXId);
        p_->rotateLatLonVectorY[varNameY[i]] = make_pair(varNameX[i], csXId);
    }
}

void CDMProcessor::rotateDirectionToLatLon(bool toLatLon, const std::vector<std::string>& varNames)
{
    if (varNames.empty())
        return;

    CoordSysMap coordSysMap;
    map<string, string> projectionVariables;
    vector<string> incompatibleVariables;
    if (0 == findBestHorizontalCoordinateSystems(true, p_->dataReader, coordSysMap, projectionVariables, incompatibleVariables)) {
        LOG4FIMEX(logger, Logger::ERROR, "no coordinate-systems with projection found, rotateVectorToLatLon not possible");
        throw CDMException("no coordinate-systems found");
    }

    for (size_t i = 0; i < varNames.size(); ++i) {
        if (projectionVariables.find(varNames[i]) == projectionVariables.end())
            throw CDMException(varNames[i] + " not rotatable since it does not belong to a horizontal projection");

        const string& csId = projectionVariables[varNames[i]];
        p_->rotateLatLonDirection[varNames[i]] = csId;

        if (p_->cachedVectorReprojection.find(csId) == p_->cachedVectorReprojection.end()) {
            const CoordSysMap::const_iterator itCS = coordSysMap.find(csId);
            assert(itCS != coordSysMap.end());
            p_->cachedVectorReprojection[csId] = makeCachedVectorReprojection(p_->dataReader, itCS->second, toLatLon);
        }
    }
}

template <typename T>
struct ScaleOffset
{
    double scale_;
    double offset_;
    ScaleOffset(double scale, double offset) : scale_(scale), offset_(offset) {}
    double operator()(const T& in) const {
        return scale_*in + offset_;
    }
};

template <typename T>
struct UnScaleOffset
{
    double invscale_;
    double offset_;
    UnScaleOffset(double scale, double offset) : invscale_(1/scale), offset_(offset) {}
    T operator()(const double& in) const {
        return invscale_*(in-offset_);
    }
};

static void replaceNanWith0(shared_array<double> dp, size_t n)
{
    mifi_nand2bad(&dp[0], &dp[0]+n, 0);
}

// add d2 to d1 and return d1
static void addDataP2Data(DataPtr& data, DataPtr& dataP, bool addingFirstTimeStep) {
    if ((data->size() != 0) && (dataP->size() != 0)) {
        assert(data->size() == dataP->size());
        auto d = data->asDouble();
        auto dp = dataP->asDouble();
        if (addingFirstTimeStep) {
            // in step 0, replace undef with 0
            replaceNanWith0(dp, dataP->size());
        }
        // this might modify the original data in the reader
        std::transform(&d[0], &d[0]+data->size(), &dp[0], &d[0], std::plus<double>());
        data = createData(data->size(), d);
    } else if (dataP->size() != 0) {
        data = dataP; // data->size was 0
    }
}

DataPtr CDMProcessor::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice for '" << varName << "' at " << unLimDimPos);
    DataPtr data;
    if (varName == "upward_air_velocity_ml" && p_->vvComp.xWind != "") {
        CDMReader_p reader = p_->dataReader;
        size_t nx = p_->vvComp.nx;
        size_t ny = p_->vvComp.ny;
        size_t nz = p_->vvComp.nz;

        assert (nx*ny*nz > 0);

        DataPtr zsD = p_->vvComp.geopotData;
        assert(zsD->size() == nx*ny);
        DataPtr psD = reader->getScaledDataSliceInUnit(p_->vvComp.ps, "Pa", unLimDimPos);
        assert(psD->size() == nx*ny);
        DataPtr apD = reader->getScaledDataInUnit(p_->vvComp.ap, "Pa");
        assert(apD->size() == nz);
        DataPtr bD = reader->getScaledData(p_->vvComp.b);
        assert(bD->size() == nz);
        DataPtr uD = reader->getScaledDataSliceInUnit(p_->vvComp.xWind, "m/s", unLimDimPos);
        assert(uD->size() == nx*ny*nz);
        DataPtr vD = reader->getScaledDataSliceInUnit(p_->vvComp.yWind, "m/s", unLimDimPos);
        assert(vD->size() == nx*ny*nz);
        DataPtr tD = reader->getScaledDataSliceInUnit(p_->vvComp.temp, "K", unLimDimPos);
        assert(tD->size() == nx*ny*nz);


        // output
        auto w = make_shared_array<float>(nx * ny * nz);
        if (MIFI_OK != mifi_compute_vertical_velocity(nx, ny, nz, p_->vvComp.dx, p_->vvComp.dy, p_->vvComp.gridDistX.get(), p_->vvComp.gridDistY.get(),
                                                      apD->asDouble().get(), bD->asDouble().get(), zsD->asFloat().get(), psD->asFloat().get(),
                                                      uD->asFloat().get(), vD->asFloat().get(), tD->asFloat().get(), w.get()))
        {
            throw CDMException("addVerticalVelocity: cannot calculate mifi_compute_vertical_velocity");
        }
        data = createData(nx*ny*nz, w);
    } else {
        data = p_->dataReader->getDataSlice(varName, unLimDimPos);
    }

    // accumulation
    if (p_->accumulateVars.find(varName) != p_->accumulateVars.end()) {
        LOG4FIMEX(logger, Logger::DEBUG, varName << " at slice " << unLimDimPos << " accumulate");
        if (unLimDimPos > 0) { // cannot accumulate first
            size_t start = 0;
            if (p_->sliceCache.varName == varName && p_->sliceCache.ulimDimPos <= unLimDimPos) {
                start = p_->sliceCache.ulimDimPos + 1;
                if (p_->sliceCache.ulimDimPos == unLimDimPos) {
                    data = p_->sliceCache.data;
                } else {
                    DataPtr dataP = p_->sliceCache.data;
                    addDataP2Data(data, dataP, false);
                }
            }
            // data contains last slice + eventually cache
            for (size_t i = start; i <= unLimDimPos-1; ++i) {
                DataPtr dataP = p_->dataReader->getDataSlice(varName, i);
                addDataP2Data(data, dataP, i == 0);
            }
            // fill the cache
            p_->sliceCache.varName = varName;
            p_->sliceCache.ulimDimPos = unLimDimPos;
            p_->sliceCache.data = data;
        }
    }

    // deaccumulation
    if (p_->deaccumulateVars.find(varName) != p_->deaccumulateVars.end()) {
        LOG4FIMEX(logger, Logger::DEBUG, varName << " at slice " << unLimDimPos << " deaccumulate");
        if (unLimDimPos != 0) { // cannot deaccumulate first
            DataPtr dataP = p_->dataReader->getDataSlice(varName, unLimDimPos-1);
            if ((data->size() != 0) && (dataP->size() != 0)) {
                assert(data->size() == dataP->size());
                auto d = data->asDouble();
                auto dp = dataP->asDouble();
                if (unLimDimPos == 1) {
                    // in step 0, replace undef with 0
                    replaceNanWith0(dp, dataP->size());
                }
                // this might modify the original data in the reader
                std::transform(&d[0], &d[0]+data->size(), &dp[0], &d[0], std::minus<double>());
                data = createData(data->size(), d);
            }
        }
    }

    if (p_->rotateLatLonVectorX.find(varName) != p_->rotateLatLonVectorX.end()
            || p_->rotateLatLonVectorY.find(varName) != p_->rotateLatLonVectorY.end())
    {
        if (p_->deaccumulateVars.find(varName) != p_->deaccumulateVars.end()) {
            LOG4FIMEX(logger, Logger::WARN, varName << " deaccumulate and rotated, this won't work as expected");
        }
        DataPtr xData;
        DataPtr yData;
        string xVar, yVar, csId;
        bool xIsFirst;
        if (p_->rotateLatLonVectorX.find(varName) != p_->rotateLatLonVectorX.end()) {
            xIsFirst = true;
            xData = data;
            xVar = varName;
            yVar = p_->rotateLatLonVectorX[varName].first;
            yData = p_->dataReader->getDataSlice(yVar, unLimDimPos);
            csId = p_->rotateLatLonVectorX[varName].second;
        } else {
            xIsFirst = false;
            yData = data;
            yVar = varName;
            xVar = p_->rotateLatLonVectorY[varName].first;
            xData = p_->dataReader->getDataSlice(xVar, unLimDimPos);
            csId = p_->rotateLatLonVectorY[varName].second;
        }
        CachedVectorReprojection_p cvr = p_->cachedVectorReprojection[csId];
        auto xArray = data2InterpolationArray(xData, getCDM().getFillValue(xVar));
        auto yArray = data2InterpolationArray(yData, getCDM().getFillValue(yVar));
        if (xData->size() != yData->size()) {
            throw CDMException("xData != yData in vectorInterpolation");
        }
        cvr->reprojectValues(xArray, yArray, xData->size());
        CDMDataType type = getCDM().getVariable(varName).getDataType();
        if (xIsFirst) {
            data = interpolationArray2Data(type, xArray, xData->size(), getCDM().getFillValue(xVar));
        } else {
            data = interpolationArray2Data(type, yArray, yData->size(), getCDM().getFillValue(yVar));
        }
    }

    if (p_->rotateLatLonDirection.find(varName) != p_->rotateLatLonDirection.end()) {
        if (p_->deaccumulateVars.find(varName) != p_->deaccumulateVars.end()) {
            LOG4FIMEX(logger, Logger::WARN, varName << " deaccumulate and rotated, this won't work as expected");
        }
        const string& csId = p_->rotateLatLonDirection[varName];
        LOG4FIMEX(logger, Logger::DEBUG, "rotating direction " << varName << " with csId " << csId);
        assert(p_->cachedVectorReprojection.find(csId) != p_->cachedVectorReprojection.end());
        CachedVectorReprojection_p cvr = p_->cachedVectorReprojection[csId];
        auto array = data2InterpolationArray(data, getCDM().getFillValue(varName));
        double addOffset = 0.;
        double scaleFactor = 1.;
        getScaleAndOffsetOf(varName, scaleFactor, addOffset);
        transform(&array[0], &array[0]+ data->size(), &array[0], ScaleOffset<float>(scaleFactor, addOffset));
        cvr->reprojectDirectionValues(array, data->size());
        transform(&array[0], &array[0]+ data->size(), &array[0], UnScaleOffset<float>(scaleFactor, addOffset));
        data = interpolationArray2Data(getCDM().getVariable(varName).getDataType(), array, data->size(), getCDM().getFillValue(varName));
    }

    return data;
}

} /* namespace MetNoFimex */
