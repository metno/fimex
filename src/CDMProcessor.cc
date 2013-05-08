/*
 * Fimex, CDMProcessor.cc
 *
 * (C) Copyright 2012, met.no
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
#include "fimex/CDMVariable.h"
#include "fimex/Logger.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/CachedVectorReprojection.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"
#include "fimex/CDMInterpolator.h"
#include <set>
#include <algorithm>
#include <functional>

namespace MetNoFimex
{

using namespace std;

struct CDMProcessorImpl {
    boost::shared_ptr<CDMReader> dataReader;
    set<string> deaccumulateVars;
    set<string> accumulateVars;
    // variable -> <counterPart, horizontalId>
    map<string, pair<string, string> > rotateLatLonVectorX;
    // variable -> <counterPart, horizontalId> (same as above but with Y as first component)
    map<string, pair<string, string> > rotateLatLonVectorY;
    // horizontalId -> cachedVectorReprojection
    map<string, boost::shared_ptr<CachedVectorReprojection> > cachedVectorReprojection;
};

CDMProcessor::CDMProcessor(boost::shared_ptr<CDMReader> dataReader)
: p_(new CDMProcessorImpl())
{
    p_->dataReader = dataReader;
    *cdm_ = p_->dataReader->getCDM();
}

CDMProcessor::~CDMProcessor()
{
}

void CDMProcessor::accumulate(const std::string& varName)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        p_->accumulateVars.insert(varName);
    } else {
        LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::WARN, varName <<  " is not unlimited, ignoring accumulate");
    }

}
void CDMProcessor::deAccumulate(const std::string& varName)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        p_->deaccumulateVars.insert(varName);
    } else {
        LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::WARN, varName <<  " is not unlimited, ignoring deaccumulate");
    }

}
void CDMProcessor::rotateVectorToLatLon(bool toLatLon, const std::vector<std::string>& varNameX, const std::vector<std::string>& varNameY, const std::vector<std::string>& stdNameX, const std::vector<std::string>& stdNameY)
{
    LoggerPtr logger = getLogger("fimex.CDMProcessor");
    if (varNameX.size() != varNameY.size()) {
        throw CDMException("rotateVectorToLatLon requires same number of x as of y variableNames");
    }
    if (varNameX.size() == 0) return;

    typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
    typedef map<string, CoordSysPtr> CoordSysMap;
    typedef vector<CoordSysPtr> CoordSysVec;
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
            cdm_->getVariable(varNameX[i]).setAsSpatialVector(varNameY[i], "longitude");
            cdm_->getVariable(varNameY[i]).setAsSpatialVector(varNameX[i], "latitude");
        } else {
            cdm_->getVariable(varNameX[i]).setAsSpatialVector(varNameY[i], "x");
            cdm_->getVariable(varNameY[i]).setAsSpatialVector(varNameX[i], "y");
        }

        string csXId = projectionVariables[varNameX[i]];
        string csYId = projectionVariables[varNameY[i]];
        if (csXId != csYId)
            throw CDMException(varNameX[i] + " belongs to different horizontal CS than " + varNameY[i] + ": " + csXId + " != " +csYId);

        if (p_->cachedVectorReprojection.find(csXId) == p_->cachedVectorReprojection.end()) {
            assert(coordSysMap.find(csXId) != coordSysMap.end());
            CoordSysPtr cs = coordSysMap[csXId];
            assert(cs->hasProjection());

            typedef CoordinateSystem::ConstAxisPtr ConstAxisPtr;
            ConstAxisPtr xAxis = cs->getGeoXAxis();
            assert(xAxis.get() != 0);
            ConstAxisPtr yAxis = cs->getGeoYAxis();
            assert(yAxis.get() != 0);

            DataPtr xAxisData;
            DataPtr yAxisData;
            if (cs->getProjection()->isDegree()) {
                xAxisData = p_->dataReader->getScaledDataInUnit(xAxis->getName(), "radian");
                yAxisData = p_->dataReader->getScaledDataInUnit(yAxis->getName(), "radian");
            } else {
                xAxisData = p_->dataReader->getScaledDataInUnit(xAxis->getName(), "m");
                yAxisData = p_->dataReader->getScaledDataInUnit(yAxis->getName(), "m");
            }
            size_t xAxisSize = xAxisData->size();
            size_t yAxisSize = yAxisData->size();
            boost::shared_array<double> xAxisD = xAxisData->asDouble();
            boost::shared_array<double> yAxisD = yAxisData->asDouble();


            LOG4FIMEX(logger, Logger::DEBUG, "creating cached vector projection interpolation matrix");
            boost::shared_array<double> matrix(new double[xAxisSize * yAxisSize * 4]);
            if (toLatLon) {
                boost::shared_array<double> inXField(new double[xAxisSize*yAxisSize]);
                boost::shared_array<double> inYField(new double[xAxisSize*yAxisSize]);
                for (size_t i = 0; i < xAxisSize; i++) {
                    for (size_t j = 0; j < yAxisSize; j++) {
                        inXField[xAxisSize*j + i] = xAxisD[i];
                        inYField[xAxisSize*j + i] = yAxisD[j];
                    }
                }
                // prepare interpolation of vectors
                mifi_get_vector_reproject_matrix_field(cs->getProjection()->getProj4String().c_str(), MIFI_WGS84_LATLON_PROJ4, &inXField[0], &inYField[0], xAxisSize, yAxisSize, matrix.get());
            } else {
                // using MIFI_PROJ_AXIS even for LAT/LON since axes already in radian
                mifi_get_vector_reproject_matrix(MIFI_WGS84_LATLON_PROJ4, cs->getProjection()->getProj4String().c_str(), &xAxisD[0], &yAxisD[0], MIFI_PROJ_AXIS, MIFI_PROJ_AXIS, xAxisSize, yAxisSize, matrix.get());
            }
            LOG4FIMEX(logger, Logger::DEBUG, "creating vector reprojection");
            p_->cachedVectorReprojection[csXId] = boost::shared_ptr<CachedVectorReprojection>(new CachedVectorReprojection(MIFI_VECTOR_KEEP_SIZE, matrix, xAxisSize, yAxisSize));

        }
        p_->rotateLatLonVectorX[varNameX[i]] = make_pair(varNameY[i], csXId);
        p_->rotateLatLonVectorY[varNameY[i]] = make_pair(varNameX[i], csXId);
    }

}

DataPtr CDMProcessor::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    DataPtr data = p_->dataReader->getDataSlice(varName, unLimDimPos);
    // accumulation
    if (p_->accumulateVars.find(varName) != p_->accumulateVars.end()) {
        LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::DEBUG, varName << " at slice " << unLimDimPos << " deaccumulate");
        if (unLimDimPos > 0) { // cannot accumulate first
            for (size_t i = 0; i <= unLimDimPos-1; ++i) {
                DataPtr dataP = p_->dataReader->getDataSlice(varName, i);
                if ((data->size() != 0) && (dataP->size() != 0)) {
                    assert(data->size() == dataP->size());
                    boost::shared_array<double> d = data->asDouble();
                    boost::shared_array<double> dp = dataP->asDouble();
                    // this might modify the original data in the reader
                    std::transform(&d[0], &d[0]+data->size(), &dp[0], &d[0], std::plus<double>());
                    data = createData(data->size(), d);
                } else if (dataP->size() != 0) {
                    data = dataP; // data->size was 0
                }
            }
        }
    }
    // deaccumulation
    if (p_->deaccumulateVars.find(varName) != p_->deaccumulateVars.end()) {
        LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::DEBUG, varName << " at slice " << unLimDimPos << " deaccumulate");
        if (unLimDimPos != 0) { // cannot deaccumulate first
            DataPtr dataP = p_->dataReader->getDataSlice(varName, unLimDimPos-1);
            if ((data->size() != 0) && (dataP->size() != 0)) {
                assert(data->size() == dataP->size());
                boost::shared_array<double> d = data->asDouble();
                boost::shared_array<double> dp = dataP->asDouble();
                // this might modify the original data in the reader
                std::transform(&d[0], &d[0]+data->size(), &dp[0], &d[0], std::minus<double>());
                data = createData(data->size(), d);
            }
        }
    }

    if (p_->rotateLatLonVectorX.find(varName) != p_->rotateLatLonVectorX.end() ||
            p_->rotateLatLonVectorY.find(varName) != p_->rotateLatLonVectorY.end()) {
        if (p_->deaccumulateVars.find(varName) != p_->deaccumulateVars.end()) {
            LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::WARN, varName << " deaccumulate and rotated, this won't work as expected");
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
        boost::shared_ptr<CachedVectorReprojection> cvr = p_->cachedVectorReprojection[csId];
        boost::shared_array<float> xArray = data2InterpolationArray(xData, getCDM().getFillValue(xVar));
        boost::shared_array<float> yArray = data2InterpolationArray(yData, getCDM().getFillValue(yVar));
        if (xData->size() != yData->size()) {
            throw CDMException("xData != yData in vectorInterpolation");
        }
        cvr->reprojectValues(xArray, yArray, xData->size());
        if (xIsFirst) {
            data = interpolationArray2Data(xArray, xData->size(), getCDM().getFillValue(xVar));
        } else {
            data = interpolationArray2Data(yArray, yData->size(), getCDM().getFillValue(yVar));
        }
    }

    return data;
}


} /* namespace MetNoFimex */
