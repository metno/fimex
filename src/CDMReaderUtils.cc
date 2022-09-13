/*
 * Fimex, CDMReaderUtils.cc
 *
 * (C) Copyright 2010-2022, met.no
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
 *  Created on: Sep 10, 2010
 *      Author: Heiko Klein
 */

#include "fimex/CDMReaderUtils.h"

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/SharedArray.h"
#include "fimex/TimeUnit.h"
#include "fimex/TimeUtils.h"
#include "fimex/Type2String.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/Projection.h"

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <vector>

namespace MetNoFimex {

using namespace std;

FimexTime getUniqueForecastReferenceTimeFT(CDMReader_p reader)
{
    const CDM& cdm = reader->getCDM();
    // try CF-1.x forecast_reference_time
    const vector<string> refVarnames = cdm.findVariables("standard_name", "forecast_reference_time");
    set<FimexTime> refTimes;
    for (const string& varname : refVarnames) {
        const string units = cdm.getUnits(varname);
        TimeUnit tu(units);
        DataPtr timeData = reader->getData(varname);
        auto times = timeData->asDouble();
        const double* tPtr = &times[0];
        const double* end = tPtr + timeData->size();
        while (tPtr != end) {
            refTimes.insert(tu.unitTime2fimexTime(*tPtr++));
        }
    }
    if (refTimes.empty()) {
        // try WRF-Convention SIMULATION_START_DATE attribute
        CDMAttribute attr;
        if (cdm.getAttribute(cdm.globalAttributeNS(), "SIMULATION_START_DATE", attr)) {
            string sd = attr.getStringValue();
            // replace _ with space
            if (sd.find("_") != string::npos) {
                sd = sd.replace(sd.find("_"), 1, " ");
            }
            refTimes.insert(string2FimexTime(sd));
        }
    }
    if (refTimes.empty()) {
        throw CDMException("no forecast reference time found");
    } else if (refTimes.size() > 1) {
        throw CDMException("forecast reference time not unique");
    }
    return *refTimes.begin();
}

vector<double> getDataSliceInUnit(CDMReader_p reader, const string& var, const string& unit, int unLimDimPos)
{
    DataPtr data = reader->getScaledDataSliceInUnit(var, unit, unLimDimPos);
    auto array = data->asDouble();
    return vector<double>(&array[0], &array[0] + data->size());
}

static std::map<std::string, std::size_t> getCDMDimensionSizes(const CDM& cdm)
{
    map<string, size_t> dimSizes;
    const CDM::DimVec& dims = cdm.getDimensions();
    for (CDM::DimVec::const_iterator dit = dims.begin(); dit != dims.end(); ++dit) {
        dimSizes[dit->getName()] = dit->getLength();
    }
    return dimSizes;
}

std::size_t estimateCDMDataSize(const CDM& cdm)
{
    size_t retVal = 0;
    map<string, size_t> dimSizes = getCDMDimensionSizes(cdm);

    const CDM::VarVec& vars = cdm.getVariables();
    for (CDM::VarVec::const_iterator vit = vars.begin(); vit != vars.end(); ++vit) {
        DataPtr d = createData(vit->getDataType(), 0u);
        const vector<string>& shape = vit->getShape();
        size_t size = 1;
        for (size_t i = 0; i < shape.size(); ++i) {
            size *= dimSizes[shape.at(i)];
        }
        retVal += d->bytes_for_one()*size;
    }
    return retVal;
}

bool compareCDMVarShapes(const CDM& cdm1, const string& varName1, const CDM& cdm2, const string& varName2)
{
    map<string, size_t> dimSizes1 = getCDMDimensionSizes(cdm1);
    map<string, size_t> dimSizes2 = getCDMDimensionSizes(cdm2);

    const vector<string>& shape1 = cdm1.getVariable(varName1).getShape();
    const vector<string>& shape2 = cdm2.getVariable(varName2).getShape();

    vector<string>::const_iterator s1 = shape1.begin();
    vector<string>::const_iterator s2 = shape2.begin();
    while (s1 != shape1.end()) {
        if (dimSizes1[*s1] == 1) {
            // skip dimensions of size 1
            s1++;
            continue;
        }
        // skip dimensions of size 1 for shape2
        while (s2 != shape2.end() && (dimSizes2[*s2] == 1)) {
            s2++;
        }
        if (s2 == shape2.end()) {
            return false; // s2 reached end, s1 not
        } else {
            if (dimSizes1[*s1] != dimSizes2[*s2]) {
                return false;
            } else {
                // shapes equal, nothing to be done here, just go to next index
            }
        }
        s1++;
        s2++;
    }
    // skip dimensions of size 1 for shape2
    while (s2 != shape2.end() && (dimSizes2[*s2] == 1)) {
        s2++;
    }
    if (s2 != shape2.end()) {
        return false; // s1 reached end, s2 not
    }
    return true;
}

string findUniqueDimVarName(const CDM& cdm, const string& baseVar)
{
    if (!(cdm.hasVariable(baseVar) || cdm.hasDimension(baseVar)))
        return baseVar;

    for (size_t i = 0; i < 99; i++) {
        const string varName = baseVar + type2string(i);
        if (!(cdm.hasVariable(varName) || cdm.hasDimension(varName)))
            return varName;
    }
    throw CDMException("unable to generate new dimension/variable name starting with "+ baseVar);
}

std::string findUniqueVarName(const CDM& cdm, const std::string& name)
{
    std::string name_number = name;
    int i = 0;
    while (cdm.hasVariable(name_number))
        name_number = name + type2string(++i);
    return name_number;
}

std::string findUniqueDimName(const CDM& cdm, const std::string& name)
{
    std::string name_number = name;
    int i = 0;
    while (cdm.hasDimension(name_number))
        name_number = name + type2string(++i);
    return name_number;
}

struct CoordsInformation {
    string xDim;
    string yDim;
    set<string> variables;
    Projection_cp proj;
};

void generateProjectionCoordinates(CDMReader_p reader)
{
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    CDM& cdm = reader->getInternalCDM();
    vector<CDMVariable> variables = cdm.getVariables();
    map<string, CoordsInformation> coords;
    for (CoordinateSystem_cp_v::iterator cs = coordSys.begin(); cs != coordSys.end(); ++cs) {
        if ((*cs)->isSimpleSpatialGridded() && (*cs)->hasProjection() &&
              (!((*cs)->hasAxisType(CoordinateAxis::Lat) && (*cs)->hasAxisType(CoordinateAxis::Lon)))) {
            CoordinateAxis_cp x = (*cs)->getGeoXAxis();
            assert(x.get() != 0);
            CoordinateAxis_cp y = (*cs)->getGeoYAxis();
            assert(y.get() != 0);
            string xy = x->getName() + "_" + y->getName();
            coords[xy].xDim = x->getName();
            coords[xy].yDim = y->getName();
            coords[xy].proj = (*cs)->getProjection();
            for (vector<CDMVariable>::iterator varIt = variables.begin(); varIt != variables.end(); ++varIt) {
                string varnm = varIt->getName();
                if ((*cs)->isComplete(varnm) && (*cs)->isCSFor(varnm)) {
                    coords[xy].variables.insert(varnm);
                }
            }
        }
    }

    for (map<string,CoordsInformation>::iterator ci = coords.begin(); ci != coords.end(); ++ci) {
        string lon = findUniqueDimVarName(cdm, "lon");
        string lat = findUniqueDimVarName(cdm, "lat");
        cdm.getVariable(ci->second.xDim).setData(reader->getData(ci->second.xDim));
        cdm.getVariable(ci->second.yDim).setData(reader->getData(ci->second.yDim));
        cdm.generateProjectionCoordinates(ci->second.proj, ci->second.xDim, ci->second.yDim, lon, lat);
        for (set<string>::iterator varIt = ci->second.variables.begin(); varIt != ci->second.variables.end(); ++varIt) {
            cdm.addOrReplaceAttribute(*varIt, CDMAttribute("coordinates", lon + " " +lat));
        }
    }

}

}
