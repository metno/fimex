/*
 * Fimex, CDMPressureConversions.cc
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

#include "fimex/CDMPressureConversions.h"
#include "fimex/CDMVerticalInterpolator.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "fimex/Logger.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/Logger.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"
#include "fimex/Logger.h"
#include "coordSys/CoordSysUtils.h"

namespace MetNoFimex
{

static LoggerPtr logger = getLogger("fimex.CDMPressureConversions");

using namespace std;

struct CDMPressureConversionsImpl {
    vector<string> ops;
    boost::shared_ptr<ToVLevelConverter> pConv;
    vector<string> changeVars;
    string oldTheta;
    string oldOmega;
    string air_temp;
    boost::shared_ptr<const CoordinateSystem> cs;
};

namespace {

template<class T>
boost::shared_array<T> dataAs(DataPtr data);

template<>
boost::shared_array<float> dataAs<float>(DataPtr data) {
    return data->asFloat();
}

template<>
boost::shared_array<double> dataAs<double>(DataPtr data) {
    return data->asDouble();
}

typedef float VerticalData_t;
typedef boost::shared_array<VerticalData_t> VerticalDataArray;

void convert_omega_to_vertical_wind(size_t size, const double* o, const double* p, const double* t, double* w)
{
    mifi_omega_to_vertical_wind(size, o, p, t, w);
}

void convert_omega_to_vertical_wind(size_t size, const float* o, const float* p, const float* t, float* w)
{
    mifi_omega_to_vertical_wind_f(size, o, p, t, w);
}

} // namespace

CDMPressureConversions::CDMPressureConversions(boost::shared_ptr<CDMReader> dataReader, std::vector<std::string> operations)
: dataReader_(dataReader), p_(new CDMPressureConversionsImpl())
{
    p_->ops = operations;
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(dataReader_);
    *cdm_ = dataReader->getCDM();

    for (vector<string>::iterator op = p_->ops.begin(); op != p_->ops.end(); ++op) {
        if (*op == "theta2T") {
            vector<string> dims;
            map<string, string> atts;
            atts["standard_name"] = "air_potential_temperature";
            vector<string> thetaV = cdm_->findVariables(atts, dims);
            if (thetaV.size() == 0) {
                LOG4FIMEX(logger, Logger::WARN, "no air_potential_temperature (theta) found");
            } else {
                boost::shared_ptr<const CoordinateSystem> cs = findCompleteCoordinateSystemFor(coordSys, thetaV[0]);
                if (!cs.get()) {
                    LOG4FIMEX(logger, Logger::WARN, "no coordinate system for air_potential_temperature (theta) found");
                } else {
                    p_->cs = cs;
                    const vector<string>& shape = cdm_->getVariable(thetaV[0]).getShape();
                    vector<CDMAttribute> thetaAtts = cdm_->getAttributes(thetaV[0]);
                    CDMDataType thetaType = cdm_->getVariable(thetaV[0]).getDataType();
                    cdm_->removeVariable(thetaV[0]);
                    p_->oldTheta = thetaV[0];
                    const string varName = "air_temperature";
                    cdm_->addVariable(CDMVariable(varName, thetaType, shape));
                    for (size_t i = 0; i < thetaAtts.size(); i++) {
                        if (thetaAtts[i].getName() != "standard_name" && thetaAtts[i].getName() != "long_name") {
                            cdm_->addAttribute(varName, thetaAtts[i]);
                        }
                    }
                    cdm_->addAttribute(varName, CDMAttribute("standard_name", "air_temperature"));
                    p_->changeVars.push_back(varName);
                }
            }
        } else if (*op == "omega2vwind") {
            vector<string> dims;
            map<string, string> atts;
            atts["standard_name"] = "(omega|lagrangian_tendency_of_air_pressure|vertical_air_velocity_expressed_as_tendency_of_pressure)";
            vector<string> omegaV = cdm_->findVariables(atts, dims);
            atts["standard_name"] = "air_temperature";
            vector<string> tempV = cdm_->findVariables(atts, dims);
            if (omegaV.size() == 0) {
                LOG4FIMEX(logger, Logger::WARN, "no omega|lagrangian_tendency_of_air_pressure|vertical_air_velocity_expressed_as_tendency_of_pressure found");
            } else {
                if (tempV.size() == 0) {
                    LOG4FIMEX(logger, Logger::WARN, "no air_temperature found needed for omega2vwind");
                } else {
                    boost::shared_ptr<const CoordinateSystem> cs = findCompleteCoordinateSystemFor(coordSys, omegaV[0]);
                    if (!cs.get() || !CompleteCoordinateSystemForComparator(tempV[0])(cs)) {
                        LOG4FIMEX(logger, Logger::WARN, "no coordinate system for omega and air_temperature found");
                    } else {
                        p_->air_temp = tempV[0];
                        p_->cs = cs;
                        const vector<string>& shape = cdm_->getVariable(omegaV[0]).getShape();
                        p_->oldOmega = omegaV[0];
                        const string varName = "upward_air_velocity";
                        cdm_->addVariable(CDMVariable(varName, CDM_FLOAT, shape));
                        cdm_->addAttribute(varName, CDMAttribute("standard_name", "upward_air_velocity"));
                        cdm_->addAttribute(varName, CDMAttribute("units", "m/s"));
                        CDMAttribute xatt;
                        if (cdm_->getAttribute(varName, "coordinates", xatt)) {
                            cdm_->addAttribute(varName, xatt);
                        }
                        cdm_->removeVariable(omegaV[0]);
                        p_->changeVars.push_back(varName);
                    }
                }
            }
        } else if (*op == "add4Dpressure") {
            if (p_->cs.get() == 0) {
                for (size_t i = 0; i < coordSys.size(); i++) {
                    if (coordSys[i]->getGeoXAxis().get() != 0 &&
                            coordSys[i]->getGeoYAxis().get() != 0 &&
                            coordSys[i]->getGeoZAxis().get() != 0 &&
                            coordSys[i]->getTimeAxis().get() != 0 &&
                            coordSys[i]->hasVerticalTransformation())
                    {
                        if (!p_->cs.get()) {
                            p_->cs = coordSys[i];
                        } else {
                            // use the largest coordinate-system with largest zAxis
                            size_t dimsizeNew = cdm_->getDimension(coordSys[i]->getGeoZAxis()->getShape()[0]).getLength();
                            size_t dimsizeOld = cdm_->getDimension(p_->cs->getGeoZAxis()->getShape()[0]).getLength();
                            if (dimsizeNew > dimsizeOld) {
                                p_->cs = coordSys[i];
                            }
                        }
                    }
                }
                if (p_->cs.get() == 0) {
                    throw CDMException("no x,y,z,t 4D-coordinate system found");
                } else {
                    LOG4FIMEX(getLogger("fimex.CDMPressureConverter"), Logger::DEBUG, "add4Dpressure using coordsys: " << *(p_->cs));
                }
            }
            CoordinateSystem::ConstAxisPtr xAxis, yAxis, zAxis, tAxis;
            size_t nx, ny, nz, nt;
            bool tIsUnlimited;
            MetNoFimex::getSimpleAxes(p_->cs, dataReader_->getCDM(),
                    xAxis, yAxis, zAxis, tAxis,
                    nx, ny, nz, nt, tIsUnlimited);
            vector<string> shape;
            shape.push_back(xAxis->getShape()[0]);
            shape.push_back(yAxis->getShape()[0]);
            shape.push_back(zAxis->getShape()[0]);
            shape.push_back(tAxis->getShape()[0]);
            const string varName = "air_pressure4D";
            cdm_->addVariable(CDMVariable(varName, CDM_FLOAT, shape));
            cdm_->addAttribute(varName, CDMAttribute("units", "hPa"));
            cdm_->addAttribute(varName, CDMAttribute("standard_name", "air_pressure"));
            p_->changeVars.push_back(varName);
        } else
            throw CDMException("unknown CDMPressureConversion-operation: " + *op);
    }
}

DataPtr CDMPressureConversions::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    if (find(p_->changeVars.begin(), p_->changeVars.end(), varName) == p_->changeVars.end()) {
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }
    DataPtr pressureData = verticalData4D(p_->cs, dataReader_, unLimDimPos, MIFI_VINT_PRESSURE);
    if (varName == "air_pressure4D")
        return pressureData;

    boost::shared_array<VerticalData_t> pressureValues = dataAs<VerticalData_t>(pressureData);
    const size_t size = pressureData->size();
    if (varName == "air_temperature") {
        boost::shared_array<float> thetaValues = checkData(dataReader_->getDataSlice(p_->oldTheta, unLimDimPos), size, p_->oldTheta)->asFloat();

        const float cp = 1004.; // J/kgK
        const float R = MIFI_GAS_CONSTANT / MIFI_MOLAR_MASS_DRY_AIR; // J/K
        const float ps = 1000.; // hPa
        const float psX1 = 1/ps;
        const float Rcp = R/cp;
        for (size_t i = 0; i < size; i++) {
            // theta = T * (ps / p)^(R/cp) => T = theta * (p/ps)^(R/cp)
            thetaValues[i] *= pow(pressureValues[i]*psX1, Rcp);
        }
        return createData(size, thetaValues);
    } else if (varName == "upward_air_velocity") {
        VerticalDataArray airtempValues = dataAs<VerticalData_t>(checkData(getDataSlice(p_->air_temp, unLimDimPos), size, p_->air_temp));
        VerticalDataArray omegaValues = dataAs<VerticalData_t>(checkData(dataReader_->getScaledDataSliceInUnit(p_->oldOmega, "hPa/s", unLimDimPos), size, p_->oldOmega));

        convert_omega_to_vertical_wind(size, omegaValues.get(), pressureValues.get(), airtempValues.get(), omegaValues.get());
        return createData(size, omegaValues);
    }
    // handled first: if (varName == "air_pressure4D")

    throw CDMException("don't know what to to with variable varName");
}

}
