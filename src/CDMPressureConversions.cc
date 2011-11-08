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
#include "ToVLevelConverter.h"
#include "fimex/Logger.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/Logger.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"

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

CDMPressureConversions::CDMPressureConversions(boost::shared_ptr<CDMReader> dataReader, std::vector<std::string> operations)
: dataReader_(dataReader), p_(new CDMPressureConversionsImpl())
{
    p_->ops = operations;
    *cdm_ = dataReader->getCDM();
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(dataReader_->getCDM());

    for (vector<string>::iterator op = p_->ops.begin(); op != p_->ops.end(); ++op) {
        if (*op == "theta2T") {
            vector<string> dims;
            map<string, string> atts;
            atts["standard_name"] = "air_potential_temperature";
            vector<string> thetaV = cdm_->findVariables(atts, dims);
            if (thetaV.size() == 0) {
                LOG4FIMEX(logger, Logger::WARN, "no air_potential_temperature (theta) found");
            } else {
                vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                        find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(thetaV[0]));
                if (varSysIt == coordSys.end()) {
                    LOG4FIMEX(logger, Logger::WARN, "no coordinate system for air_potential_temperature (theta) found");
                } else {
                    p_->cs = *varSysIt;
                    vector<string> shape = cdm_->getVariable(thetaV[0]).getShape();
                    vector<CDMAttribute> thetaAtts = cdm_->getAttributes(thetaV[0]);
                    CDMDataType thetaType = cdm_->getVariable(thetaV[0]).getDataType();
                    cdm_->removeVariable(thetaV[0]);
                    p_->oldTheta = thetaV[0];
                    string varName = "air_temperature";
                    cdm_->addVariable(CDMVariable(varName, thetaType, shape));
                    for (size_t i = 0; i < thetaAtts.size(); i++) {
                        if (thetaAtts[i].getName() != "standard_name" &&
                                thetaAtts[i].getName() != "long_name") {
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
                    vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                        find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(omegaV[0]));
                    if (varSysIt == coordSys.end() || CompleteCoordinateSystemForComparator(tempV[0])(*varSysIt)) {
                        LOG4FIMEX(logger, Logger::WARN, "no coordinate system for omega and air_temperature found");
                    } else {
                        p_->air_temp = tempV[0];
                        p_->cs = *varSysIt;
                        vector<string> shape = cdm_->getVariable(omegaV[0]).getShape();
                        p_->oldOmega = omegaV[0];
                        string varName = "upward_air_velocity";
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
                            coordSys[i]->getTimeAxis().get() != 0) {
                        p_->cs = coordSys[i];
                        break;
                    }
                }
                if (p_->cs.get() == 0)
                    throw CDMException("no x,y,z,t 4D-coordinate system found");
            }
            CoordinateSystem::ConstAxisPtr xAxis, yAxis, zAxis, tAxis;
            size_t nx, ny, nz, nt;
            bool tIsUnlimited;
            CDMVerticalInterpolator::getSimpleAxes(p_->cs, dataReader_->getCDM(),
                    xAxis, yAxis, zAxis, tAxis,
                    nx, ny, nz, nt, tIsUnlimited);
            vector<string> shape;
            shape.push_back(xAxis->getShape()[0]);
            shape.push_back(yAxis->getShape()[0]);
            shape.push_back(zAxis->getShape()[0]);
            shape.push_back(tAxis->getShape()[0]);
            string varName = "air_pressure4D";
            cdm_->addVariable(CDMVariable(varName, CDM_FLOAT, shape));
            cdm_->addAttribute(varName, CDMAttribute("units", "hPa"));
            cdm_->addAttribute(varName, CDMAttribute("standard_name", "air_pressure"));
            p_->changeVars.push_back(varName);
        } else
            throw CDMException("unknown CDMPressureConversion-operation: " + *op);
    }
}

boost::shared_ptr<Data> CDMPressureConversions::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    if (find(p_->changeVars.begin(), p_->changeVars.end(), varName) == p_->changeVars.end()) {
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }
    // get all axes
    CoordinateSystem::ConstAxisPtr xAxis, yAxis, zAxis, tAxis;
    size_t nx, ny, nz, nt;
    bool tIsUnlimited;
    CDMVerticalInterpolator::getSimpleAxes(p_->cs, dataReader_->getCDM(),
            xAxis, yAxis, zAxis, tAxis,
            nx, ny, nz, nt, tIsUnlimited);
    // changing t-loop if unlimited to a 1-time loop at correct position
    size_t startT = 0;
    if (tIsUnlimited) {
        nt = unLimDimPos + 1;
        startT = unLimDimPos;
    }

    boost::shared_ptr<ToVLevelConverter> pConv = ToVLevelConverter::getPressureConverter(dataReader_, unLimDimPos, zAxis, nx, ny, (nt-startT));
    if (varName == "air_temperature") {
        const float cp = 1004.; // J/kgK
        const float R = 287.; // J/K
        const float ps = 1000.; // hPa
        const float psX1 = 1/ps;
        const float Rcp = R/cp;
        boost::shared_ptr<Data> d = dataReader_->getDataSlice(p_->oldTheta, unLimDimPos);
        assert(d.get() != 0);
        size_t size = d->size();
        boost::shared_array<float> da = d->asFloat();
        d.reset(); // deallocate d
        for (size_t t = startT; t < nt; t++) {
            float *dPos = &da[(t-startT)*(nx*ny*nz)];
            for (size_t y = 0; y < ny; y++) {
                for (size_t x = 0; x < nx; x++) {
                    vector<double> p = (*pConv)(x, y, t);
                    assert(p.size() == nz);
                    for (size_t z = 0; z < nz; z++) {
                        // theta = T * (ps / p)^(R/cp) => T = theta * (p/ps)^(R/cp)
                        dPos[mifi_3d_array_position(x,y,z,nx,ny,nz)] *= pow(static_cast<float>(p[z]*psX1), Rcp);
                    }
                }
            }
        }
        return createData(size, da);
    } else if (varName == "upward_air_velocity") {
        boost::shared_ptr<Data> td = getDataSlice(p_->air_temp, unLimDimPos);
        assert(td.get() != 0);
        boost::shared_array<float> tda = td->asFloat();
        boost::shared_ptr<Data> d = dataReader_->getScaledDataSliceInUnit(p_->oldOmega, "hPa/s", unLimDimPos);
        assert(d.get() != 0);
        size_t size = d->size();
        assert(size == td->size());
        boost::shared_array<float> da = d->asFloat();
        td.reset(); // deallocate td
        d.reset(); // deallocate d
        vector<double> omega(nz);
        vector<double> w(nz);
        vector<double> tv(nz);
        for (size_t t = startT; t < nt; t++) {
            float *dPos = &da[(t-startT)*(nx*ny*nz)];
            float *tPos = &tda[(t-startT)*(nx*ny*nz)];
            for (size_t y = 0; y < ny; y++) {
                for (size_t x = 0; x < nx; x++) {
                    vector<double> p = (*pConv)(x, y, t);
                    assert(p.size() == nz);
                    for (size_t z = 0; z < nz; z++) {
                        size_t xyzPos = mifi_3d_array_position(x,y,z,nx,ny,nz);
                        omega.at(z) = dPos[xyzPos];
                        tv.at(z) = tPos[xyzPos];
                    }
                    mifi_omega_to_vertical_wind(nz, &omega[0], &p[0], &tv[0], &w[0]);
                    for (size_t z = 0; z < nz; z++) {
                        dPos[mifi_3d_array_position(x,y,z,nx,ny,nz)] = w.at(z);
                    }
                }
            }
        }
        return createData(size, da);
    } else if (varName == "air_pressure4D") {
        const size_t size = nx*ny*nz*(nt-startT);
        boost::shared_array<float> ary(new float[size]);
        for (size_t t = startT; t < nt; t++) {
            float *pos = &ary[(t-startT)*(nx*ny*nz)];
            for (size_t y = 0; y < ny; y++) {
                for (size_t x = 0; x < nx; x++) {
                    vector<double> p = (*pConv)(x, y, t);
                    assert(p.size() == nz);
                    for (size_t z = 0; z < nz; z++) {
                        // theta = T * (ps / p)^(R/cp) => T = theta * (p/ps)^(R/cp)
                        pos[mifi_3d_array_position(x,y,z,nx,ny,nz)] = p[z];
                    }
                }
            }
        }
        return createData(size, ary);
    }

    throw CDMException("don't know what to to with variable varName");

}



}
