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
#include "fimex/Utils.h"
#include "fimex/Logger.h"
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex
{

static LoggerPtr logger = getLogger("fimex.CDMPressureConversions");


using namespace std;

struct CDMPressureConversionsImpl {
    vector<string> ops;
    boost::shared_ptr<ToVLevelConverter> pConv;
    vector<string> changeVars;
    string oldTheta;
    boost::shared_ptr<const CoordinateSystem> cs;
};

CDMPressureConversions::CDMPressureConversions(boost::shared_ptr<CDMReader> dataReader, std::vector<std::string> operations)
: dataReader_(dataReader), p_(new CDMPressureConversionsImpl())
{
    p_->ops = operations;
    *cdm_ = dataReader->getCDM();

    // find a 4d variable coordSys
    typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;
    // get all coordinate systems from file
    vector<CoordSysPtr> coordSys = listCoordinateSystems(dataReader_->getCDM());
    for (size_t i = 0; i < coordSys.size(); i++) {
        CoordinateSystem::ConstAxisPtr xAxis = coordSys[i]->getGeoXAxis();
        CoordinateSystem::ConstAxisPtr yAxis = coordSys[i]->getGeoYAxis();
        CoordinateSystem::ConstAxisPtr zAxis = coordSys[i]->getGeoZAxis();
        CoordinateSystem::ConstAxisPtr tAxis = coordSys[i]->getTimeAxis();
        // require x and y axis (ps(x,y)) and obviously zAxis
        if (xAxis != 0 && yAxis != 0 && zAxis != 0 && tAxis != 0 &&
                xAxis->getShape().size() == 1 &&
                yAxis->getShape().size() == 1 &&
                zAxis->getShape().size() == 1 &&
                tAxis->getShape().size() == 1) {
            p_->cs == coordSys[i];
        }
    }
    if (p_->cs.get() == 0)
        throw CDMException("CDMPressureConversions could not find 4d coordinate system");

    CoordinateSystem::ConstAxisPtr xAxis = p_->cs->getGeoXAxis();
    CoordinateSystem::ConstAxisPtr yAxis = p_->cs->getGeoYAxis();
    CoordinateSystem::ConstAxisPtr zAxis = p_->cs->getGeoZAxis();
    CoordinateSystem::ConstAxisPtr tAxis = p_->cs->getTimeAxis();


    for (vector<string>::iterator op = p_->ops.begin(); op != p_->ops.end(); ++op) {
        if (*op == "theta2T") {
            vector<string> dims;
            dims.push_back(xAxis->getShape()[0]);
            dims.push_back(yAxis->getShape()[0]);
            dims.push_back(zAxis->getShape()[0]);
            dims.push_back(tAxis->getShape()[0]);
            map<string, string> atts;
            atts["standard_name"] = "air_potential_temperature";
            vector<string> thetaV = cdm_->findVariables(atts, dims);
            if (!thetaV.size() > 0) {
                LOG4FIMEX(logger, Logger::WARN, "CDMPressureConversion: no air_potential_temperature (theta) found");
            }
            vector<string> shape = cdm_->getVariable(thetaV[0]).getShape();
            vector<CDMAttribute> thetaAtts = cdm_->getAttributes(thetaV[0]);
            cdm_->removeVariable(thetaV[0]);
            p_->oldTheta = thetaV[0];
            string varName = "air_temperature";
            p_->changeVars.push_back(varName);
            cdm_->addVariable(CDMVariable(varName, CDM_FLOAT, shape));
            for (size_t i = 0; i < thetaAtts.size(); i++) {
                if (thetaAtts[i].getName() != "standard_name") {
                    cdm_->addAttribute(varName, thetaAtts[i]);
                }
            }
            cdm_->addAttribute(varName, CDMAttribute("standard_name", "air_temperature"));
        } else if (*op == "add4Dpressure") {
            // TODO
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
    //TODO
    if (varName == "air_temperature") {

    }

}



}
