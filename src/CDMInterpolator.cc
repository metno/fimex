/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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


#include "../config.h"
#ifdef HAVE_OPENMP
#include <omp.h>
#endif
#include "fimex/CDM.h"
#include "fimex/CDMInterpolator.h"
#include "proj_api.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/CachedForwardInterpolation.h"
#include "fimex/CachedInterpolation.h"
#include <boost/regex.hpp>
#include <functional>
#include <algorithm>
#include <limits>
#include "fimex/DataImpl.h"
#include "fimex/Logger.h"
#include "fimex/SpatialAxisSpec.h"
#include "kdtree++/kdtree.hpp"

namespace MetNoFimex
{

using namespace std;
typedef boost::shared_ptr<const CoordinateSystem> CoordSysPtr;

const bool DEBUG = false;
static LoggerPtr logger = getLogger("fimex.CDMInterpolator");

CDMInterpolator::CDMInterpolator(boost::shared_ptr<CDMReader> dataReader)
: dataReader(dataReader), latitudeName("lat"), longitudeName("lon")
{
    *cdm_ = dataReader->getCDM();
}

CDMInterpolator::~CDMInterpolator()
{
}

static boost::shared_array<float> data2InterpolationArray(const boost::shared_ptr<Data>& inData, double badValue) {
    boost::shared_array<float> array = inData->asFloat();
    mifi_bad2nanf(&array[0], &array[inData->size()], badValue);
    return array;
}

// for performance reasons, the iData-reference will be modified and used within the return data
static boost::shared_ptr<Data> interpolationArray2Data(boost::shared_array<float> iData, size_t size, double badValue) {
    mifi_nanf2bad(&iData[0], &iData[size], badValue);
    return boost::shared_ptr<Data>(new DataImpl<float>(iData, size));
}

boost::shared_ptr<Data> CDMInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    if (std::find(projectionVariables.begin(), projectionVariables.end(), variable.getName()) == projectionVariables.end()) {
        // no projection, just forward
        return dataReader->getDataSlice(varName, unLimDimPos);
    } else {
        boost::shared_ptr<Data> data = dataReader->getDataSlice(varName, unLimDimPos);
        double badValue = cdm_->getFillValue(varName);
        boost::shared_array<float> array = data2InterpolationArray(data, badValue);
        size_t newSize = 0;
        boost::shared_array<float> iArray = cachedInterpolation->interpolateValues(array, data->size(), newSize);
        if (variable.isSpatialVector()) {
            if (cachedVectorReprojection.get() != 0) {
                // fetch and transpose vector-data
                // transposing needed once for each direction (or caching, but that needs to much memory)
                const std::string& counterpart = variable.getSpatialVectorCounterpart();
                boost::shared_array<float> counterPartArray = data2InterpolationArray(dataReader->getDataSlice(counterpart, unLimDimPos), cdm_->getFillValue(counterpart));
                boost::shared_array<float> counterpartiArray = cachedInterpolation->interpolateValues(counterPartArray, data->size(), newSize);
                const std::string& direction = variable.getSpatialVectorDirection();
                if (direction.find("x") != string::npos || direction.find("longitude") != string::npos) {
                    cachedVectorReprojection->reprojectValues(iArray, counterpartiArray, newSize);
                } else if (direction.find("y") != string::npos || direction.find("latitude") != string::npos) {
                    cachedVectorReprojection->reprojectValues(counterpartiArray, iArray, newSize);
                } else {
                    throw CDMException("could not find x,longitude,y,latitude direction for vector: " + varName + ", direction: " + direction);
                }
            } else {
                LOG4FIMEX(logger, Logger::WARN, "Cannot reproject vector " << variable.getName());
            }
        }
        return interpolationArray2Data(iArray, newSize, badValue);
    }
}

static void degreeToRad(double& val) {
    val *= DEG_TO_RAD;
}

static string getProjectionName(const string& proj_input) {
    // get the new projection
    std::string newProj;
    boost::smatch what;
    if (boost::regex_search(proj_input, what, boost::regex("\\+proj=(\\S+)"))) {
        newProj = what[1].str();
    } else {
        throw CDMException("cannot find +proj=... in proj-string: " + proj_input);
    }
    return newProj;
}


void CDMInterpolator::changeProjection(int method, const string& proj_input, const string& out_x_axis, const string& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit)
{
    SpatialAxisSpec xAxisSpec(out_x_axis);
    SpatialAxisSpec yAxisSpec(out_y_axis);
    if (xAxisSpec.requireStartEnd() || yAxisSpec.requireStartEnd()) {
        // detect the bounding box in the final projection
        CoordSysPtr coordSys = findBestCoordinateSystemAndProjectionVars(false);
        if (projectionVariables.size() < 1) throw CDMException("could not find variables with coordinate system");
        LOG4FIMEX(logger, Logger::DEBUG, "variables with coordinates: " << join(projectionVariables.begin(), projectionVariables.end(), ","));
        string longitude = coordSys->findAxisOfType(CoordinateAxis::Lon)->getName();
        string latitude = coordSys->findAxisOfType(CoordinateAxis::Lat)->getName();
        if (latitude == "" || longitude == "") throw CDMException("could not find lat/long variables");
        const vector<string> dims = cdm_->getVariable(latitude).getShape();
        boost::shared_array<double> latVals = dataReader->getScaledData(latitude)->asConstDouble();
        size_t latSize = dataReader->getData(latitude)->size();
        boost::shared_array<double> lonVals = dataReader->getScaledData(longitude)->asConstDouble();
        for_each(&latVals[0], &latVals[latSize], degreeToRad);
        for_each(&lonVals[0], &lonVals[latSize], degreeToRad);
        if (getProjectionName(proj_input) != "latlong") {
            std::string orgProjStr = "+ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong";
            if (MIFI_OK != mifi_project_values(orgProjStr.c_str(), proj_input.c_str(), &lonVals[0], &latVals[0], latSize)) {
                throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input);
            }
            // lonVals contains now all x-values, latVals all y-values
            // get bounding box:
            double xMin = *(min_element(&lonVals[0], &lonVals[latSize]));
            double yMin = *(min_element(&latVals[0], &latVals[latSize]));
            double xMax = *(max_element(&lonVals[0], &lonVals[latSize]));
            double yMax = *(max_element(&latVals[0], &latVals[latSize]));
            xAxisSpec.setStartEnd(xMin, xMax);
            yAxisSpec.setStartEnd(yMin, yMax);
            LOG4FIMEX(logger, Logger::INFO, "changeProjection, boundingbox: (" <<  xMin << "," << yMin << "), (" << xMax << "," << yMax << ")");
        } else {
            throw CDMException("changeProjection with autotuning axes only implemented for projections in m, not degree yet");
        }
    }
    changeProjection(method, proj_input, xAxisSpec.getAxisSteps(), yAxisSpec.getAxisSteps(), out_x_axis_unit, out_y_axis_unit);
}

void CDMInterpolator::changeProjection(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit)
{
    *cdm_ = dataReader->getCDM(); // reset previous changes
    projectionVariables.assign(0, ""); // reset variables
    switch (method) {
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
    case MIFI_INTERPOL_BILINEAR:
    case MIFI_INTERPOL_BICUBIC:
        changeProjectionByProjectionParameters(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit); break;
    case MIFI_INTERPOL_COORD_NN:
    case MIFI_INTERPOL_COORD_NN_KD:
        changeProjectionByCoordinates(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit); break;
    case MIFI_INTERPOL_FORWARD_SUM:
    case MIFI_INTERPOL_FORWARD_MEAN:
    case MIFI_INTERPOL_FORWARD_MEDIAN:
    case MIFI_INTERPOL_FORWARD_MAX:
    case MIFI_INTERPOL_FORWARD_MIN:
        changeProjectionByForwardInterpolation(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit); break;
    default: throw CDMException("unknown projection method: " + type2string(method));
    }
}

CoordSysPtr CDMInterpolator::findBestCoordinateSystemAndProjectionVars(bool withProjection)
{
    typedef vector<CoordSysPtr> CoordSysVec;
    CoordSysVec coordSys;
    CoordSysVec tempCoordSys = listCoordinateSystems(getCDM());
    for (CoordSysVec::iterator cs = tempCoordSys.begin(); cs != tempCoordSys.end(); ++cs) {
        if (((!withProjection) || ((*cs)->isSimpleSpatialGridded() && (*cs)->hasProjection())) &&
              (withProjection || ((*cs)->hasAxisType(CoordinateAxis::Lat) && (*cs)->hasAxisType(CoordinateAxis::Lon)))) {
            coordSys.push_back(*cs);
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "CS dropped: simpleSpatialGrid="<<(*cs)->isSimpleSpatialGridded() << " projection=" << (*cs)->hasProjection() << " lon="<<(*cs)->hasAxisType(CoordinateAxis::Lon)<< " lat="<<(*cs)->hasAxisType(CoordinateAxis::Lat));
        }
    }
    if (coordSys.size() == 0) {
        LOG4FIMEX(logger, Logger::ERROR, "no coordinate-systems" << (withProjection ? " with projection found, maybe you should try coordinate interpolation" : " found"));
        throw CDMException("no coordinate-systems found");
    }
    // make sure we have only one projection with only one set of axes
    for (CoordSysVec::iterator csx = coordSys.begin(); csx != coordSys.end(); ++csx) {
        if ((*csx)->getGeoXAxis()->getName() != coordSys[0]->getGeoXAxis()->getName()) {
            throw CDMException("CDMInterpolator cannot handle cdms with different axes: " + coordSys[0]->getGeoXAxis()->getName() + " != " + (*csx)->getGeoXAxis()->getName());
        }
        if ((*csx)->getGeoYAxis()->getName() != coordSys[0]->getGeoYAxis()->getName()) {
            throw CDMException("CDMInterpolator cannot handle cdms with different axes: " + coordSys[0]->getGeoYAxis()->getName() + " != " + (*csx)->getGeoYAxis()->getName());
        }
        if (withProjection) {
            if (!(*((*csx)->getProjection()) == *(coordSys[0]->getProjection()))) {
                throw CDMException("CDMInterpolator cannot handle cdms with several projection : " + coordSys[0]->getProjection()->toString() + " != " + (*csx)->getProjection()->toString());
            }
        } else {
            // require lat/lon
            if ((*csx)->findAxisOfType(CoordinateAxis::Lat)->getName() != coordSys[0]->findAxisOfType(CoordinateAxis::Lat)->getName()) {
                throw CDMException("CDMInterpolator cannot handle cdms with different axes: " + coordSys[0]->findAxisOfType(CoordinateAxis::Lat)->getName() + " != " + (*csx)->findAxisOfType(CoordinateAxis::Lat)->getName());
            }
            if ((*csx)->findAxisOfType(CoordinateAxis::Lon)->getName() != coordSys[0]->findAxisOfType(CoordinateAxis::Lon)->getName()) {
                throw CDMException("CDMInterpolator cannot handle cdms with different axes: " + coordSys[0]->findAxisOfType(CoordinateAxis::Lon)->getName() + " != " + (*csx)->findAxisOfType(CoordinateAxis::Lon)->getName());
            }
        }
    }

    // mapping all variables with matching orgX/orgY dimensions
    // find all variables belonging to a cs containing the projection
    projectionVariables.clear();
    const CDM::VarVec& vars = getCDM().getVariables();
    for (CDM::VarVec::const_iterator v = vars.begin(); v != vars.end(); ++v) {
        if (coordSys.end() != find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(v->getName()))) {
            projectionVariables.push_back(v->getName());
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "projection variables: " << join(projectionVariables.begin(), projectionVariables.end(), ","));

    // create a minimal one
    boost::shared_ptr<CoordinateSystem> cs(new CoordinateSystem());
    cs->setConventionName(coordSys[0]->getConventionName());
    cs->setAxis(coordSys[0]->getGeoXAxis());
    cs->setAxis(coordSys[0]->getGeoYAxis());
    // find lat and lon, eventually different from GeoX and GeoY
    for (CoordSysVec::iterator csx = coordSys.begin(); csx != coordSys.end(); ++csx) {
        if ((*csx)->hasProjection()) {
            cs->setProjection(coordSys[0]->getProjection());
        }
        cs->setProjection(coordSys[0]->getProjection());
        if ((*csx)->hasAxisType(CoordinateAxis::Lon)) {
            cs->setAxis((*csx)->findAxisOfType(CoordinateAxis::Lon));
        }
        if ((*csx)->hasAxisType(CoordinateAxis::Lat)) {
            cs->setAxis((*csx)->findAxisOfType(CoordinateAxis::Lat));
        }
    }

    // add all variables to the coordinate system
    for (vector<string>::const_iterator v = projectionVariables.begin(); v != projectionVariables.end(); ++v) {
        cs->setCSFor(*v);
    }

    LOG4FIMEX(logger,Logger::DEBUG, "interpolator of cs " << *cs);
    return cs;
}

/**
 * make changes in the CDM structure to reflect the new projection (attributes, coordinates, projection-variable, dimensions)
 *
 * @param cmd
 * @param proj_input
 * @param orgProjection
 * @param orgXAxis
 * @param orgYAxis
 */
//void changeCDM(CDM& cdm, const string& proj_input, const string& orgProjection, const vector<string>& projectionVariables, const string& orgXAxis, const string& orgYAxis, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit, const string& longitudeName, const string& latitudeName)
static void changeCDM(CDM& cdm, const string& proj_input, const CoordSysPtr& cs, const vector<string>& projectionVariables, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit, const string& longitudeName, const string& latitudeName)
{
    string newProj = getProjectionName(proj_input);
    string orgProjection;
    if (cs->hasProjection()) {
        orgProjection = cs->getProjection()->getName();
        // remove all other projections, those might confuse other programs, i.e. IDV
        // this is not int the orgProjection name i.e. stereographic but in the variable name projection_stere
        std::vector<std::string> gridMappings = cdm.findVariables("grid_mapping_name", ".*");
        for (size_t i = 0; i < gridMappings.size(); i++) {
            LOG4FIMEX(logger, Logger::DEBUG, "removing projection-variable " << gridMappings[i]);
            cdm.removeVariable(gridMappings[i]);
        }
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

    // remove projection and coordinates (lon lat)
    if (cs->hasAxisType(CoordinateAxis::Lat) && cs->hasAxisType(CoordinateAxis::Lon)) {
        typedef CoordinateSystem::ConstAxisPtr ConstAxisPtr;
        ConstAxisPtr latAxis = cs->findAxisOfType(CoordinateAxis::Lat);
        ConstAxisPtr lonAxis = cs->findAxisOfType(CoordinateAxis::Lon);
        LOG4FIMEX(logger, Logger::DEBUG, "removing old coordinate axes " << latAxis->getName() << " and " << lonAxis->getName());
        cdm.removeVariable(latAxis->getName());
        cdm.removeVariable(lonAxis->getName());
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
        std::vector<CDMAttribute> projAttrs = Projection::createByProj4(proj_input)->getParameters();
        for (std::vector<CDMAttribute>::iterator it = projAttrs.begin(); it != projAttrs.end(); ++it) {
            cdm.addAttribute(newProjection, *it);
        }
    }

    LOG4FIMEX(logger, Logger::DEBUG, "orgX, orgY: " << orgXAxis << ", "<< orgYAxis);
    LOG4FIMEX(logger, Logger::DEBUG, "original projection: " << orgProjection);
    LOG4FIMEX(logger, Logger::DEBUG, "new projection: " << newProjection);
    LOG4FIMEX(logger, Logger::DEBUG, "new proj: " << newProj);


    // change/add new axes
    // don't change the name of the dimension, even if this might look strange if e.g. lon is a projection_x_coordinate
    cdm.removeAttribute(orgXAxis, "long_name");
    cdm.removeAttribute(orgYAxis, "long_name");
    std::string xStandardName;
    std::string yStandardName;
    if (newProj == "latlong") {
        xStandardName = "longitude";
        yStandardName = "latitude";
    } else if (newProjection == "projection_rotated_latitude_longitude") {
        xStandardName = "grid_longitude";
        yStandardName = "grid_latitude";
    } else {
        xStandardName = "projection_x_coordinate";
        yStandardName = "projection_y_coordinate";
    }
    if (!cdm.hasVariable(orgXAxis)) {
        // create dimension-variable
        vector<string> shape;
        shape.push_back(orgXAxis);
        cdm.addVariable(CDMVariable(orgXAxis, CDM_DOUBLE, shape));
    } else {
        cdm.getVariable(orgXAxis).setDataType(CDM_DOUBLE);
    }
    if (!cdm.hasVariable(orgYAxis)) {
        // create dimension-variable
        vector<string> shape;
        shape.push_back(orgYAxis);
        cdm.addVariable(CDMVariable(orgYAxis, CDM_DOUBLE, shape));
    } else {
        cdm.getVariable(orgYAxis).setDataType(CDM_DOUBLE);
    }
    cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("standard_name", xStandardName));
    cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("standard_name", yStandardName));
    cdm.addOrReplaceAttribute(orgXAxis, CDMAttribute("units", out_x_axis_unit));
    cdm.addOrReplaceAttribute(orgYAxis, CDMAttribute("units", out_y_axis_unit));
    cdm.getVariable(orgXAxis).setData(createData(CDM_DOUBLE, out_x_axis.begin(), out_x_axis.end()));
    cdm.getVariable(orgYAxis).setData(createData(CDM_DOUBLE, out_y_axis.begin(), out_y_axis.end()));

    cdm.getDimension(orgXAxis).setLength(out_x_axis.size());
    cdm.getDimension(orgYAxis).setLength(out_y_axis.size());

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
        cdm.generateProjectionCoordinates(newProjection, orgXAxis, orgYAxis, lon, lat);
    }

    // find all reprojectible variables and change variable attributes grid_mapping and coordinates
    {
        for (std::vector<std::string>::const_iterator varIt = projectionVariables.begin(); varIt != projectionVariables.end(); ++varIt) {
            if (newProj != "latlong") {
                cdm.addOrReplaceAttribute(*varIt, CDMAttribute("coordinates", lon + " " + lat));
                cdm.addOrReplaceAttribute(*varIt, CDMAttribute("grid_mapping", newProjection));
            } else {
                cdm.removeAttribute(*varIt, "coordinates");
                cdm.removeAttribute(*varIt, "grid_mapping");
            }
        }
    }

}

// internal setup for kd-tree
typedef double p2dtype;
class point2d {
private:
    p2dtype x;
    p2dtype y;
    int i;
    int j;
public:
    point2d(p2dtype x, p2dtype y, int i, int j) : x(x), y(y), i(i), j(j) {}
    point2d() : x(0), y(0), i(-1), j(-1) {}
    // squared distance
    double distance_to(const point2d& rhs) const {return (x-rhs.x)*(x-rhs.x) + (y-rhs.y)*(y-rhs.y);}
    p2dtype operator[](size_t N) const { assert(N<2); return (N == 0) ? x : y; }
    p2dtype getX() const {return x;}
    p2dtype getY() const {return y;}
    int getI() const {return i;}
    int getJ() const {return j;}
};
inline bool operator==(const point2d& A, const point2d& B)
{
    return A.getX() == B.getX() && A.getY() == B.getY();
}
std::ostream& operator<<(std::ostream& out, const point2d& P)
{
    return out << '(' << P.getX() << ',' << P.getY() << ')';
}
// p2d accessor
inline p2dtype pac(point2d p, size_t k) { return p[k]; }
typedef KDTree::KDTree<2,point2d,std::pointer_to_binary_function<point2d,size_t,p2dtype> > tree_type;

class DoesNotEqual {
    point2d p;
public:
    DoesNotEqual(point2d p) : p(p) {}
    bool operator()( const point2d& rhs ) const { return !(p == rhs); };
};


void kdTreeTranslatePointsToClosestInputCell(vector<double>& pointsOnXAxis, vector<double>& pointsOnYAxis, double* lonVals, double* latVals, size_t orgXDimSize, size_t orgYDimSize)
{
    // find the max distance of two neighboring points in the output
    double maxDist = 0;
    {
        vector<point2d> xyPoints;
        xyPoints.reserve(pointsOnXAxis.size());
        for (size_t i = 0; i < pointsOnXAxis.size(); i++) {
            xyPoints.push_back(point2d(pointsOnXAxis[i], pointsOnYAxis[i], -1, -1));
        }
        tree_type xyTree(xyPoints.begin(), xyPoints.end(), std::ptr_fun(pac));
        xyPoints.clear(); // release memory
        for (size_t i = 0; i < pointsOnXAxis.size(); i++) {
            point2d target(pointsOnXAxis[i], pointsOnYAxis[i], -1, -1);
            std::pair<tree_type::const_iterator,double> found = xyTree.find_nearest_if(target, std::numeric_limits<double>::max(), DoesNotEqual(target));
            assert(found.first != xyTree.end());
            if (found.second> maxDist) {
                maxDist = found.second;
            }
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "maxDist: " << maxDist);
    assert(maxDist != 0);

    // pointsOnXAxis and pointsOnYAxis as well as lonVals and latVals are now represented in m on projectionSpace
    vector<point2d> llPoints;
    llPoints.reserve(orgXDimSize*orgYDimSize);
    for (size_t ix = 0; ix < orgXDimSize; ix++) {
        for (size_t iy = 0; iy < orgYDimSize; iy++) {
            size_t pos = ix+iy*orgXDimSize;
            if (!(isnan(lonVals[pos]) || isnan(latVals[pos]))) {
                llPoints.push_back(point2d(lonVals[pos], latVals[pos], ix, iy));
            }
        }
    }
    tree_type lonLatTree(llPoints.begin(), llPoints.end(), std::ptr_fun(pac));
    llPoints.clear(); // release memory

    for (size_t i = 0; i < pointsOnXAxis.size(); i++) {
        point2d target(pointsOnXAxis[i], pointsOnYAxis[i], -1, -1);
        std::pair<tree_type::const_iterator,double> found = lonLatTree.find_nearest(target, maxDist);
        if (found.first != lonLatTree.end()) {
            LOG4FIMEX(logger, Logger::DEBUG, "found (" << pointsOnXAxis[i] << "," << pointsOnYAxis[i] << ") at (" << found.first->getI() << "," << found.first->getJ() << ")");
            pointsOnXAxis[i] = found.first->getI();
            pointsOnYAxis[i] = found.first->getJ();
        }
    }
    LOG4FIMEX(logger, Logger::DEBUG, "finished kdTreeTranslatePointsToClosestInputCell");
}

double getGridDistance(vector<double>& pointsOnXAxis, vector<double>& pointsOnYAxis, double* lonVals, double* latVals, size_t orgXDimSize, size_t orgYDimSize) {
    // try to determine a average grid-distance, take some example points, evaluate the max,
    // multiply that with a number slightly bigger than 1 (i use 1.414
    // and define that as grid distance
    vector<double> samples;
    size_t sampler = 13; // unusual grid-dimension
#ifdef HAVE_OPENMP
#pragma omp parallel default(shared)
    {
        omp_lock_t my_lock;
        omp_init_lock(&my_lock);
#pragma omp for
#endif
    for (int ik = 0; ik < static_cast<int>(pointsOnXAxis.size()/sampler); ik++) { // using int instead of size_t because of openMP < 3.0
        size_t samplePos = sampler * ik;
        double lon0 = lonVals[samplePos];
        double lat0 = latVals[samplePos];
        if (!(isnan(lon0) || isnan(lat0))) {
            double min_cos_d = -2; // max possible distance on unit-sphere has cos_d -1 -> d= pi * r
            for (size_t ix = 0; ix < orgXDimSize; ix++) {
                for (size_t iy = 0; iy < orgYDimSize; iy++) {
                    // find smallest distance (= max cosinus value): http://en.wikipedia.org/wiki/Great-circle_distance
                    size_t pos = ix+iy*orgXDimSize;
                    if (pos != samplePos) {
                        double lon1 = lonVals[pos];
                        double lat1 = latVals[pos];
                        if (!(isnan(lon1) || isnan(lat1))) {

                            double dlon = lon0 - lon1;

                            double cos_d = cos(lat0) * cos(lat1) * cos(dlon) + sin(lat0) * sin(lat1);
                            if (cos_d > min_cos_d) {
                                min_cos_d = cos_d;
                            }
                        }
                    }
                }
            }
#ifdef HAVE_OPENMP
            omp_set_lock (&my_lock);
#endif
            samples.push_back(min_cos_d);
#ifdef HAVE_OPENMP
            omp_unset_lock (&my_lock);
#endif
        }
    }
#ifdef HAVE_OPENMP
    omp_destroy_lock(&my_lock);
    }
#endif
    double max_grid_d = acos(*(max_element(samples.begin(), samples.end())));
    max_grid_d *= 1.414; // allow a bit larger extrapolation (diagonal = sqrt(2))
    if (max_grid_d > PI) max_grid_d = PI;
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
    double max_grid_d = getGridDistance(pointsOnXAxis, pointsOnYAxis, &lonVals[0], &latVals[0], orgXDimSize, orgYDimSize);
    double min_grid_cos_d = cos(max_grid_d);

    // 1. order lat/lon after latitude
    // 2. search for closest latitude
    // 3. go up and down in list as long as dlat < min_dist
    std::vector<LL_POINT> latlons;
    latlons.reserve(orgXDimSize*orgYDimSize);
    for (size_t ix = 0; ix < orgXDimSize; ix++) {
        for (size_t iy = 0; iy < orgYDimSize; iy++) {
            size_t pos = ix+iy*orgXDimSize;
            if (!(isnan(lonVals[pos]) || isnan(latVals[pos]))) {
                latlons.push_back(LL_POINT(latVals[pos],lonVals[pos], ix, iy));
            }
        }
    }
    // sort latlons by latitudes
    sort(latlons.begin(), latlons.end());
#ifdef HAVE_OPENMP
#pragma omp parallel default(shared)
    {
#pragma omp for
#endif
    for (int i = 0; i < pointsOnXAxis.size(); i++) { // using int instead of size_t because of openMP < 3.0
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
#ifdef HAVE_OPENMP
    }
#endif
}

/**
 * convert latVals and lonVals to matrices
 * @return lonVals in size (lonSize*latSize)
 */
static void lonLatVals2Matrix(boost::shared_array<double>& lonVals, boost::shared_array<double>& latVals, size_t lonSize, size_t latSize)
{
    boost::shared_array<double> matrixLatVals(new double[lonSize * latSize]);
    boost::shared_array<double> matrixLonVals(new double[lonSize * latSize]);
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

void CDMInterpolator::changeProjectionByForwardInterpolation(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit)
{
    CoordSysPtr cs = findBestCoordinateSystemAndProjectionVars(false);

    changeCDM(*cdm_.get(), proj_input, cs, projectionVariables,
              out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit,
              getLongitudeName(), getLatitudeName());

    string latitude = cs->findAxisOfType(CoordinateAxis::Lat)->getName();
    string longitude = cs->findAxisOfType(CoordinateAxis::Lon)->getName();
    boost::shared_array<double> latVals = dataReader->getScaledData(latitude)->asConstDouble();
    size_t latSize = dataReader->getData(latitude)->size();
    boost::shared_array<double> lonVals = dataReader->getScaledData(longitude)->asConstDouble();
    for_each(&latVals[0], &latVals[latSize], degreeToRad);
    for_each(&lonVals[0], &lonVals[latSize], degreeToRad);

    size_t orgXDimSize;
    size_t orgYDimSize;
    bool latLonProj = (cs->hasProjection() && (cs->getProjection()->getName() == "latitude_longitude"));
    if (latLonProj) {
        orgXDimSize = dataReader->getCDM().getDimension(cs->getGeoXAxis()->getName()).getLength();
        orgYDimSize = dataReader->getCDM().getDimension(cs->getGeoYAxis()->getName()).getLength();
        // create new latVals and lonVals as a matrix
        lonLatVals2Matrix(lonVals, latVals, orgXDimSize, orgYDimSize);
        latSize = orgXDimSize * orgYDimSize;
    } else {
        if (cs->getGeoYAxis()->getName() == latitude) {
            // x and y axis not properly defined, guessing
            vector<string> latShape = dataReader->getCDM().getVariable(latitude).getShape();
            if (latShape.size() != 2) {
                throw CDMException("latitude needs 2 dims for forward interpolation");
            }
            orgXDimSize = dataReader->getCDM().getDimension(latShape[0]).getLength();
            orgYDimSize = dataReader->getCDM().getDimension(latShape[1]).getLength();
            LOG4FIMEX(logger, Logger::DEBUG, "x and y axis: " << latShape[0] << "(" << orgXDimSize << "), " << latShape[1] << "(" << orgYDimSize << ")");
        } else {
            orgXDimSize = dataReader->getCDM().getDimension(cs->getGeoXAxis()->getName()).getLength();
            orgYDimSize = dataReader->getCDM().getDimension(cs->getGeoYAxis()->getName()).getLength();
        }
    }


    // store projection changes to be used in data-section
    // translate temporary new axes from deg2rad if required
    int miupXAxis = MIFI_PROJ_AXIS;
    int miupYAxis = MIFI_PROJ_AXIS;
    vector<double> outXAxis = out_x_axis;
    vector<double> outYAxis = out_y_axis;
    boost::regex degree(".*degree.*");
    if (boost::regex_match(out_x_axis_unit, degree)) {
        for_each(outXAxis.begin(), outXAxis.end(), degreeToRad);
        miupXAxis = MIFI_LONGITUDE;
    }
    if (boost::regex_match(out_y_axis_unit, degree)) {
        for_each(outYAxis.begin(), outYAxis.end(), degreeToRad);
        miupYAxis = MIFI_LATITUDE;
    }

    // translate all input points to output-coordinates, stored in lonVals and latVals
    std::string orgProjStr = "+ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong";
    if (MIFI_OK != mifi_project_values(orgProjStr.c_str(), proj_input.c_str(), &lonVals[0], &latVals[0], latSize)) {
        throw CDMException("unable to project axes from "+proj_input+ " to " +orgProjStr);
    }

    // translate the converted input-coordinates (lonvals and latvals) to cell-positions in output
    mifi_points2position(&lonVals[0], latSize, &outXAxis[0], outXAxis.size(), miupXAxis);
    mifi_points2position(&latVals[0], latSize, &outYAxis[0], outYAxis.size(), miupYAxis);

    //convert lonVals and latVals to vector
    vector<double> pointsOnXAxis, pointsOnYAxis;
    pointsOnXAxis.reserve(latSize);
    pointsOnYAxis.reserve(latSize);
    copy(&lonVals[0], &lonVals[0]+latSize, back_inserter(pointsOnXAxis));
    copy(&latVals[0], &latVals[0]+latSize, back_inserter(pointsOnYAxis));

    // store the interpolation
    LOG4FIMEX(logger, Logger::DEBUG, "creating cached forward interpolation matrix " << orgXDimSize << "x" << orgYDimSize << " => " << out_x_axis.size() << "x" << out_y_axis.size());
    cachedInterpolation = boost::shared_ptr<CachedInterpolationInterface>(new CachedForwardInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXDimSize, orgYDimSize, out_x_axis.size(), out_y_axis.size()));

    if (hasSpatialVectors()) {
        LOG4FIMEX(logger, Logger::WARN, "vector data found, but not possible? to interpolate with forward-interpolation");
    }

}

void CDMInterpolator::changeProjectionByCoordinates(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit)
{
    CoordSysPtr cs = findBestCoordinateSystemAndProjectionVars(false);

    changeCDM(*cdm_.get(), proj_input, cs, projectionVariables,
              out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit,
              getLongitudeName(), getLatitudeName());

    string latitude = cs->findAxisOfType(CoordinateAxis::Lat)->getName();
    string longitude = cs->findAxisOfType(CoordinateAxis::Lon)->getName();
    boost::shared_array<double> latVals = dataReader->getScaledData(latitude)->asConstDouble();
    size_t latSize = dataReader->getData(latitude)->size();
    boost::shared_array<double> lonVals = dataReader->getScaledData(longitude)->asConstDouble();
    for_each(&latVals[0], &latVals[latSize], degreeToRad);
    for_each(&lonVals[0], &lonVals[latSize], degreeToRad);

    size_t orgXDimSize;
    size_t orgYDimSize;
    bool latLonProj = (cs->hasProjection() && (cs->getProjection()->getName() == "latitude_longitude"));
    if (latLonProj) {
        orgXDimSize = dataReader->getCDM().getDimension(cs->getGeoXAxis()->getName()).getLength();
        orgYDimSize = dataReader->getCDM().getDimension(cs->getGeoYAxis()->getName()).getLength();
        // create new latVals and lonVals as a matrix
        lonLatVals2Matrix(lonVals, latVals, orgXDimSize, orgYDimSize);
        latSize = orgXDimSize * orgYDimSize;
    } else {
        if (cs->getGeoYAxis()->getName() == latitude) {
            // x and y axis not properly defined, guessing
            vector<string> latShape = dataReader->getCDM().getVariable(latitude).getShape();
            if (latShape.size() != 2) {
                throw CDMException("latitude needs 2 dims for forward interpolation");
            }
            orgXDimSize = dataReader->getCDM().getDimension(latShape[0]).getLength();
            orgYDimSize = dataReader->getCDM().getDimension(latShape[1]).getLength();
            LOG4FIMEX(logger, Logger::DEBUG, "x and y axis: " << latShape[0] << "(" << orgXDimSize << "), " << latShape[1] << "(" << orgYDimSize << ")");
        } else {
            orgXDimSize = dataReader->getCDM().getDimension(cs->getGeoXAxis()->getName()).getLength();
            orgYDimSize = dataReader->getCDM().getDimension(cs->getGeoYAxis()->getName()).getLength();
        }
    }

    // store projection changes to be used in data-section
    // translate temporary new axes from deg2rad if required
    vector<double> outXAxis = out_x_axis;
    vector<double> outYAxis = out_y_axis;
    boost::regex degree(".*degree.*");
    if (boost::regex_match(out_x_axis_unit, degree)) {
        for_each(outXAxis.begin(), outXAxis.end(), degreeToRad);
    }
    if (boost::regex_match(out_y_axis_unit, degree)) {
        for_each(outYAxis.begin(), outYAxis.end(), degreeToRad);
    }
    // get output axes expressed in latitude, longitude
    size_t fieldSize = outXAxis.size() * outYAxis.size();
    vector<double> pointsOnXAxis(fieldSize);
    vector<double> pointsOnYAxis(fieldSize);
    if (method == MIFI_INTERPOL_COORD_NN) {
        if (getProjectionName(proj_input) != "latlong") {
            std::string orgProjStr = "+ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong";
            if (MIFI_OK != mifi_project_axes(proj_input.c_str(), orgProjStr.c_str(), &outXAxis[0], &outYAxis[0], outXAxis.size(), outYAxis.size(), &pointsOnXAxis[0], &pointsOnYAxis[0])) {
                throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input.c_str());
            }
        }
        fastTranslatePointsToClosestInputCell(pointsOnXAxis, pointsOnYAxis, &lonVals[0], &latVals[0], orgXDimSize, orgYDimSize);
    } else if (method == MIFI_INTERPOL_COORD_NN_KD) {
        for (size_t ix = 0; ix < outXAxis.size(); ix++) {
            for (size_t iy = 0; iy < outYAxis.size(); iy++) {
                size_t pos = ix+iy*outXAxis.size();
                pointsOnXAxis[pos] = outXAxis[ix];
                pointsOnYAxis[pos] = outYAxis[iy];
            }
        }
        if (getProjectionName(proj_input) != "latlong") {
            std::string orgProjStr = "+ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0 +proj=latlong";
            if (MIFI_OK != mifi_project_values(orgProjStr.c_str(), proj_input.c_str(), &lonVals[0], &latVals[0], latSize)) {
                throw CDMException("unable to project axes from "+proj_input+ " to " +orgProjStr);
            }
        } else {
            throw CDMException("unable to use kd-tree interpolation when output-projection in degree");
        }
        if (DEBUG) {
            boost::shared_array<double> oLatVals = dataReader->getData(latitude)->asDouble();
            boost::shared_array<double> oLonVals = dataReader->getData(longitude)->asDouble();
            for (size_t i = 0; i < latSize; i++) {
                if ((!isnan(latVals[i])) && (!isnan(lonVals[i]))) {
                    cerr << i << ": (" << oLatVals[i] << "," <<oLonVals[i] << ") -> (" << latVals[i] << "," << lonVals[i] << ")" << endl;
                }
            }
        }
        kdTreeTranslatePointsToClosestInputCell(pointsOnXAxis, pointsOnYAxis, &lonVals[0], &latVals[0], orgXDimSize, orgYDimSize);
    } else {
        throw CDMException("unkown interpolation method for coordinates: " + type2string(method));
    }

    LOG4FIMEX(logger, Logger::DEBUG, "creating cached coordinate interpolation matrix " << orgXDimSize << "x" << orgYDimSize << " => " << out_x_axis.size() << "x" << out_y_axis.size());
    cachedInterpolation = boost::shared_ptr<CachedInterpolationInterface>(new CachedInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXDimSize, orgYDimSize, out_x_axis.size(), out_y_axis.size()));

    if (hasSpatialVectors()) {
        LOG4FIMEX(logger, Logger::WARN, "vector data found, but not possible? to interpolate with coordinate-interpolation");
    }

}

void CDMInterpolator::changeProjectionByProjectionParameters(int method, const string& proj_input, const vector<double>& out_x_axis, const vector<double>& out_y_axis, const string& out_x_axis_unit, const string& out_y_axis_unit)
{
    CoordSysPtr cs = findBestCoordinateSystemAndProjectionVars(true);

    changeCDM(*cdm_.get(), proj_input, cs, projectionVariables,
              out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit,
              getLongitudeName(), getLatitudeName());

    // translate axes to 'm' if given in other metric units
    std::string orgUnit = cs->getProjection()->isDegree() ? "degree" : "m";
    boost::shared_ptr<Data> orgXAxisVals = dataReader->getScaledDataInUnit(cs->getGeoXAxis()->getName(), orgUnit);
    boost::shared_ptr<Data >orgYAxisVals = dataReader->getScaledDataInUnit(cs->getGeoYAxis()->getName(), orgUnit);

    // store projection changes to be used in data-section
    // translate temporary new axes from deg2rad if required
    vector<double> outXAxis = out_x_axis;
    vector<double> outYAxis = out_y_axis;
    boost::regex degree(".*degree.*");
    if (boost::regex_match(out_x_axis_unit, degree)) {
        for_each(outXAxis.begin(), outXAxis.end(), degreeToRad);
    }
    if (boost::regex_match(out_y_axis_unit, degree)) {
        for_each(outYAxis.begin(), outYAxis.end(), degreeToRad);
    }

    // calculate the mapping from the new projection points to the original axes pointsOnXAxis(x_new, y_new), pointsOnYAxis(x_new, y_new)
    size_t fieldSize = outXAxis.size() * outYAxis.size();
    vector<double> pointsOnXAxis(fieldSize);
    vector<double> pointsOnYAxis(fieldSize);
    std::string orgProjStr = cs->getProjection()->getProj4String();
    if (MIFI_OK != mifi_project_axes(proj_input.c_str(), orgProjStr.c_str(), &outXAxis[0], &outYAxis[0], outXAxis.size(), outYAxis.size(), &pointsOnXAxis[0], &pointsOnYAxis[0])) {
        throw CDMException("unable to project axes from "+orgProjStr+ " to " +proj_input.c_str());
    }
    LOG4FIMEX(logger, Logger::DEBUG, "mifi_project_axes: "<< proj_input << "," << orgProjStr << "," << outXAxis[0] << "," << outYAxis[0] << " => " << pointsOnXAxis[0] << "," << pointsOnYAxis[0]);

    // translate original axes from deg2rad if required
    int miupXAxis = MIFI_PROJ_AXIS;
    int miupYAxis = MIFI_PROJ_AXIS;
    boost::shared_array<double> orgXAxisValsArray = orgXAxisVals->asDouble();
    boost::shared_array<double> orgYAxisValsArray = orgYAxisVals->asDouble();
    if (cs->getProjection()->isDegree()) {
        miupXAxis = MIFI_LONGITUDE;
        for_each(&orgXAxisValsArray[0], &orgXAxisValsArray[orgXAxisVals->size()], degreeToRad);
        miupYAxis = MIFI_LATITUDE;
        for_each(&orgYAxisValsArray[0], &orgYAxisValsArray[orgYAxisVals->size()], degreeToRad);
    }
    // translate coordinates (in rad or m) to indices
    mifi_points2position(&pointsOnXAxis[0], fieldSize, orgXAxisValsArray.get(), orgXAxisVals->size(), miupXAxis);
    mifi_points2position(&pointsOnYAxis[0], fieldSize, orgYAxisValsArray.get(), orgYAxisVals->size(), miupYAxis);

    LOG4FIMEX(logger, Logger::DEBUG, "creating cached projection interpolation matrix " << orgXAxisVals->size() << "x" << orgYAxisVals->size() << " => " << out_x_axis.size() << "x" << out_y_axis.size());
    cachedInterpolation = boost::shared_ptr<CachedInterpolationInterface>(new CachedInterpolation(method, pointsOnXAxis, pointsOnYAxis, orgXAxisVals->size(), orgYAxisVals->size(), out_x_axis.size(), out_y_axis.size()));

    if (hasSpatialVectors()) {
        // prepare interpolation of vectors
        LOG4FIMEX(logger, Logger::DEBUG, "creating cached vector projection interpolation matrix " << orgXAxisVals->size() << "x" << orgYAxisVals->size() << " => " << out_x_axis.size() << "x" << out_y_axis.size());
        boost::shared_array<double> matrix(new double[out_x_axis.size() * out_y_axis.size() * 4]);
        mifi_get_vector_reproject_matrix(orgProjStr.c_str(), proj_input.c_str(), &out_x_axis[0], &out_y_axis[0], miupXAxis, miupYAxis, out_x_axis.size(), out_y_axis.size(), matrix.get());
        cachedVectorReprojection = boost::shared_ptr<CachedVectorReprojection>(new CachedVectorReprojection(MIFI_VECTOR_KEEP_SIZE, matrix, out_x_axis.size(), out_y_axis.size()));
    }
}

bool CDMInterpolator::hasSpatialVectors() const
{
    const CDM::VarVec& variables = getCDM().getVariables();
    for (CDM::VarVec::const_iterator varIt = variables.begin(); varIt != variables.end(); ++varIt) {
        if (std::find(projectionVariables.begin(), projectionVariables.end(), varIt->getName()) != projectionVariables.end()) {
            if (varIt->isSpatialVector()) {
                return true;
            }
        }
    }
    return false;
}

}
