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

#include "fimex/CDMExtractor.h"
#include "fimex/Data.h"
#include "fimex/CDM.h"
#include "fimex/SliceBuilder.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"
#include <vector>
#include <set>
#include <algorithm>
#include <iterator>

namespace MetNoFimex
{

LoggerPtr logger(getLogger("fimex.CDMExtractor"));

CDMExtractor::CDMExtractor(boost::shared_ptr<CDMReader> datareader)
: dataReader(datareader)
{
	*cdm_ = dataReader->getCDM();
}

CDMExtractor::~CDMExtractor()
{
}

boost::shared_ptr<Data> CDMExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
	const CDMVariable& variable = cdm_->getVariable(varName);
	if (variable.hasData()) {
	    // remove dimension makes sure that variables with dimensions requiring slicing
	    // don't have in local in memory data, so return the memory data is save here
		return getDataSliceFromMemory(variable, unLimDimPos);
	}
	boost::shared_ptr<Data> data;
	if (dimChanges.empty()) {
		// simple read
		data = dataReader->getDataSlice(varName, unLimDimPos);
	} else {
		// translate slice-variable size where dimensions have been transformed, (via data.slice)
		const CDM& orgCDM = dataReader->getCDM();
		SliceBuilder sb(orgCDM, varName);
        const std::vector<std::string>& dims = sb.getDimensionNames();
		// loop over variables dimensions and see which to reduce
		for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
			const CDMDimension& dim = orgCDM.getDimension(*it);
			if (dim.isUnlimited()) {
			    sb.setStartAndSize(dim.getName(), unLimDimPos, 1);
			}
			DimChangeMap::iterator foundDim = dimChanges.find(dim.getName());
			if (foundDim != dimChanges.end()) {
                   size_t start = (foundDim->second)[0];
                   size_t length = (foundDim->second)[1];
	               if (dim.isUnlimited()) { // this is the slice-dim
	                   // changing unLimDimPos to readers dimension
	                   // and fetch only one slice
                       sb.setStartAndSize(dim.getName(), start + unLimDimPos, 1);
	               } else {
	                   sb.setStartAndSize(dim.getName(), start, length);
	               }
			}
		}
		// read
	    data = dataReader->getDataSlice(varName, sb);
	 }
	// TODO: translate datatype where required
	return data;
}

void CDMExtractor::removeVariable(std::string variable)
{
    LOG4FIMEX(logger, Logger::DEBUG, "removing variable "<< variable);
	cdm_->removeVariable(variable);
}

void CDMExtractor::selectVariables(std::set<std::string> variables)
{
    using namespace std;
    const CDM::VarVec& allVars = getCDM().getVariables();

    set<string> allVarNames;
    transform(allVars.begin(),
              allVars.end(),
              inserter(allVarNames, allVarNames.begin()),
              mem_fun_ref(&CDMVariable::getName));

    // find the variables in one list, but not in the other
    set<string> difference;
    set_difference(allVarNames.begin(),
                   allVarNames.end(),
                   variables.begin(),
                   variables.end(),
                   inserter(difference, difference.begin()));

    // remove all unnecessary variables
    for_each(difference.begin(),
             difference.end(),
             bind1st(mem_fun(&CDMExtractor::removeVariable),this));
}

void CDMExtractor::reduceDimension(std::string dimName, size_t start, size_t length)
{
	CDMDimension& dim = cdm_->getDimension(dimName);
	if (start+length > dim.getLength()) {
		throw CDMException("can't enlarge dimension " + dimName + ": start+length ("+type2string(start)+"+"+type2string(length)+") out of bounds: "+ type2string(dim.getLength()));
	}
	// keep track of changes
	dim.setLength(length);
	boost::array<size_t, 2> changes = { {start, length} };
	dimChanges[dimName] = changes;


	// removing all data containing this dimension, just to be sure it's read from the dataReader
	const CDM::VarVec& variables = cdm_->getVariables();
	for (CDM::VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
		const std::vector<std::string>& shape = it->getShape();
		if (std::find(shape.begin(), shape.end(), dim.getName()) != shape.end()) {
			cdm_->getVariable(it->getName()).setData(boost::shared_ptr<Data>());
		}
	}
}

void CDMExtractor::reduceDimensionStartEnd(std::string dimName, size_t start, long end)
{
	size_t length = 0;
	if (end > 0) {
		length = end - start + 1;
	} else {
		CDMDimension& dim = cdm_->getDimension(dimName);
		length = dim.getLength();
		length -= start;
		length += end;
	}
	reduceDimension(dimName, start, length);
}

void CDMExtractor::reduceAxes(const std::vector<CoordinateAxis::AxisType>& types, const std::string& aUnits, double startVal, double endVal)
{
    using namespace std;
    LOG4FIMEX(logger, Logger::DEBUG, "reduceAxes of "<< aUnits << "(" << startVal << "," << endVal <<")");
    if (startVal > endVal) {
        // make sure startVal <= endVal
        swap(startVal, endVal);
    }

    Units units;
    const CDM& cdm = getCDM();
    typedef vector<boost::shared_ptr<const CoordinateSystem> > CsList;
    CsList coordsys = listCoordinateSystems(cdm);
    typedef vector<CoordinateSystem::ConstAxisPtr> VAxesList;
    VAxesList vAxes;
    for (CsList::const_iterator cs = coordsys.begin(); cs != coordsys.end(); ++cs) {
        for (vector<CoordinateAxis::AxisType>::const_iterator vType = types.begin(); vType != types.end(); ++vType) {
            CoordinateSystem::ConstAxisPtr vAxis = (*cs)->findAxisOfType(*vType);
            if (vAxis.get() != 0) {
                string vaUnits = cdm.getUnits(vAxis->getName());
                if (units.areConvertible(vaUnits, aUnits)) {
                    vAxes.push_back(vAxis);
                }
            }
        }
    }

    set<string> usedDimensions;
    for (VAxesList::const_iterator va = vAxes.begin(); va != vAxes.end(); ++va) {
        const vector<string>& shape = (*va)->getShape();
        if (shape.size() != 1) {
            LOG4FIMEX(logger, Logger::WARN, "cannot reduce axis '" << (*va)->getName() << "': axis is not 1-dim");
        } else if (usedDimensions.find(shape[0]) == usedDimensions.end()) {
            // set usedDimensions to not process dimension again
            usedDimensions.insert(shape[0]);
            boost::shared_ptr<Data> vData = dataReader->getScaledData((*va)->getName());
            if (vData->size() > 0) {
                boost::shared_array<double> vArray = vData->asDouble();
                // calculate everything in the original unit
                string vaUnits = cdm.getUnits((*va)->getName());
                double offset,slope;
                units.convert(aUnits, vaUnits, slope, offset);
                double roundingDelta = 1e-5;
                if (vData->size() > 1 && vArray[0] != vArray[1]) {
                    // make a relative rounding delta
                    roundingDelta = .01 * fabs(vArray[0] - vArray[1]);
                }
                startVal = startVal*slope + offset - roundingDelta;
                endVal = endVal*slope + offset + roundingDelta;
                LOG4FIMEX(logger, Logger::DEBUG, "reduceAxes of " << (*va)->getName() << " after unit-conversion: ("<< startVal << ","<< endVal<<")");

                // find start and end time in time-axis
                // make sure data is growing
                bool isReverse = false;
                if ((vData->size() > 1) && (vArray[0] > vArray[1])) {
                    isReverse = true;
                    reverse(&vArray[0], &vArray[0] + vData->size());
                }

                // vArray assumed to be monotonic growing
                double* lower = lower_bound(&vArray[0], &vArray[0] + vData->size(), startVal); // val included
                double* upper = upper_bound(&vArray[0], &vArray[0] + vData->size(), endVal);   // val excluded

                if (upper == (&vArray[0] + vData->size())) {
                    LOG4FIMEX(logger, Logger::DEBUG, "reduceAxes found lower,upper ("<< *lower << ",end)");
                } else {
                    LOG4FIMEX(logger, Logger::DEBUG, "reduceAxes found lower,upper ("<< *lower << ","<< *upper <<")");
                }


                // reduce dimension according to these points (name, startPos, size)
                size_t startPos = distance(&vArray[0], lower);
                size_t size = distance(lower, upper);
                if ((size == 0) &&
                    ((cdm.getUnlimitedDim() == 0) || (cdm.getUnlimitedDim()->getName() != shape[0]))) {
                    // 0 size only allowed in unlim-dim, using best effort
                    LOG4FIMEX(logger, Logger::DEBUG, "reduceAxes lower == upper => size = 0: increasing size to 1");
                    size = 1;
                }
                if (isReverse) {
                    startPos = vData->size() - size - startPos;
                    LOG4FIMEX(logger, Logger::DEBUG, "reduceAxis on reverse data, new (start,size) = ("<<startPos<<","<<size<<")" );
                    // reverse data back for possible later usage
                    reverse(&vArray[0], &vArray[0] + vData->size());
                }

                LOG4FIMEX(logger, Logger::DEBUG, "reducing axes-dimension "<< shape[0] << " from: " << startPos << " size: " << size);
                reduceDimension(shape[0], startPos, size);

            }
        }
    }
}

void CDMExtractor::reduceTime(const FimexTime& startTime, const FimexTime& endTime)
{
    std::string unit = "seconds since 1970-01-01 00:00:00";
    TimeUnit tu(unit);
    std::vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::Time);
    reduceAxes(types, unit, tu.fimexTime2unitTime(startTime), tu.fimexTime2unitTime(endTime));
}

void CDMExtractor::reduceVerticalAxis(const std::string& units, double startVal, double endVal)
{
    std::vector<CoordinateAxis::AxisType> types;
    types.push_back(CoordinateAxis::GeoZ);
    types.push_back(CoordinateAxis::Height);
    types.push_back(CoordinateAxis::Pressure);
    reduceAxes(types, units, startVal, endVal);
}

void CDMExtractor::reduceLatLonBoundingBox(double south, double north, double west, double east)
{
    using namespace std;
    // check input
    if (south > north) throw CDMException("reduceLatLonBoundingBox south > north: "+type2string(south) + ">" +type2string(north));
    if (west > east) throw CDMException("reduceLatLonBoundingBox west > east: "+type2string(west) + ">" +type2string(east));
    if (south < -90. || south > 90) throw CDMException("reduceLatLonBoundingBox south outside domain: " + type2string(south));
    if (north < -90. || north > 90) throw CDMException("reduceLatLonBoundingBox north outside domain: " + type2string(north));
    if (west < -180. || west > 180) throw CDMException("reduceLatLonBoundingBox west outside domain: " + type2string(west));
    if (east < -180. || east > 180) throw CDMException("reduceLatLonBoundingBox east outside domain: " + type2string(east));

    // find coordinate-systems
    typedef vector<boost::shared_ptr<const CoordinateSystem> > CsList;
    CsList coordsys = listCoordinateSystems(getCDM());
    typedef vector<CoordinateSystem::ConstAxisPtr> VAxesList;
    VAxesList vAxes;
    set<string> convertedAxes;
    for (CsList::const_iterator cs = coordsys.begin(); cs != coordsys.end(); ++cs) {
        if ((*cs)->isSimpleSpatialGridded()) {
            string xAxisName = (*cs)->getGeoXAxis()->getName();
            string yAxisName = (*cs)->getGeoYAxis()->getName();
            // check for projection and if the axes have already been processed by another cs
            if ((*cs)->hasProjection() &&
                (convertedAxes.find(xAxisName) == convertedAxes.end()) &&
                (convertedAxes.find(yAxisName) == convertedAxes.end())) {
                // create a set of border-points to the projection to avoid singularities in the projected plane
                vector<double> xLonVals;
                vector<double> yLatVals;
                const int steps = 7;
                double northSouthStep = (north - south) / steps;
                double eastWestStep = (east - west) / steps;
                for (int i = 0; i < steps; ++i) {
                    // going along southern axis to east
                    xLonVals.push_back(west + i*eastWestStep);
                    yLatVals.push_back(south);
                    // going along eastern axis to north
                    xLonVals.push_back(east);
                    yLatVals.push_back(south + i*northSouthStep);
                    // going along northern axis to west
                    xLonVals.push_back(east - i*eastWestStep);
                    yLatVals.push_back(north);
                    // going along western axis to south
                    xLonVals.push_back(west);
                    yLatVals.push_back(north - i*northSouthStep);
                }
                // reproject the border-points (in place)
                boost::shared_ptr<const Projection> proj = (*cs)->getProjection();
                proj->convertFromLonLat(xLonVals, yLatVals);

                // find the minimum and maximum values in each projection-axis
                double xMin = *min_element(xLonVals.begin(), xLonVals.end());
                double xMax = *max_element(xLonVals.begin(), xLonVals.end());
                double yMin = *min_element(yLatVals.begin(), yLatVals.end());
                double yMax = *max_element(yLatVals.begin(), yLatVals.end());

                // run reduceAxes to the min/max values
                vector<CoordinateAxis::AxisType> xAxis;
                xAxis.push_back((*cs)->getGeoXAxis()->getAxisType());
                vector<CoordinateAxis::AxisType> yAxis;
                yAxis.push_back((*cs)->getGeoYAxis()->getAxisType());
                string unit = proj->isDegree() ? "degree" : "m";
                reduceAxes(xAxis, unit, xMin, xMax);
                reduceAxes(yAxis, unit, yMin, yMax);
                // avoid double conversion
                convertedAxes.insert(xAxisName);
                convertedAxes.insert(yAxisName);
            }
        }
    }

}


void CDMExtractor::changeDataType(std::string variable, CDMDataType datatype)
{
	// TODO
	throw CDMException("not implemented yet");
}

} // end of namespace
