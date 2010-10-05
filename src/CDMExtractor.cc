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

boost::shared_ptr<Data> CDMExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
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

void CDMExtractor::removeVariable(std::string variable) throw(CDMException)
{
	cdm_->removeVariable(variable);
}

void CDMExtractor::reduceDimension(std::string dimName, size_t start, size_t length) throw(CDMException)
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

void CDMExtractor::reduceTime(const FimexTime& startTime, const FimexTime& endTime) throw(CDMException)
{
    if (startTime > endTime) {
        throw CDMException("reduceTime requires startTime <= endTime");
    }
    using namespace std;
    const CDM& cdm = dataReader->getCDM();
    typedef vector<boost::shared_ptr<const CoordinateSystem> > CsList;
    CsList coordsys = listCoordinateSystems(cdm);
    typedef vector<CoordinateSystem::ConstAxisPtr> TAxesList;
    TAxesList tAxes;
    for (CsList::const_iterator cs = coordsys.begin(); cs != coordsys.end(); ++cs) {
        CoordinateSystem::ConstAxisPtr tAxis = (*cs)->getTimeAxis();
        if (tAxis.get() != 0) {
            tAxes.push_back(tAxis);
        }
    }

    set<string> usedDimensions;
    for (TAxesList::const_iterator ta = tAxes.begin(); ta != tAxes.end(); ++ta) {
        const vector<string>& shape = (*ta)->getShape();
        if (shape.size() != 1) {
            LOG4FIMEX(logger, Logger::WARN, "cannot reduce time-axis '" << (*ta)->getName() << "': axis is not 1-dim");
        } else if (usedDimensions.find(shape[0]) == usedDimensions.end()) {
            // set usedDimensions to not process dimension again
            usedDimensions.insert(shape[0]);

            // find start and end time in time-axis
            string unit = cdm.getUnits((*ta)->getName());
            if (unit.size() == 0) {
                throw CDMException("time axis "+ (*ta)->getName() + " without unit-string");
            }
            // calculate everything in the original unit
            TimeUnit tu(unit);
            double start = tu.fimexTime2unitTime(startTime);
            double end = tu.fimexTime2unitTime(endTime);
            boost::shared_ptr<Data> timeData = dataReader->getScaledData((*ta)->getName());
            const boost::shared_array<double> timeArray = timeData->asConstDouble();

            // timeArray assumed to be monotonic growing
            double* lower = lower_bound(&timeArray[0], &timeArray[0] + timeData->size(), start);
            double* upper = upper_bound(&timeArray[0], &timeArray[0] + timeData->size(), end);

            // reduce dimension according to these points (name, startPos, size)
            size_t startPos = distance(&timeArray[0], lower);
            size_t size = distance(lower, upper);
            LOG4FIMEX(logger, Logger::DEBUG, "reducing dimension "<< shape[0] << " from: " << startPos << " size: " << size);
            reduceDimension(shape[0], startPos, size);
        }
    }
}

void CDMExtractor::reduceDimensionStartEnd(std::string dimName, size_t start, long end) throw(CDMException)
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


void CDMExtractor::changeDataType(std::string variable, CDMDataType datatype) throw(CDMException)
{
	// TODO
	throw CDMException("not implemented yet");
}

} // end of namespace
