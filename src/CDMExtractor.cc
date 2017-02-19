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
#include "fimex/CDMReaderUtils.h"
#include "fimex/Logger.h"
#include "CDMMergeUtils.h"
#include <vector>
#include <set>
#include <algorithm>
#include <functional>
#include <numeric>
#include <iterator>

namespace MetNoFimex
{

static LoggerPtr logger(getLogger("fimex.CDMExtractor"));

CDMExtractor::CDMExtractor(boost::shared_ptr<CDMReader> datareader)
: dataReader_(datareader)
{
    *cdm_ = dataReader_->getCDM();
}

CDMExtractor::~CDMExtractor()
{
}

// join SliceBuilder request and build a big dataPtr
// the slices need to come in a logical order of the data
DataPtr joinSlices(boost::shared_ptr<CDMReader> reader, std::string varName, const std::vector<SliceBuilder>& slices)
{
    const CDMVariable& var = reader->getCDM().getVariable(varName);
    // handle trivial cases
    if (slices.size() == 0) return createData(var.getDataType(), 0, 0.);
    if (slices.size() == 1) return reader->getDataSlice(varName, slices.at(0));

    size_t totalSize = 0;
    for (size_t i = 0; i < slices.size(); i++) {
        // estimate total size
        const SliceBuilder& sb = slices.at(i);
        std::vector<std::size_t> dimSizes = sb.getDimensionSizes();
        std::size_t sliceSize = std::accumulate(dimSizes.begin(), dimSizes.end(), 1ul, std::multiplies<size_t>());
        totalSize += sliceSize;
    }

    DataPtr retData = createData(var.getDataType(), totalSize, reader->getCDM().getFillValue(varName));
    size_t dataPos = 0;
    for (size_t i = 0; i < slices.size(); i++) {
        // add the data
        const SliceBuilder& sb = slices.at(i);
        std::vector<std::size_t> dimSizes = sb.getDimensionSizes();
        std::size_t sliceSize = std::accumulate(dimSizes.begin(), dimSizes.end(), 1ul, std::multiplies<size_t>());
        DataPtr sliceData = reader->getDataSlice(varName, sb);
        if (sliceData->size() == 0) {
            dataPos += sliceSize;
        } else {
            assert(sliceData->size() == sliceSize);
            retData->setValues(dataPos, *sliceData);
            dataPos += sliceSize;
        }
    }
    if (dataPos != retData->size()) throw CDMException("joining slices failed: " + type2string(retData->size()) + " != " + type2string(dataPos) );
    return retData;
}

DataPtr CDMExtractor::getDataSlice_(const std::string& varName,
        const SliceBuilder& sb)
{
    std::vector<SliceBuilder> slices;
    // translate slice-variable size where dimensions have been transformed, (via data.slice)
    const CDM& orgCDM = dataReader_->getCDM();
    slices.push_back(SliceBuilder(orgCDM, varName));

    const std::vector<std::string>& dims = slices.at(0).getDimensionNames();
    // loop over variables dimensions and see which to reduce
    // revert the dimensions to match joining, unlimit is last in slicebuilder, but slowest changing
    for (std::vector<std::string>::const_iterator it = dims.begin();
            it != dims.end(); ++it) {
        const CDMDimension& dim = orgCDM.getDimension(*it);
        DimSlicesMap::iterator foundDim = dimSlices_.find(dim.getName());
        size_t sbStart, sbSize;
        sb.getStartAndSize(*it, sbStart, sbSize);
        if (foundDim == dimSlices_.end()) {
            // handle pure sb changes
            for (std::vector<SliceBuilder>::iterator sliceIt = slices.begin();
                    sliceIt != slices.end(); ++sliceIt) {
                sliceIt->setStartAndSize(dim.getName(), sbStart, sbSize);
            }
        } else {
            // handle slices in chunks
            const std::vector<std::size_t>& positions = foundDim->second;
            assert(positions.size() > sbStart);
            assert(positions.size() - sbStart >= sbSize);
            if (slices.size() <= 1) {
                // make chunks as large as possible for efficiency
                std::vector < std::pair<size_t, size_t> > chunks; // start,size
                size_t start = positions.at(sbStart);
                size_t last = positions.at(sbStart);
                for (size_t i = sbStart + 1; i < sbStart + sbSize; ++i) {
                    if (positions.at(i) == last + 1) {
                        // make a larger continuous chunk
                        last = positions.at(i);
                    } else {
                        chunks.push_back(
                                std::make_pair(start, last - start + 1));
                        start = positions.at(i);
                        last = positions.at(i);
                    }
                }
                chunks.push_back(std::make_pair(start, last - start + 1)); // chunks with start and size
                // create one slice for each chunk
                std::vector<SliceBuilder> newSlices;
                for (std::vector<std::pair<size_t, size_t> >::iterator chunksIt =
                        chunks.begin(); chunksIt != chunks.end(); ++chunksIt) {
                    for (std::vector<SliceBuilder>::iterator sliceIt =
                            slices.begin(); sliceIt != slices.end();
                            ++sliceIt) {
                        SliceBuilder lsb = *sliceIt;
                        lsb.setStartAndSize(dim.getName(), chunksIt->first,
                                chunksIt->second);
                        newSlices.push_back(lsb);
                    }
                }
                slices = newSlices;
            } else {
                // chunks already splitted up, just add the relevant slices one by one
                std::vector<SliceBuilder> newSlices;
                for (size_t i = sbStart; i < sbStart + sbSize; ++i) {
                    for (std::vector<SliceBuilder>::iterator sliceIt =
                            slices.begin(); sliceIt != slices.end();
                            ++sliceIt) {
                        SliceBuilder lsb = *sliceIt;
                        lsb.setStartAndSize(dim.getName(), positions.at(i), 1);
                        newSlices.push_back(lsb);
                    }
                }
                slices = newSlices;
            }
        }
    }
    // read
    DataPtr data = joinSlices(dataReader_, varName, slices);
    return data;
}


DataPtr CDMExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        // remove dimension makes sure that variables with dimensions requiring slicing
        // don't have in local in memory data, so return the memory data is save here
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    DataPtr data;
    if (dimSlices_.empty()) {
        // simple read
        data = dataReader_->getDataSlice(varName, unLimDimPos);
    } else {
        // translate unlimdim to SliceBuilder, fetch slices and join
        SliceBuilder sb(getCDM(), varName);
        const std::vector<std::string>& dims = sb.getDimensionNames();
        for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
            const CDMDimension& dim = cdm_->getDimension(*it);
            if (dim.isUnlimited()) {
                sb.setStartAndSize(*it, unLimDimPos, 1);
            }
        }
        data = getDataSlice_(varName, sb);
     }
    // TODO: translate datatype where required
    return data;
}

DataPtr CDMExtractor::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        LOG4FIMEX(logger, Logger::DEBUG, "fetching data from memory");
        DataPtr data = variable.getData();
        if (data->size() == 0) {
            return data;
        } else {
            return variable.getData()->slice(sb.getMaxDimensionSizes(),
                    sb.getDimensionStartPositions(),
                    sb.getDimensionSizes());
        }
    }

    if (dimSlices_.empty()) {
        // no further slicing of dimensions
        return dataReader_->getDataSlice(varName, sb);
    }
    return getDataSlice_(varName, sb);
}


void CDMExtractor::removeVariable(std::string variable)
{
    LOG4FIMEX(logger, Logger::DEBUG, "removing variable "<< variable);
    cdm_->removeVariable(variable);
}

void CDMExtractor::selectVariables(std::set<std::string> variables, bool addAuxiliaryVariables)
{
    using namespace std;
    if (addAuxiliaryVariables) {
        addAuxiliary(variables, getCDM(), listCoordinateSystems(this->dataReader_));
    }


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

void CDMExtractor::reduceDimension(std::string dimName, const std::set<std::size_t>& slices)
{
    CDMDimension& dim = cdm_->getDimension(dimName);
    std::set<std::size_t> useSlices;
    for (std::set<std::size_t>::const_iterator sliceIt = slices.begin(); sliceIt != slices.end(); ++sliceIt ) {
        if (*sliceIt > dim.getLength()) {
//            LOG4FIMEX(logger,Logger.WARN, "can't select slice of dimension '" << dimName + "': "<<*sliceIt << " out of bounds: "<< dim.getLength());
            throw CDMException("can't select slice of dimension '" + dimName + "': " + type2string(*sliceIt) + " out of bounds: "+ type2string(dim.getLength()));
        } else {
            useSlices.insert(*sliceIt);
        }
    }
    // keep track of changes
    dim.setLength(useSlices.size());
    dimSlices_[dimName] = std::vector<size_t>(useSlices.begin(), useSlices.end());
    LOG4FIMEX(logger,Logger::DEBUG, "reducing dimension '" << dimName << "' to: " << join(useSlices.begin(), useSlices.end(), ",") );

    // removing all data containing this dimension, just to be sure it's read from the dataReader_
    const CDM::VarVec& variables = cdm_->getVariables();
    for (CDM::VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
        const std::vector<std::string>& shape = it->getShape();
        if (std::find(shape.begin(), shape.end(), dim.getName()) != shape.end()) {
            cdm_->getVariable(it->getName()).setData(DataPtr());
        }
    }

}

void CDMExtractor::reduceDimension(std::string dimName, size_t start, size_t length)
{
    std::set<std::size_t> useSlices;
    for (std::size_t i = 0; i < length; i++) {
        useSlices.insert(start+i);
    }
    reduceDimension(dimName, useSlices);
}

void CDMExtractor::reduceDimensionStartEnd(std::string dimName, size_t start, long long end)
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
    typedef vector<boost::shared_ptr<const CoordinateSystem> > CsList;
    CsList coordsys = listCoordinateSystems(dataReader_);
    const CDM& cdm = dataReader_->getCDM();
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
            DataPtr vData = dataReader_->getScaledData((*va)->getName());
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
                double startValX = startVal*slope + offset - roundingDelta;
                double endValX = endVal*slope + offset + roundingDelta;
                LOG4FIMEX(logger, Logger::DEBUG, "reduceAxes of " << (*va)->getName() << " after unit-conversion (slope,offset="<<slope<<","<<offset<<"): ("<< startValX << ","<< endValX<<")");

                // find start and end time in time-axis
                // make sure data is growing
                bool isReverse = false;
                if ((vData->size() > 1) && (vArray[0] > vArray[1])) {
                    isReverse = true;
                    reverse(&vArray[0], &vArray[0] + vData->size());
                }

                // vArray assumed to be monotonic growing
                double* lower = lower_bound(&vArray[0], &vArray[0] + vData->size(), startValX); // val included
                double* upper = upper_bound(&vArray[0], &vArray[0] + vData->size(), endValX);   // val excluded

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
                if (dimSlices_.find(shape[0]) == dimSlices_.end()) {
                    LOG4FIMEX(logger, Logger::DEBUG, "reducing axes-dimension "<< shape[0] << " from: " << startPos << " size: " << size);
                    reduceDimension(shape[0], startPos, size);
                } else {
                    LOG4FIMEX(logger, Logger::DEBUG, "not reducing axes-dimension "<< shape[0] << ": already done earlier");
                }

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

    if (south < -90. || south > 90) throw CDMException("reduceLatLonBoundingBox south outside domain: " + type2string(south));
    if (north < -90. || north > 90) throw CDMException("reduceLatLonBoundingBox north outside domain: " + type2string(north));
    if (west < -180. || west > 180) throw CDMException("reduceLatLonBoundingBox west outside domain: " + type2string(west));
    if (east < -180. || east > 180) throw CDMException("reduceLatLonBoundingBox east outside domain: " + type2string(east));

    if (west > east) {
        west -= 360;
    }
    // find coordinate-systems
    typedef vector<boost::shared_ptr<const CoordinateSystem> > CsList;
    CsList coordsys = listCoordinateSystems(dataReader_);
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
