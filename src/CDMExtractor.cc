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

namespace MetNoFimex
{

CDMExtractor::CDMExtractor(boost::shared_ptr<CDMReader> datareader)
: dataReader(datareader)
{
	cdm = dataReader->getCDM();
}

CDMExtractor::~CDMExtractor()
{
 }

const boost::shared_ptr<Data> CDMExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	const CDMVariable& variable = cdm.getVariable(varName);
	if (variable.hasData()) {
		return getDataSliceFromMemory(variable, unLimDimPos);
	}
	boost::shared_ptr<Data> data;
	if (dimChanges.empty()) {
		// simple read
		data = dataReader->getDataSlice(varName, unLimDimPos);
	} else {
		// translate slice-variable size where dimensions have been transformed, (via data.slice)
		bool hasChangedDim = false;
		const std::vector<std::string>& dims = variable.getShape();
		std::vector<size_t> orgDimSize, newDimSize, newDimStart;
		const CDM& orgCDM = dataReader->getCDM();
		for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
			const CDMDimension& dim = orgCDM.getDimension(*it);
			if (! dim.isUnlimited()) { // this is the slice-dim
				orgDimSize.push_back(dim.getLength());
				DimChangeMap::iterator foundDim = dimChanges.find(dim.getName());
				if (foundDim == dimChanges.end()) {
					newDimStart.push_back(0);
					newDimSize.push_back(dim.getLength());
				} else {
					hasChangedDim = true;
					newDimStart.push_back((foundDim->second)[0]);
					newDimSize.push_back((foundDim->second)[1]);
				}
			} else {
				// just changing unLimDimPos
				DimChangeMap::iterator foundDim = dimChanges.find(dim.getName());
				if (foundDim != dimChanges.end()) {
					unLimDimPos += (foundDim->second)[0];
				}
			}
		}
		// read
		data = dataReader->getDataSlice(varName, unLimDimPos);
		if (hasChangedDim && (data->size() > 0)) { // datasize might be 0, i.e. if time doesn't exist
			data = data->slice(orgDimSize, newDimStart, newDimSize);
		}
	}
	// TODO: translate datatype where required
	return data;
}

void CDMExtractor::removeVariable(std::string variable) throw(CDMException)
{
	cdm.removeVariable(variable);
}

void CDMExtractor::reduceDimension(std::string dimName, size_t start, size_t length) throw(CDMException)
{
	CDMDimension& dim = cdm.getDimension(dimName);
	if (start+length >= dim.getLength()) {
		throw CDMException("can't enlarge dimension " + dimName + ": start+length ("+type2string(start)+"+"+type2string(length)+") out of bounds");
	}
	// keep track of changes
	dim.setLength(length);
	boost::array<size_t, 2> changes = { {start, length} };
	dimChanges[dimName] = changes;


	// removing all data containing this dimension, just to be sure it's read from the dataReader
	const CDM::VarVec& variables = cdm.getVariables();
	for (CDM::VarVec::const_iterator it = variables.begin(); it != variables.end(); ++it) {
		const std::vector<std::string>& shape = it->getShape();
		if (std::find(shape.begin(), shape.end(), dim.getName()) != shape.end()) {
			cdm.getVariable(it->getName()).setData(boost::shared_ptr<Data>());
		}
	}
}

void CDMExtractor::reduceDimensionStartEnd(std::string dimName, size_t start, long end) throw(CDMException)
{
	size_t length = 0;
	if (end > 0) {
		length = end - start;
	} else {
		CDMDimension& dim = cdm.getDimension(dimName);
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
