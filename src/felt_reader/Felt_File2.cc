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

#include "fimex/Felt_File2.h"
#include "felt/FeltFile.h"
#include "felt/FeltField.h"
#include <milib/milib.h>
#include "fimex/CDMDataType.h"
#include "fimex/DataImpl.h"
#include "fimex/CDMconstants.h"
#include "fimex/Utils.h"
#include "fimex/interpolation.h"
#include <cstring>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <cerrno>
#include <cassert>
#include <ctime>
#include <cmath>
#include <set>
#include <iostream>
#include <fstream>
#include <boost/scoped_array.hpp>
#include <algorithm>
#include <iostream>
#include <boost/date_time/posix_time/posix_time.hpp>

namespace MetNoFelt {

using namespace MetNoFimex;

Felt_File2::Felt_File2(const string& filename) throw(Felt_File_Error)
	: filename_(filename)
{
	int pos = filename.rfind("/");
	std::string dianaSetup = filename.substr(0, pos+1) + "diana.setup";
	std::ifstream setupFile(dianaSetup.c_str());
	if (setupFile.is_open()) {
		setupFile.close();
		feltParameters = FeltParameters(dianaSetup);
	}
	// else default constructor

	// read the data
	std::map<std::string, std::string> options;
	init(options);
}

Felt_File2::Felt_File2(const std::string& filename, const std::vector<std::string>& dianaParamList, const std::map<std::string, std::string>& options) throw(Felt_File_Error)
: filename_(filename), feltParameters(dianaParamList)
{
	init(options);
}

void Felt_File2::setOptions(const std::map<std::string, std::string>& options) {
	// set gridParameterDelta from string ' ' splitted string of max 6 double values
	std::set<std::string> knownOptions;

	gridParameterDelta = std::vector<double>(6,0);
	std::string optName = "gridParameterDelta";
	std::map<std::string, std::string>::const_iterator gridParOpt = options.find("gridParameterDelta");
	if (gridParOpt != options.end()) {
		std::vector<std::string> tokens = tokenize(gridParOpt->second);
		int end = tokens.size() < gridParameterDelta.size() ? tokens.size() : gridParameterDelta.size();
		for (int i = 0; i < end; ++i) {
			gridParameterDelta[i] = string2type<double>(tokens[i]);
		}
		LOG4FIMEX(logger, Logger::DEBUG, "adding " << optName << " processing-option: " << gridParOpt->second);
		knownOptions.insert(optName);
	}

	// test for unknown options
	for (std::map<std::string, std::string>::const_iterator oit = options.begin(); oit != options.end(); ++oit) {
		if (knownOptions.find(oit->first) == knownOptions.end()) {
			LOG4FIMEX(logger, Logger::WARN, "unknown processing options: " << oit->first);
		}
	}
}

void fdPtrClose2(int* fdPtr) {
	// close(*fdPtr) and release fortran data
	// dummy variables needed for mrfelt, so not used there for close
	short is;
	int i;
	float f;
	mrfelt(3,"", *fdPtr, &is, 0, 1, &f, 1.f, 1024, &is, &i);
	delete fdPtr;
}

void Felt_File2::init(const std::map<std::string, std::string>& options) throw(Felt_File_Error)
{
	logger = getLogger("fimex.Felt_File2");
	setOptions(options);
	feltFile_ = boost::shared_ptr<felt::FeltFile>(new felt::FeltFile(boost::filesystem::path(filename_)));
	for (felt::FeltFile::const_iterator ffit = feltFile_->begin(); ffit != feltFile_->end(); ++ffit) {
	    felt::FeltFile::FeltFieldPtr field = *ffit;
	    const felt::FeltField::Header& header = field->getHeader();
	    std::string name = feltParameters.getParameterName(header);
	    if (name != UNDEFINED()) {
	        findOrCreateFeltArray(field);
	        if (field->verticalCoordinate() == 10) {
	            // hybrid levels, read all level-parameters once for each level-pairs
	            Felt_Array2::LevelPair lp = make_pair(field->level1(), field->level2());
	            if (hybridLevels_.find(lp) == hybridLevels_.end()) {
	                hybridLevels_[lp] = field->miscField();
	            }
	        }
	    }
	}
}

Felt_File2::~Felt_File2()
{
}

// true = find, false = create
bool Felt_File2::findOrCreateFeltArray(boost::shared_ptr<felt::FeltField> field) {
	string name = feltParameters.getParameterName(field->getHeader());
	string dataType = feltParameters.getParameterDatatype(name);
	map<string, boost::shared_ptr<Felt_Array2> >::iterator it = feltArrayMap_.find(name);
	if (it == feltArrayMap_.end()) {
		//cerr << "new FeltArray " << name << ": " << dataType << " " << feltParameters.getParameterFillValue(name) << endl;
		boost::shared_ptr<Felt_Array2> fa(new Felt_Array2(name, field, dataType, feltParameters.getParameterFillValue(name)));
		feltArrayMap_[name] = fa;   // copy to map
		return false; // reference from map
	} else {
	    it->second->addInformationByField(field);
		return true;
	}
}

const boost::shared_ptr<Felt_Array2> Felt_File2::getFeltArray(const string& arrayName) const throw(Felt_File_Error){
	map<string, boost::shared_ptr<Felt_Array2> >::const_iterator it = feltArrayMap_.find(arrayName);
	if (it == feltArrayMap_.end()) {
		throw Felt_File_Error("unknown parameter: " + arrayName);
	}
	return it->second;
}

std::vector<boost::shared_ptr<Felt_Array2> > Felt_File2::listFeltArrays() const {
	vector<boost::shared_ptr<Felt_Array2> > li;
	for (map<string, boost::shared_ptr<Felt_Array2> >::const_iterator it = feltArrayMap_.begin(); it != feltArrayMap_.end(); ++it) {
		li.push_back(it->second);
	}
	return li;
}

template<typename T>
class Scale : public unary_function<short, T> {
public:
	Scale(double newFillValue, double scalingFactor) : newFill(static_cast<T>(newFillValue)), scalingFactor(scalingFactor) {}
	T operator()(short val) {
		return (val == ANY_VALUE() ? newFill : static_cast<T>(val * scalingFactor));
	}
private:
	const T newFill;
	const double scalingFactor;
};

// convert felt short to a scaled Data
template<typename T>
boost::shared_ptr<MetNoFimex::Data> createScaledData(const vector<short>& indata, double newFillValue, double scalingFactor) {
	boost::shared_array<T> data(new T[indata.size()]);
	transform(indata.begin(), indata.end(), &data[0], Scale<T>(newFillValue, scalingFactor));
	return boost::shared_ptr<MetNoFimex::Data>(new DataImpl<T>(data, indata.size()));
}

boost::shared_ptr<MetNoFimex::Data> Felt_File2::getScaledDataSlice(boost::shared_ptr<Felt_Array2> feltArray, const boost::posix_time::ptime time, const Felt_Array2::LevelPair level) throw(Felt_File_Error)
{
    size_t dataSize = feltArray->getX() * feltArray->getY();
    vector<short> data;
    data.reserve(dataSize);
    const boost::shared_ptr<felt::FeltField> ff = feltArray->getField(time, level);
	ff->grid(data);
	boost::shared_ptr<MetNoFimex::Data> returnData;
	if (feltArray->getDatatype() == "short") {
		if (ff->scaleFactor() != feltArray->scaleFactor()) {
			throw Felt_File_Error("change in scaling factor for parameter: " + feltArray->getName() + " consider using float or double datatpye");
		}
		returnData = createScaledData<short>(data, feltArray->getFillValue(), 1.);
	} else if (feltArray->getDatatype() == "float") {
		returnData = createScaledData<float>(data, feltArray->getFillValue(), std::pow(10,static_cast<double>(ff->scaleFactor())));
	} else if (feltArray->getDatatype() == "double") {
		returnData = createScaledData<double>(data, feltArray->getFillValue(), std::pow(10,static_cast<double>(ff->scaleFactor())));
	} else {
		throw Felt_File_Error("unknown datatype for feltArray " + feltArray->getName() + ": " + feltArray->getDatatype());
	}
	return returnData;
}

std::map<short, std::vector<Felt_Array2::LevelPair> > Felt_File2::getFeltLevelPairs() const {
	// put level values of each id into the levelSet (sort and unique)
	std::map<short, set<Felt_Array2::LevelPair, Felt_Array2::LevelPairLess> > typeLevelSet;
	for (std::map<std::string, boost::shared_ptr<Felt_Array2> >::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
		vector<Felt_Array2::LevelPair> levels = fait->second->getLevelPairs();
		typeLevelSet[fait->second->getLevelType()].insert(levels.begin(), levels.end());
	}
	// convert the set into a vector
	std::map<short, std::vector<Felt_Array2::LevelPair> > typeLevelVector;
	for (std::map<short, set<Felt_Array2::LevelPair, Felt_Array2::LevelPairLess> >::iterator it = typeLevelSet.begin(); it != typeLevelSet.end(); ++it) {
		typeLevelVector[it->first] = std::vector<Felt_Array2::LevelPair>(it->second.begin(), it->second.end());
	}
	return typeLevelVector;
}

std::vector<boost::posix_time::ptime> Felt_File2::getFeltTimes() const {
	std::set<boost::posix_time::ptime> times;
	for (std::map<std::string, boost::shared_ptr<Felt_Array2> >::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
		vector<boost::posix_time::ptime> fa_times = fait->second->getTimes();
		times.insert(fa_times.begin(), fa_times.end());
	}	// times automatically unique and sorted due to set
	std::vector<boost::posix_time::ptime> sortedTimes(times.begin(), times.end());
	return sortedTimes;
}

int Felt_File2::getNX() const {
	int nx = 0;
	for (std::map<std::string, boost::shared_ptr<Felt_Array2> >::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
		nx = std::max(fait->second->getX(), nx);
	}
	return nx;
}

int Felt_File2::getNY() const {
	int ny = 0;
	for (std::map<std::string, boost::shared_ptr<Felt_Array2> >::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
		ny = std::max(fait->second->getY(), ny);
	}
	return ny;
}

boost::shared_ptr<Data> Felt_File2::getXData() const throw(Felt_File_Error) {
	boost::shared_ptr<Data> xData = createData(CDM_FLOAT, getNX());
	boost::shared_ptr<felt::FeltGridDefinition> gridDef = getGridDefinition();
	for (int i = 0; i < getNX(); i++) {
		float value = gridDef->startX() + i * gridDef->getXIncrement();
		xData->setValue(i, value);
	}
	return xData;
}

boost::shared_ptr<Data> Felt_File2::getYData() const throw(Felt_File_Error)
{
	boost::shared_ptr<Data> yData = createData(CDM_FLOAT, getNY());
    boost::shared_ptr<felt::FeltGridDefinition> gridDef = getGridDefinition();
    for (int i = 0; i < getNY(); i++) {
        float value = gridDef->startY() + i * gridDef->getYIncrement();
        yData->setValue(i, value);
    }
    return yData;
}

int Felt_File2::getGridType() const throw(Felt_File_Error)
{
    // TODO: check against delta, check changes
    if (feltArrayMap_.size() > 0) {
        return feltArrayMap_.begin()->second->getGridType();
    }
    return -1; // default
}

boost::shared_ptr<felt::FeltGridDefinition> Felt_File2::getGridDefinition() const throw(Felt_File_Error) {
	std::map<std::string, boost::shared_ptr<Felt_Array2> >::const_iterator fait = feltArrayMap_.begin();
	// TODO: check against delta, check changes
	if (feltArrayMap_.size() > 0) {
	    return fait->second->getGridDefinition();
	}

#if 0
	if (feltArrayMap_.size() > 0) {
		const boost::array<float, 6>& params = fait->second.getGridParameters();
		for (++fait; fait != feltArrayMap_.end(); ++fait) {
			const boost::array<float, 6>& newParams = fait->second.getGridParameters();
			for (int i = 0; i < 6; i++) {
				// allow params to differ by a delta (optional)
				if (newParams[i] != params[i] && std::fabs(newParams[i]-params[i]) > gridParameterDelta[i]) {
					throw(Felt_File_Error("cannot change gridParameters within a file for " + fait->second.getName() + " param " + type2string(i) + ": " + type2string(params[i]) + " != " + type2string(newParams[i]) + "("+type2string(newParams[i]-params[i])+")"));
				}
			}
		}
		return params;
	}
#endif
	else {
		throw(Felt_File_Error("cannot read gridParameters: no Felt_Array2 available"));
	}
}


} // end namespace MetNoFelt
