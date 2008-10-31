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

#include "fimex/Felt_File.h"
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

namespace MetNoFelt {

using namespace MetNoFimex;

Felt_File::Felt_File(const string& filename) throw(Felt_File_Error)
	: filename(filename)
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

Felt_File::Felt_File(const std::string& filename, const std::vector<std::string>& dianaParamList, const std::map<std::string, std::string>& options) throw(Felt_File_Error)
: filename(filename), feltParameters(dianaParamList)
{
	init(options);
}

void Felt_File::setOptions(const std::map<std::string, std::string>& options) {
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

void fdPtrClose(int* fdPtr) {
	// close(*fdPtr) and release fortran data
	// dummy variables needed for mrfelt, so not used there for close
	short is;
	int i;
	float f;
	mrfelt(3,"", *fdPtr, &is, 0, 1, &f, 1.f, 1024, &is, &i);
	delete fdPtr;
}

void Felt_File::init(const std::map<std::string, std::string>& options) throw(Felt_File_Error)
{
	logger = getLogger("fimex.Felt_File");
	setOptions(options);
	const int MAXNIN = 256;
	int foundall = 0;
	int ireq, iexist, nin;
	short inmr[16];
	short idrec1[1024];
	float dummy;
	int nfound, iend, ierror, ioerr;

	// open in C to get a free file-descriptor
	int fd = open(filename.c_str(), O_RDONLY|O_LARGEFILE);
	if (fd == -1) {
		char* msg = strerror(errno);
		throw Felt_File_Error("error reading file " + filename + ": " + msg);
	}
	// initialize feltfile
	mrfelt(1,filename.c_str(),fd,&inmr[0],0,1,&dummy,1.f,1024,&idrec1[0],&ierror);
	if (ierror != 0) {
		throw Felt_File_Error("error reading file " + filename + ": error in mrfelt");
	}
	fdPtr = boost::shared_ptr<int>(new int(fd), fdPtrClose);
	for (int i = 0; i < 16; i++) {
		//TODO: something with idrec1, inmr ???
	}

	// reading of data-identifier blocks (table of contents)
	ireq = 1;
	iexist = 1;
	iend = 0;
	nin = MAXNIN;
	boost::scoped_array<short> in(new short[nin*16]);
	boost::scoped_array<int> ifound(new int[nin]);
	while ((iend == 0) && (ierror == 0)) {
		// init field = undef
    	for (int i = 0; i < 16; i++) {
    		in[i] = -32767; // first row = undef
    	}
        qfelt(*fdPtr,ireq,iexist,nin,in.get(),ifound.get(),&nfound,&iend,&ierror,&ioerr);
        if (ierror != 0) {
			throw Felt_File_Error("problems querying feltfile");
        } else {
 	       	foundall += nfound;
 	       	boost::array<short, 16> idx;
    	    for (int i = 0; i < nfound; i++) {
				for (int j = 0; j < 16; j++) {
					idx[j] = in[i*16 + j];
				}
				std::string name = feltParameters.getParameterName(idx);
				if (name != UNDEFINED()) {
					Felt_Array& fa = findOrCreateFeltArray(idx);
					fa.addInformationByIndex(idx, ifound[i]);
					if (idx[10] == 10) {
						// hybrid levels are defined in the data-section (ident19), need all levels once
						if (hybridLevels.find(index16toLevelPair(idx)) == hybridLevels.end()) {
							getDataSlice(fa, idx, ifound[i]); // initialize data-section
							hybridLevels[index16toLevelPair(idx)] = fa.getIdent19(index16toTime(idx), index16toLevelPair(idx));
//							pair<short, short> p = index16toLevelPair(idx);
//							std::cerr << "reading level for: " << p.first << " " << p.second << " "<< hybridLevels[p] <<std::endl;
						}
					}
					if (fa.getX() == ANY_VALUE()) {
						// make sure that all info is initialized even if it requires reading a bit more data
						// return data of no interest
						getDataSlice(fa, idx, ifound[i]);
					}
				}
        	}
        }
    }
}

Felt_File::~Felt_File()
{
}

Felt_Array& Felt_File::findOrCreateFeltArray(const boost::array<short, 16>& idx) {
	string name = feltParameters.getParameterName(idx);
	string dataType = feltParameters.getParameterDatatype(name);
	map<string, Felt_Array>::iterator it = feltArrayMap.find(name);
	if (it == feltArrayMap.end()) {
		//cerr << "new FeltArray " << name << ": " << dataType << " " << feltParameters.getParameterFillValue(name) << endl;
		Felt_Array fa(name, idx, dataType);
		fa.setFillValue(feltParameters.getParameterFillValue(name));
		feltArrayMap[name] = fa;   // copy to map
		return feltArrayMap[name]; // reference from map
	} else {
		return it->second;
	}
}

Felt_Array& Felt_File::getFeltArray(const string& arrayName) throw(Felt_File_Error){
	map<string, Felt_Array>::iterator it = feltArrayMap.find(arrayName);
	if (it == feltArrayMap.end()) {
		throw Felt_File_Error("unknown parameter: " + arrayName);
	}
	return it->second;
}

std::vector<Felt_Array> Felt_File::listFeltArrays() {
	vector<Felt_Array> li;
	for (map<string, Felt_Array>::iterator it = feltArrayMap.begin(); it != feltArrayMap.end(); ++it) {
		li.push_back(it->second);
	}
	return li;
}

boost::shared_array<short> Felt_File::getHeaderData(Felt_Array& fa, boost::array<short, 16>& idx, int fieldSize)  throw(Felt_File_Error) {
	boost::shared_array<short> header_data(new short[fieldSize]); // contains header (20 fields) and data (nx*ny) and something extra???
	if (fdPtr == 0) {
		throw Felt_File_Error("file already closed");
	}
	float dummy;
	std::string lastFile("*");
	int ierror(0);
	// read feltfile-data
	//mrfelt(mode,          filnam,iunit,in         ,ipack,lfield,field ,fscale,     ldata,            idata, ierror)
	mrfelt(     2,lastFile.c_str(),*fdPtr,idx.begin(),    0,     0,&dummy,   1.f, fieldSize,header_data.get(),&ierror);
	if (ierror > 0) {
		throw Felt_File_Error("error reading with mrfelt");
	}
	return header_data;
}
std::vector<short> Felt_File::getDataSlice(Felt_Array& fa, boost::array<short, 16>& idx, int fieldSize) throw(Felt_File_Error) {
	boost::shared_array<short> header_data = getHeaderData(fa, idx, fieldSize);
	boost::array<short, 20> header;
	copy(&header_data[0], &header_data[20], header.begin());
	fa.setDataHeader(header);
	// get gridParameters via libmi
	boost::array<float, 6> gridParameters;
	int gridType, nx, ny;
	int ierror(0);
//	gridpar(int icall, int ldata,      short *idata, int *igtype, int *nx, int *ny,              float *grid, int *ierror);
	gridpar(1,         fieldSize, header_data.get(),   &gridType,     &nx,     &ny, gridParameters.c_array(),     &ierror);
	if (ierror > 0) {
		throw Felt_File_Error("error interpreting grid Parameters");
	}
	fa.setGridType(gridType);
	fa.setGridParameters(gridParameters);
	fa.addIdent19(index16toTime(idx), index16toLevelPair(idx), header_data[19-1]); // -1 due to fortran/c conversion
	vector<short> data(nx*ny);
	copy(&header_data[20], &header_data[20+nx*ny], data.begin());

	LoggerPtr logger = getLogger("fimex.Felt_File");
	if (logger->isEnabledFor(Logger::DEBUG)) {
		std::ostringstream oss;
		oss << "header(2,3,4,8,19): " << header[2] << " " << header[3] << " " << header[4] << " " << header[8] << " " << header[19];
		if (header[8] > 1000) {
			int extra = header[8] % 1000;
			oss << "\textraHeader: ";
			for (int i = 0; i < extra; i++) {
				oss << header_data[20 + nx*ny + i] << " ";
			}
		}

		LOG4FIMEX(logger, Logger::DEBUG, oss.str());
	}

	return data;
}

vector<short> Felt_File::getDataSlice(const std::string& compName, const std::time_t time, const short level) throw(Felt_File_Error) {
	Felt_Array& fa = getFeltArray(compName);
	boost::array<short, 16> idx(fa.getIndex(time, level));
	int fieldSize(fa.getFieldSize(time, level));
	return getDataSlice(fa, idx, fieldSize);
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

// convert felt short 'header+data+gridinfo = header_data' to a scaled Data
template<typename T>
boost::shared_ptr<MetNoFimex::Data> createScaledData(const boost::shared_array<short>& header_data, size_t dataSize, double newFillValue, double scalingFactor) {
	boost::shared_array<T> data(new T[dataSize]);
	transform(&header_data[20], &header_data[20+dataSize], &data[0], Scale<T>(newFillValue, scalingFactor));
	return boost::shared_ptr<MetNoFimex::Data>(new DataImpl<T>(data, dataSize));
}

boost::shared_ptr<MetNoFimex::Data> Felt_File::getScaledDataSlice(const std::string& compName, const std::time_t time, const short level, double fillValue) throw(Felt_File_Error) {
	Felt_Array& fa = getFeltArray(compName);
	boost::array<short, 16> idx(fa.getIndex(time, level));
	int fieldSize(fa.getFieldSize(time, level));
	boost::shared_array<short> header_data = getHeaderData(fa, idx, fieldSize);
	double scalingFactor = std::pow(10,static_cast<double>(header_data[19]));
	size_t dataSize = fa.getX() * fa.getY();
	boost::shared_ptr<MetNoFimex::Data> returnData;
	if (fa.getDatatype() == "short") {
		if (scalingFactor != fa.getScalingFactor()) {
			throw Felt_File_Error("change in scaling factor for parameter: " + fa.getName() + " consider using float or double datatpye");
		}
		returnData = createScaledData<short>(header_data, dataSize, fa.getFillValue(), 1.);
	} else if (fa.getDatatype() == "float") {
		returnData = createScaledData<float>(header_data, dataSize, fa.getFillValue(), scalingFactor);
	} else if (fa.getDatatype() == "double") {
		returnData = createScaledData<double>(header_data, dataSize, fa.getFillValue(), scalingFactor);
	} else {
		throw Felt_File_Error("unknown datatype for feltArray " + fa.getName() + ": " + fa.getDatatype());
	}
	return returnData;
}
std::map<short, std::vector<short> > Felt_File::getFeltLevels() const {
	// put level values of each id into the levelSet (sort and unique)
	std::map<short, std::set<short> > typeLevelSet;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		vector<short> levels = fait->second.getLevels();
		typeLevelSet[fait->second.getLevelType()].insert(levels.begin(), levels.end());
	}
	// convert the set into a vector
	std::map<short, std::vector<short> > typeLevelVector;
	for (std::map<short, std::set<short> >::iterator it = typeLevelSet.begin(); it != typeLevelSet.end(); ++it) {
		typeLevelVector[it->first] = std::vector<short>(it->second.begin(), it->second.end());
	}
	return typeLevelVector;
}

std::map<short, std::vector<pair<short,short> > > Felt_File::getFeltLevelPairs() const {
	// put level values of each id into the levelSet (sort and unique)
	std::map<short, ShortPairSet> typeLevelSet;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		vector<pair<short, short> > levels = fait->second.getLevelPairs();
		typeLevelSet[fait->second.getLevelType()].insert(levels.begin(), levels.end());
	}
	// convert the set into a vector
	std::map<short, std::vector<pair<short,short> > > typeLevelVector;
	for (std::map<short, ShortPairSet >::iterator it = typeLevelSet.begin(); it != typeLevelSet.end(); ++it) {
		typeLevelVector[it->first] = std::vector<pair<short, short> >(it->second.begin(), it->second.end());
	}
	return typeLevelVector;
}

std::vector<time_t> Felt_File::getFeltTimes() const {
	std::set<time_t> times;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		vector<time_t> fa_times = fait->second.getTimes();
		times.insert(fa_times.begin(), fa_times.end());
	}	// times automatically sorted due to set
	std::vector<time_t> sortedTimes(times.begin(), times.end());
	return sortedTimes;
}

int Felt_File::getNX() const {
	int nx = 0;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		nx = std::max(fait->second.getX(), nx);
	}
	return nx;
}

int Felt_File::getNY() const {
	int ny = 0;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		ny = std::max(fait->second.getY(), ny);
	}
	return ny;
}

boost::shared_ptr<Data> Felt_File::getXData() const throw(Felt_File_Error) {
	boost::shared_ptr<Data> xData = createData(CDM_FLOAT, getNX());
	const boost::array<float, 6>& params = getGridParameters();
//	for (int i = 0; i < 6; i++) {
//		std::cerr << params[i] << " ";
//	}
//	std::cerr << std::endl;
	float d, lon0, x0, scale;
	switch (getGridType()) {
		case 1:
		case 4: // polarstereographic
			//R*(1+sin(phi) == 1/4 of earth circumference
			d = MIFI_EARTH_RADIUS_M/1000 * (1+std::sin(DEG_TO_RAD*params[4]))/params[2];
			lon0 = 0;
			x0 = params[0];
			scale = 1000;
			break;
		case 2: // geographic grid
			d = params[2];
			lon0 = params[0];
			x0 = 1;
			scale = 1;
			break;
		case 3: // geographic rotated grid
			d = params[2];
			lon0 = params[0];
			x0 = 1;
			scale = 1;
			break;
		case 5: // mercator
			d = params[2];
			lon0 = params[0];
			x0 = 1;
			scale = 1;
			break;
		default: throw Felt_File_Error("unknown gridType: " + type2string(getGridType()));
	}
	// coordinates are given in fortran type, i.e. first cell is 1, translation to first cell = 0
	for (int i = 1; i <= getNX(); i++) {
		float value = (lon0 + (i-x0)*d) * scale; // (km -> m)
		xData->setValue(i-1, value);
	}
	return xData;
}

boost::shared_ptr<Data> Felt_File::getYData() const throw(Felt_File_Error) {
	boost::shared_ptr<Data> yData = createData(CDM_FLOAT, getNY());
	const boost::array<float, 6>& params = getGridParameters();
	float d, lat0, y0, scale;
	switch (getGridType()) {
		case 1:
		case 4: // polarstereographic
			//R*(1+sin(phi) == 1/4 of earth circumference
			d = MIFI_EARTH_RADIUS_M/1000* (1+std::sin(DEG_TO_RAD*params[4]))/params[2];
			lat0 = 0;
			y0 = params[1];
			scale = 1000; // (km -> m)
			break;
		case 2: // geographical grid
			d = params[3];
			lat0 = params[1];
			y0 = 1;
			scale = 1;
			break;
		case 3: // spherical rotated grid
			d = params[3] ;
			lat0 = params[1];
			y0 = 1;
			scale = 1;
			break;
		case 5: // mercator
			d = params[3];
			lat0 = params[1];
			y0 = 1;
			scale = 1;
			break;
		default: throw Felt_File_Error("unknown gridType: " + type2string(getGridType()));
	}
	// coordinates are given in fortran type, i.e. first cell is 1, translation to first cell = 0
	for (int i = 1; i <= getNY(); i++) {
		float value = (lat0 + (i-y0)*d) * scale;
		yData->setValue(i-1, value);
	}
	return yData;
}



short Felt_File::getGridType() const throw(Felt_File_Error) {
	std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin();
	if (feltArrayMap.size() > 0) {
		short gridType = fait->second.getGridType();
		for (++fait; fait != feltArrayMap.end(); ++fait) {
			if (gridType != fait->second.getGridType()) {
				throw(Felt_File_Error("cannot change gridType within a file"));
			}
		}
		return gridType;
	} else {
		throw(Felt_File_Error("cannot read gridParameters: no Felt_Array available"));
	}
}

const boost::array<float, 6>& Felt_File::getGridParameters() const throw(Felt_File_Error) {
	std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin();
	if (feltArrayMap.size() > 0) {
		const boost::array<float, 6>& params = fait->second.getGridParameters();
		for (++fait; fait != feltArrayMap.end(); ++fait) {
			const boost::array<float, 6>& newParams = fait->second.getGridParameters();
			for (int i = 0; i < 6; i++) {
				// TODO: allow params to differ by a delta (optional)
				if (newParams[i] != params[i] && std::fabs(newParams[i]-params[i]) > gridParameterDelta[i]) {
					throw(Felt_File_Error("cannot change gridParameters within a file for " + fait->second.getName() + " param " + type2string(i) + ": " + type2string(params[i]) + " != " + type2string(newParams[i]) + "("+type2string(newParams[i]-params[i])+")"));
				}
			}
		}
		return params;
	} else {
		throw(Felt_File_Error("cannot read gridParameters: no Felt_Array available"));
	}
}


} // end namespace MetNoFelt
