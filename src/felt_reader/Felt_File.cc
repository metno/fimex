#include "felt_reader/Felt_File.h"
#include "milib.h"
#include "CDMDataType.h"
#include "Utils.h"
#include <cassert>
#include <ctime>
#include <boost/scoped_array.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

#include <algorithm>
#include <iostream>

namespace MetNoFelt {

using namespace MetNoUtplukk;

Felt_File::Felt_File(const string& filename) throw(Felt_File_Error)
	: filename(filename)
{
	int pos = filename.rfind("/");
	std::string dianaSetup = filename.substr(0, pos+1) + "diana.setup";
	boost::filesystem::path path(dianaSetup);
	if (boost::filesystem::exists(path)) {
		feltParameters = FeltParameters(dianaSetup);
	} // else default constructor
	
	// read the data
	init();
}

Felt_File::Felt_File(const std::string& filename, const std::vector<std::string>& dianaParamList) throw(Felt_File_Error)
: filename(filename), feltParameters(dianaParamList)
{
	init();
}

void Felt_File::init() throw(Felt_File_Error)
{
	const int MAXNIN = 256;
	int foundall = 0;
	int iunit, ireq, iexist, nin;
	short inmr[16];
	short idrec1[1024];
	float dummy;
	int nfound, iend, ierror, ioerr;
	std::FILE* fileh;
	
	// open in C to get a free file-descriptor
	fileh = fopen(filename.c_str(), "r");
	if (fileh == NULL) {
		return;
	}
	fh = boost::shared_ptr<std::FILE>(fileh, fclose);
	iunit = fileno(fileh);
	// initialize feltfile
	mrfelt(1,filename.c_str(),iunit,&inmr[0],0,1,&dummy,1.f,1024,&idrec1[0],&ierror);
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
        qfelt(iunit,ireq,iexist,nin,in.get(),ifound.get(),&nfound,&iend,&ierror,&ioerr);
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
	map<string, Felt_Array>::iterator it = feltArrayMap.find(name); 
	if (it == feltArrayMap.end()) {
		feltArrayMap[name] = Felt_Array(name, idx);
		return feltArrayMap[name];
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
std::vector<short> Felt_File::getDataSlice(Felt_Array& fa, boost::array<short, 16>& idx, int fieldSize) throw(Felt_File_Error) {
	boost::scoped_array<short> header_data(new short[fieldSize]); // contains header (20 fields) and data (nx*ny) and something extra???
	if (fh == 0) {
		throw Felt_File_Error("file already closed");
	}
	int iunit = fileno(fh.get());
	float dummy;
	std::string lastFile("*");
	int ierror(0);
	// read feltfile-data
	//mrfelt(mode,          filnam,iunit,in         ,ipack,lfield,field ,fscale,     ldata,            idata, ierror)
	mrfelt(     2,lastFile.c_str(),iunit,idx.begin(),    0,     0,&dummy,   1.f, fieldSize,header_data.get(),&ierror);
	if (ierror > 0) {
		throw Felt_File_Error("error reading with mrfelt");
	}
	boost::array<short, 20> header;
	for (int i = 0; i < 20; i++) {
		header[i] = header_data[i];
	}
	fa.setDataHeader(header);
	// get gridParameters via libmi
	boost::array<float, 6> gridParameters;
	int gridType, nx, ny;
//	gridpar(int icall, int ldata,      short *idata, int *igtype, int *nx, int *ny,              float *grid, int *ierror);
	gridpar(1,         fieldSize, header_data.get(),   &gridType,     &nx,     &ny, gridParameters.c_array(),     &ierror);
	if (ierror > 0) {
		throw Felt_File_Error("error interpreting grid Parameters");
	}
	fa.setGridType(gridType);
	fa.setGridParameters(gridParameters);
	vector<short> data(nx*ny);
	vector<short>::iterator d_iter(data.begin());
	for (int i = 20; i < 20+nx*ny; i++) {
		*d_iter++ = header_data[i];
	}
	return data;
}

vector<short> Felt_File::getDataSlice(const std::string& compName, const std::time_t time, const short level) throw(Felt_File_Error) {
	Felt_Array& fa = getFeltArray(compName);
	boost::array<short, 16> idx(fa.getIndex(time, level));
	int fieldSize(fa.getFieldSize(time, level));
	return getDataSlice(fa, idx, fieldSize);
}

std::map<short, std::vector<short> > Felt_File::getFeltLevels() const {
	std::map<short, std::set<short> > typeLevelSet;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		vector<short> levels = fait->second.getLevels();
		typeLevelSet[fait->second.getLevelType()].insert(levels.begin(), levels.end());
	}
	std::map<short, std::vector<short> > typeLevelVector;
	for (std::map<short, std::set<short> >::iterator it = typeLevelSet.begin(); it != typeLevelSet.end(); ++it) {
		std::vector<short> sortedLevels(it->second.begin(), it->second.end());
		sort(sortedLevels.begin(), sortedLevels.end());
		typeLevelVector[it->first] = sortedLevels;
	}
	return typeLevelVector;
}

std::vector<time_t> Felt_File::getFeltTimes() const {
	std::set<time_t> times;
	for (std::map<std::string, Felt_Array>::const_iterator fait = feltArrayMap.begin(); fait != feltArrayMap.end(); ++fait) {
		vector<time_t> fa_times = fait->second.getTimes();
		times.insert(fa_times.begin(), fa_times.end());
	}
	std::vector<time_t> sortedTimes(times.begin(), times.end());
	sort(sortedTimes.begin(), sortedTimes.end());
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
	float d, lon0, x0, scale;
	switch (getGridType()) {
		case 1:
		case 4: // polarstereographic
			d = (150.*79.)/params[2]; // (150.*79.) are met.no constant for 150km grid with 79 cells between equator and northpole?
			lon0 = 0;
			x0 = params[0];
			scale = 1000;
			break;
		case 2:
		case 3: // geographic or spherical rotated grid
		case 5: // mercator
			d = params[2];
			lon0 = params[0];
			x0 = 1;
			scale = 1;
			break;
		default: throw Felt_File_Error("unknown gridType: " + type2string(getGridType()));
	}
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
			d = (150.*79.)/params[2]; // (150.*79.) are met.no constant for 150km grid with 79 cells between equator and northpole?
			lat0 = 0;
			y0 = params[1];
			scale = 1000; // (km -> m)
			break;
		case 2:
		case 3: // geographic or spherical rotated grid
		case 5: // mercator
			d = params[2];
			lat0 = params[1];
			y0 = 1;
			scale = 1;
			break;
		default: throw Felt_File_Error("unknown gridType: " + type2string(getGridType()));
	}
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
				if (newParams[i] != params[i]) {
					throw(Felt_File_Error("cannot change gridParameters within a file"));
				}
			}
		}
		return params;
	} else {
		throw(Felt_File_Error("cannot read gridParameters: no Felt_Array available"));
	}
}


} // end namespace MetNoFelt
