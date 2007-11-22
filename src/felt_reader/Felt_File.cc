#include "felt_reader/Felt_File.h"
#include "milib.h"
#include <ctime>
#include <boost/scoped_array.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

#include <iostream>

namespace MetNoFelt {

Felt_File::Felt_File(const string& filename)
	: filename(filename)
{
	int pos = filename.rfind("/");
	std::string dianaSetup = filename.substr(0, pos+1) + "diana.setup";
	boost::filesystem::path path(dianaSetup);
	if (boost::filesystem::exists(path)) {
		feltParameters = FeltParameters(dianaSetup);
	} // else default constructor
	
	const int MAXNIN = 256;
	int foundall = 0;
	int iunit, ireq, iexist, nin;
	short inmr[16];
	short idrec1[1024];
	float dummy;
	int nfound, iend, ierror, ioerr;
	
	// open in C to get a free file-descriptor
	fh = fopen(filename.c_str(), "r");
	if (fh == NULL) {
		return;
	}
	iunit = fileno(fh);
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
			//TODO: error-handling???
        } else {
 	       	foundall += nfound;
 	       	boost::array<short, 16> idx;
    	    for (int i = 0; i < nfound; i++) {
				//TODO: fill feltArrayMap
				for (int j = 0; j < 16; j++) {
					idx[j] = in[i*16 + j];
				}
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

Felt_File::~Felt_File()
{
	if (fh != NULL) {
		fclose(fh);
	}
}

Felt_Array& Felt_File::findOrCreateFeltArray(const boost::array<short, 16>& idx) {
	string name = feltParameters.getParameterName(idx);
	map<string, Felt_Array>::iterator it = feltArrayMap.find(name); 
	if (it == feltArrayMap.end()) {
		Felt_Array fa(name, idx);
		feltArrayMap[name] = fa;
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
	if (fh == NULL) {
		throw Felt_File_Error("file already closed");
	}
	int iunit = fileno(fh);
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
	int nx(fa.getX());
	int ny(fa.getY());
	vector<short> extraGridInfo(fieldSize-20-nx*ny);
	// copy extra data to extraGridInfo
	vector<short>::iterator egi_iter(extraGridInfo.begin());
	int i;
	for (i = 20+nx*ny; i < fieldSize; i++) {
		*egi_iter++ = header_data[i];
	}
	fa.setExtraInformation(extraGridInfo);
	vector<short> data(nx*ny);
	vector<short>::iterator d_iter(data.begin());
	for (i = 20; i < 20+nx*ny; i++) {
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


} // end namespace MetNoFelt
