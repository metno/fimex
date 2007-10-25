#include "felt_reader/Felt_File.h"
#include "milib.h"
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

#include <iostream>

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
	short *inmr = new short[16];
	int *ifound;
	short *idrec1 = new short[1024];
	float *field = new float[1];
	int nfound, iend, ierror, ioerr;
	
	// open in C to get a free file-descriptor
	fh = fopen(filename.c_str(), "r");
	if (fh == NULL) {
		return;
	}
	iunit = fileno(fh);
	// initialize feltfile
	mrfelt(1,filename.c_str(),iunit,inmr,0,1,field,1.f,1024,idrec1,&ierror);
	for (int i = 0; i < 16; i++) {
		//TODO: something with idrec1, inmr ???
	}

	// reading of data-identifier blocks (table of contents)
	ireq = 1;
	iexist = 1;
	iend = 0;
	nin = MAXNIN;
	short *in = new short[nin*16];
	ifound = new int[nin];
	while ((iend == 0) && (ierror == 0)) {
		// init field = undef
    	for (int i = 0; i < 16; i++) {
    		in[i] = -32767; // first row = undef
    	}
        qfelt(iunit,ireq,iexist,nin,in,ifound,&nfound,&iend,&ierror,&ioerr);
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
				fa.addInformationByIndex(idx);
        	}
        }
    }
    
    delete [] in;
	delete [] idrec1;
	delete [] ifound;
    
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
		boost::array<short, 16> clearIdx = feltParameters.getParameters(name); // clear of variable params
		Felt_Array fa(name, clearIdx);
		feltArrayMap[name] = fa;
		return feltArrayMap[name];
	} else {
		return it->second;
	}
}

Felt_Array& Felt_File::getFeltArray(const string& arrayName) {
	map<string, Felt_Array>::iterator it = feltArrayMap.find(arrayName); 
	if (it == feltArrayMap.end()) {
		throw Felt_File_Error("unknown parameter: " + arrayName);
	}
	return it->second;
}

vector<Felt_Array> Felt_File::listFeltArrays() {
	vector<Felt_Array> li;
	for (map<string, Felt_Array>::iterator it = feltArrayMap.begin(); it != feltArrayMap.end(); ++it) {
		li.push_back(it->second);
	}	
	return li;
}
