#include <milib.h>
#include <iostream>
#include <cstdio>

using namespace std;

int
main(int argc, char* args[]) {
	const int MAXNIN = 256;
	int foundall = 0;
	int iunit, ireq, iexist, nin;
	short *inmr = new short[16];
	short *in;
	int *ifound;
	short *idrec1 = new short[1024];
	float *field = new float[1];
	int nfound, iend, ierror, ioerr;
	// open in C to get a free file-descriptor
	FILE* feltFile = fopen(args[1], "r");
	iunit = fileno(feltFile);
	
	// initialize feltfile
	mrfelt(1,args[1],iunit,inmr,0,1,field,1.f,1024,idrec1,&ierror);
	cerr << "initialized feltfile " << "iunit:" << iunit << endl;
	for (int i = 0; i < 10; i++) {
		cout << idrec1[i] << ", ";
	}
	cout << endl;
	
	ireq=1;
    iexist=1;
	iend = 0;
	nin = MAXNIN;
	in = new short[nin*16];
	ifound = new int[nin];
    while ((iend == 0) && (ierror == 0)) {
		cerr << "init field" << endl;
    	for (int i = 0; i < 16; i++) {
    		in[i] = -32767; // first row = undef
    	}
    	cerr << "starting qfelt" << ireq << " " << iexist << endl;
        qfelt(iunit,ireq,iexist,nin,in,ifound,&nfound,&iend,&ierror,&ioerr);
        if (ierror != 0) {
        	cerr << "iend, ierror: " << iend << "," << ierror << endl;
        } else {
 	       foundall += nfound;
 	       cerr << foundall << " " << nfound << endl;
    	    for (int i = 0; i < nfound; i++) {
				//cout << in[16*i + 0] << endl;
        		// do something with in[16*i + 0..15]
        	}
        }
    }
    cout << "found " << foundall << " elements" << endl;
	delete [] in;
	delete [] idrec1;
	delete [] ifound;
	// close feltfile 
	mrfelt(3,args[1],iunit,inmr,0,1,field,1.f,1024,idrec1,&ierror);
	delete [] inmr;
	return 1;
}
