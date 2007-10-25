#ifndef FELT_FILE_H_
#define FELT_FILE_H_

#include <cstdio>
#include <map>
#include <vector>
#include <string>
#include "felt_reader/Felt_Array.h"
#include "felt_reader/Felt_File_Error.h"
#include "felt_reader/FeltParameters.h"

using namespace std;

/// Felt File access
/** 
 * Felt_File gives c++ style access to felt files. It uses internally
 * libmi and caches the table of contents 
 * 
 */
class Felt_File
{
	const string filename;
	FILE* fh;
	map<string, Felt_Array> feltArrayMap;
	FeltParameters feltParameters;
	
private:
	Felt_Array& findOrCreateFeltArray(const boost::array<short, 16>& idx);
	
public:
	/// constructor
	/**
	 * open and read toc of a felt file
	 * \param filename name of felt file
	 */
	Felt_File(const string& filename);
	virtual ~Felt_File();
	//float* getData(const string& compName);
	
	/// retrieve a Felt_Array
	/**
	 * \param compName parameter name of felt file as named in diana setup
	 */ 
	Felt_Array& getFeltArray(const string& compName);

	/**
	 *  retrieve all felt arrays
	 */
	vector<Felt_Array> listFeltArrays();
	
	
};
#endif /*FELT_FILE_H_*/
