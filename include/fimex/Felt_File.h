#ifndef FELT_FILE_H_
#define FELT_FILE_H_

#include <cstdio>
#include <map>
#include <vector>
#include <string>
#include "felt_reader/Felt_Array.h"
#include "felt_reader/Felt_File_Error.h"
#include "felt_reader/FeltParameters.h"

namespace MetNoFelt {

/// Felt File access
/** 
 * Felt_File gives c++ style access to felt files. It uses internally
 * libmi and caches the table of contents 
 * 
 */
class Felt_File
{
	const std::string filename;
	std::FILE* fh;
	std::map<std::string, Felt_Array> feltArrayMap;
	FeltParameters feltParameters;
	
private:
	Felt_Array& findOrCreateFeltArray(const boost::array<short, 16>& idx);
	
public:
	/// constructor
	/**
	 * open and read toc of a felt file
	 * \param filename name of felt file
	 */
	Felt_File(const std::string& filename);
	virtual ~Felt_File();
	//float* getData(const string& compName);
	
	/// retrieve a Felt_Array
	/**
	 * \param compName parameter name of felt file as named in diana setup
	 */ 
	Felt_Array& getFeltArray(const std::string& compName) throw(Felt_File_Error);
	vector<short> getDataSlice(const std::string& compName, const std::time_t time, const short level) throw(Felt_File_Error);

	/**
	 *  retrieve all felt arrays
	 */
	std::vector<Felt_Array> listFeltArrays();
	
	
};


} // end namespace MetNoFelt
#endif /*FELT_FILE_H_*/
