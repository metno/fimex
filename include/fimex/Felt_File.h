#ifndef FELT_FILE_H_
#define FELT_FILE_H_

#include <cstdio>
#include <ctime>
#include <map>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include "Data.h"
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
	std::vector<short> getDataSlice(Felt_Array& fa, boost::array<short, 16>& idx, int fieldSize) throw(Felt_File_Error);
	
public:
	/// constructor
	/**
	 * open and read toc of a felt file
	 * \param filename name of felt file
	 */
	explicit Felt_File(const std::string& filename);
	virtual ~Felt_File();
	//float* getData(const string& compName);
	
	/// retrieve a Felt_Array
	/**
	 * \param compName parameter name of felt file as named in diana setup
	 */ 
	Felt_Array& getFeltArray(const std::string& compName) throw(Felt_File_Error);
	/// retrieve a data slice
	/**
	 * @param compName parameter name of felt file
	 * @param time time of slice
	 * @param level level of slice
	 */
	std::vector<short> getDataSlice(const std::string& compName, const std::time_t time, const short level) throw(Felt_File_Error);

	/**
	 *  retrieve all felt arrays
	 */
	std::vector<Felt_Array> listFeltArrays();

	/**
	 *  Z-axis types and values
	 * @return map consisting of felt level-ids and a vector of level values
	 */ 
	std::map<short, std::vector<short> > getFeltLevels() const;
	/// all time values
	std::vector<time_t> getFeltTimes() const;
	/// get size in x direction
	int getNX() const;
	/// get size in y direction
	int getNY() const;
	/// get the values of the x axis
	boost::shared_ptr<MetNoUtplukk::Data> getXData() const throw(Felt_File_Error);
	/// get the values of the y axis
	boost::shared_ptr<MetNoUtplukk::Data> getYData() const throw(Felt_File_Error);
	
	/// assumes one set of grid-type for the whole file
	short getGridType() const throw(Felt_File_Error);
	
	/// assumes one set of grid-parameters for the whole file
	const boost::array<float, 6>& getGridParameters() const throw(Felt_File_Error);
	
};


} // end namespace MetNoFelt
#endif /*FELT_FILE_H_*/
