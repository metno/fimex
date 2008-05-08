#ifndef FELT_FILE_H_
#define FELT_FILE_H_

#include <ctime>
#include <map>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
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
	std::string filename;
	boost::shared_ptr<int> fdPtr; // file descriptor
	std::map<std::string, Felt_Array> feltArrayMap;
	FeltParameters feltParameters;
	
private:
	Felt_Array& findOrCreateFeltArray(const boost::array<short, 16>& idx);
	std::vector<short> getDataSlice(Felt_Array& fa, boost::array<short, 16>& idx, int fieldSize) throw(Felt_File_Error);
	boost::shared_array<short> getHeaderData(Felt_Array& fa, boost::array<short, 16>& idx, int fieldSize)  throw(Felt_File_Error);
	/// actually read the data with the parameters from the felt_file, should be called from constructors
	void init() throw(Felt_File_Error);
	
public:
	/// constructor
	/**
	 * open an empty felt file, just a default constructor, no useful information
	 */
	Felt_File() {}
	/**
	 * open and read toc of a felt file
	 * \param filename name of felt file
	 */
	explicit Felt_File(const std::string& filename) throw(Felt_File_Error);
	/**
	 * open and read toc of a felt file
	 * \param paramList a list of known parameters (in diana format, e.g. 17,2,1000:prod=74), only the known parameters will be read
	 * \warning The diana format is extended by dataType=short|float|double and fillValue=(number in short|float|double) to add the return type of the data.
	 * Autoscaling will be turned on for 'getDataSlice'. default is dataType=short:fillValue=-32767 
	 */
	explicit Felt_File(const std::string& filename, const std::vector<std::string>& dianaParamList) throw(Felt_File_Error);
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
	 * retrieve the data prescaled (if float or double) and replaced with the new fill value
	 * 
	 * @param compName parameter name of felt file
	 * @param time time of slice
	 * @param level level of slice
	 */	
	boost::shared_ptr<MetNoFimex::Data> getScaledDataSlice(const std::string& compName, const std::time_t time, const short level, double fillValue) throw(Felt_File_Error);

	/**
	 *  retrieve all felt arrays
	 */
	std::vector<Felt_Array> listFeltArrays();

	/**
	 *  Z-axis types and values
	 * @return map consisting of felt level-ids and a sorted vector of level values
	 */ 
	std::map<short, std::vector<short> > getFeltLevels() const;
	/**
	 *  Z-axis types and values
	 * @return map consisting of felt level-ids and a sorted vector of level-pairs of values
	 */ 
	std::map<short, std::vector<pair<short,short> > > getFeltLevelPairs() const;
	/// all time values, sorted
	std::vector<time_t> getFeltTimes() const;
	/// get size in x direction
	int getNX() const;
	/// get size in y direction
	int getNY() const;
	/// get the values of the x axis
	boost::shared_ptr<MetNoFimex::Data> getXData() const throw(Felt_File_Error);
	/// get the values of the y axis
	boost::shared_ptr<MetNoFimex::Data> getYData() const throw(Felt_File_Error);
	
	/// assumes one set of grid-type for the whole file
	short getGridType() const throw(Felt_File_Error);
	
	/// assumes one set of grid-parameters for the whole file
	const boost::array<float, 6>& getGridParameters() const throw(Felt_File_Error);
	
};


} // end namespace MetNoFelt
#endif /*FELT_FILE_H_*/
