#ifndef FELT_ARRAY_H_
#define FELT_ARRAY_H_
#include <string>
#include <set>
#include <vector>
#include <map>
#include <ctime>
#include <boost/array.hpp>
#include "Felt_File_Error.h"

namespace MetNoFelt {
using namespace std;

typedef std::map<time_t, boost::array<short, 4> > TIME_MAP;

/// encapsulate parameters of a felt file
/**
 * store local variables of a parameter, partially retrieved from the diana.setup,
 * partially retrieved from the file
 */
class Felt_Array
{
	string feltArrayName;
	set<short> levels;
	// the time-array[0,1,2,3] correspond to index-array[2,3,4,9]
	TIME_MAP times;
	map<time_t, map<short, int> > fieldSizeMap;
	int nx;
	int ny;
	long scaling_factor;
	int gridType;
	boost::array<short, 16> idx;
	boost::array<short, 20> header;
	boost::array<float, 6> gridParameters;
	
public:
	/** constructor */
	Felt_Array();
	/** 
	 * constructor applying the parameter name and the felt description index array
	 * \param name parameter name
	 * \param idx feltfiles are indexed by a short[16] arrays representing different parameters. The parameters used here are those applied to qfelt (query felt)
	 */
	explicit Felt_Array(const string name, const boost::array<short, 16> idx);
	virtual ~Felt_Array();
	/**
	 * add information from the felt-index (usually retrieved from qfelt) to this Felt_Array
	 * the index given here must correspond to the initialization index
	 */
	void addInformationByIndex(const boost::array<short, 16>& idx, int fieldSize) throw(Felt_File_Error);
	/// @brief get the time/level independent index of this header
	const boost::array<short, 16>& getIndexHeader() const {return idx;}
	
	/**
	 * @brief set the felt data-header for this array
	 * a Felt_File_Error will be thrown if the header is different for the different times/layers of this Array
	 * @throw Felt_File_Error when data-definitions change 
	 */
	void setDataHeader(boost::array<short, 20> header) throw(Felt_File_Error);
	/// get the time/level independent data-header
	const boost::array<short, 20>& getDataHeader() const {return header;} 
	
	/// get the felt level type of this array
	short getLevelType() const {return idx[10];}
	/// set the gridType as used in libmi gridPar function
	void setGridType(int gridType) {this->gridType = gridType;}
	/// get the gridType
	int getGridType() const {return gridType;}
	/**
	 * set all the grid parameters from the felt file as retrieved from libmi's gridPar function 
	 */
	void setGridParameters(boost::array<float, 6> gridParameters) {this->gridParameters = gridParameters;}
	/// get the extra grid information from the end of the data
	const boost::array<float, 6>& getGridParameters() const {return gridParameters;}

	/** return the parameter name */
	const string& getName() const;
	/** return the times available for this parameter */
	vector<time_t> getTimes() const;
	/** return the levels available for this parameter */
	vector<short> getLevels() const;
	/** return x/longitude size */
	int getX() const {return header[9];}
	/** return y/latitude size */
	int getY() const {return header[10];}
	/** return the felt-type of the vertical axis */
	short getVerticalFeltType() const {return idx[10];}
	/** return scalingFactor */
	double getScalingFactor() const;
	
	/** return a copy of the index used within this Felt_Array */
	boost::array<short, 16> const getIndex(time_t time, short level) throw(Felt_File_Error);
	int getFieldSize(time_t time, short level) const throw (Felt_File_Error);
	
private:
	void testHeaderElement(short oldVal, short newVal, const std::string& msg) const throw(Felt_File_Error);
};

} // end namespace MetNoFelt
#endif /*FELT_ARRAY_H_*/
