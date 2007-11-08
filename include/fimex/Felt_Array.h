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
	boost::array<short, 16> idx;
	vector<short> extraGridInfo;
	
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
	void addInformationByIndex(const boost::array<short, 16> idx, int fieldSize) throw(Felt_File_Error);
	
	/** 
	 * set x and y dimension (or long/lat respectively) for this array
	 * since this information is not available before reading the data, it will be set late only, (though Felt_File does this automatically)
	 * x = y = ANY_VALUE() indicate not set value
	 * 
	 * @throw Felt_File_Error thrown when changing nx or ny values
	 */
	void setXandY(int nx, int ny) throw(Felt_File_Error);

	/**
	 *
	 * @throw Felt_File_Error thrown when changing scalingFactor
	 */
	void setScalingFactor(long scalingFactor) throw(Felt_File_Error);

	/**
	 * some felt-files contain some extra information (behind the data array)
	 * they should be set here. No tests are run when changing extra-information between fields
	 */
	void setExtraInformation(vector<short> extraInfo);

	/** return the parameter name */
	const string& getName();
	/** return the times available for this parameter */
	vector<time_t> getTimes();
	/** return the levels available for this parameter */
	vector<short> getLevels();
	/** return x/longitude size */
	const int getX();
	/** return y/latitude size */
	const int getY();
	/** return scalingFactor */
	const long getScalingFactor();
	
	/** return a copy of the index used within this Felt_Array */
	boost::array<short, 16> const getIndex(time_t time, short level) throw(Felt_File_Error);
	int const getFieldSize(time_t time, short level) throw (Felt_File_Error);
};

} // end namespace MetNoFelt
#endif /*FELT_ARRAY_H_*/
