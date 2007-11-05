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
	int nx;
	int ny;
	boost::array<short, 16> idx;
	
public:
	/** constructor */
	Felt_Array();
	/** 
	 * constructor applying the parameter name and the felt description index array
	 * \param name parameter name
	 * \param idx feltfiles are indexed by a short[16] arrays representing different parameters. The parameters used here are those applied to qfelt (query felt)
	 */
	Felt_Array(const string name, const boost::array<short, 16> idx);
	virtual ~Felt_Array();
	/**
	 * add information from the felt-index (usually retrieved from qfelt) to this Felt_Array
	 * the index given here must correspond to the initialization index
	 */
	void addInformationByIndex(const boost::array<short, 16> idx);
	/** return the parameter name */
	const string& getName();
	/** return the times available for this parameter */
	vector<time_t> getTimes();
	/** return the levels available for this parameter */
	vector<short> getLevels();
	
	/** return a copy of the index used within this Felt_Array */
	boost::array<short, 16> const getIndex() {return idx;}
	boost::array<short, 16> const getIndex(time_t time) throw(Felt_File_Error);
};

} // end namespace MetNoFelt
#endif /*FELT_ARRAY_H_*/
