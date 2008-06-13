/*
 * Fimex
 * 
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

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

/**
 * comparison operator for pair<short, short> used for levelPairs
 * 
 */
struct ShortPairLess : public binary_function<const pair<short, short>, const pair<short, short>, bool> 
{
	bool operator()(const pair<short, short>& p1, const pair<short, short>& p2) const {
		if (p1.first == p2.first) return p1.second < p2.second;
		return p1.first < p2.first;
	}
};
/** set<pair<short,short> > with comparator*/ 
typedef set<pair<short,short>, ShortPairLess > ShortPairSet;
/** map<pair<short,short>, short> with comparator*/
typedef map<pair<short,short>, short, ShortPairLess > ShortPairMap;


/// encapsulate parameters of a felt file
/**
 * store local variables of a parameter, partially retrieved from the diana.setup,
 * partially retrieved from the file
 */
class Felt_Array
{
private:
	typedef map<time_t, boost::array<short, 4> > TIME_MAP;
	string feltArrayName;
	ShortPairSet levelPairs;
	// the time-array[0,1,2,3] correspond to index-array[2,3,4,9]
	TIME_MAP times;
	map<time_t, map<short, int> > fieldSizeMap;
	// ident19 stores extra informations which might be time/level dependent
	map<time_t, ShortPairMap> ident19;
	int nx;
	int ny;
	long scaling_factor;
	int gridType;
	boost::array<short, 16> idx;
	boost::array<short, 20> header;
	boost::array<float, 6> gridParameters;
	string dataType;
	double fillValue;
	
public:
	/** constructor */
	Felt_Array();
	/** 
	 * constructor applying the parameter name and the felt description index array
	 * \param name parameter name
	 * \param idx feltfiles are indexed by a short[16] arrays representing different parameters. The parameters used here are those applied to qfelt (query felt)
	 * \param dataType short|float|double datatype used for autoscaling, getScalingFactor() will be allways return 1 for float and double
	 */
	explicit Felt_Array(const string name, const boost::array<short, 16> idx, const string& dataType = "short");
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
	/** return the datatype as string short|float|double */
	const string& getDatatype() const {return dataType;}
	/** return the changed fill used in #Felt_File::getScaledDataSlice */
	double getFillValue() const { return fillValue;}
	/** set the fill value to be used in #Felt_File::getScaledDataSlice */
	void setFillValue(double fillValue) {this->fillValue = fillValue;}
	/** return the times available for this parameter, sorted */
	vector<time_t> getTimes() const;
	/** return the levels available for this parameter, sorted */
	vector<short> getLevels() const;
	/** 
	 * return the level pairs (niveau 1, niveau 2) for this parameter as used by hybrid levels
	 */
	vector<pair<short, short> > getLevelPairs() const;
	/**
	 * add the ident19 parameter from the data-header
	 */
	void addIdent19(time_t time, pair<short, short> levelPair, short value) {ident19[time][levelPair] = value;}
	/** 
	 * get the ident19 parameter from the data-header, throw error if levelPair/time doesn't exists
	 *  @warning only ident19 of data already read will be taken into account  
	 */
	short getIdent19(time_t time, pair<short, short> levelPair) const throw(Felt_File_Error);
	/** 
	 * get the ident19 parameter from the data-header, assures that the parameters keep constant
	 * across all times for each levelPair or throws a Felt_File_Error
	 * @warning only ident19 of data already read will be taken into account
	 */
	short getIdent19(pair<short, short> levelPair) const throw(Felt_File_Error);
	/** 
	 * get the ident19 parameter from the data-header, assures that the parameters keep constant
	 * across all levelPair for each time or throws a Felt_File_Error
	 * @warning only ident19 of data already read will be taken into account
	 */
	short getIdent19(time_t time) const throw(Felt_File_Error);
	/** 
	 * get the ident19 parameter from the data-header, assures that the parameters keep constant
	 * across all levelPair and times or throws a Felt_File_Error
	 * @warning only ident19 of data already read will be taken into account
	 */
	short getIdent19() const throw(Felt_File_Error);
		
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

/**
 * convert the 16-short header to a time
 */
time_t index16toTime(const boost::array<short,16>& idx);
/**
 * convert the 16-short header to a levelPair
 */
pair<short, short> index16toLevelPair(const boost::array<short, 16>& idx);

} // end namespace MetNoFelt
#endif /*FELT_ARRAY_H_*/
