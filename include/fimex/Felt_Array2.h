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

#ifndef FELT_ARRAY2_H_
#define FELT_ARRAY2_H_
#include <string>
#include <set>
#include <vector>
#include <map>
#include <boost/array.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#include <fimex/Felt_Types.h>
#include "fimex/Felt_File_Error.h"

#include "felt/FeltTypes.h"

namespace MetNoFelt {
using namespace std;


/// A collection of FeltFields to build a 4-dimensional data array for 1-parameter and 1-vertical coordinate
/**
 * A Felt_Array2 collects all felt::FeltField from a felt::FeltFile with the same parameter and vertical coordinate. It is possible
 * to access the data of such each layer.
 */
class Felt_Array2
{
private:
    // sorted container for level -> field associations
    typedef map<LevelPair, boost::shared_ptr<felt::FeltField>, LevelPairLess> LevelFieldMap;
    // container for all available times and levels (ordered by time and level)
	typedef map<boost::posix_time::ptime, LevelFieldMap > TimeLevelFieldMap;
	string feltArrayName_;
	const boost::shared_ptr<felt::FeltField> defaultField_; // this is also one of the feltFields
    string dataType_;
    double fillValue_;
	TimeLevelFieldMap feltFields_; // main storage for all fields with same information
    /** constructor, no sensible default, since one feltField required for most information */
    Felt_Array2();
    void addField_(const boost::shared_ptr<felt::FeltField> field);
public:
	/** 
	 * constructor applying the parameter name and the felt description from the first feltField
	 * \param name parameter name
	 * \param field a field added to this constructor
	 * \param dataType short|float|double datatype used for autoscaling, getScalingFactor() will be always return 1 for float and double
	 * \param fillValue fillValue of the datatype, usually -32767
	 */
	explicit Felt_Array2(const string name, const boost::shared_ptr<felt::FeltField> feltField, const string& dataType, double fillValue);
	virtual ~Felt_Array2();
	/**
	 * add information from the felt-index (usually retrieved from qfelt) to this Felt_Array2
	 * the index given here must correspond to the initialization index
	 */
	void addInformationByField(boost::shared_ptr<felt::FeltField> field) throw(Felt_File_Error);
	
	/// get the time/level independent data-header
//	const boost::array<short, 20>& getDataHeader() const {}
	
	/** return the parameter name */
	const string& getName() const;
	/** return the datatype as string short|float|double */
	const string& getDatatype() const {return dataType_;}
	void getGrid(boost::posix_time::ptime time, LevelPair levelPair, vector<short>& gridOut) throw(Felt_File_Error);
    /// get the felt level type of this array
    int getLevelType() const;
	/** return the changed fill used in #Felt_File::getScaledDataSlice */
	double getFillValue() const {return fillValue_;}

	/** return the times available for this parameter, sorted */
	vector<boost::posix_time::ptime> getTimes() const;
	/** 
	 * return the level pairs (niveau 1, niveau 2) for this parameter as used by hybrid levels
	 */
	vector<LevelPair> getLevelPairs() const;
	/** 
	 * get the ident19 parameter from the data-header, throw error if levelPair/time doesn't exists
	 *  @warning only ident19 of data already read will be taken into account  
	 */
	int getIdent19(boost::posix_time::ptime time, LevelPair levelPair) const throw(Felt_File_Error);
		
	/** return x/longitude size */
	int getX() const;
	/** return y/latitude size */
	int getY() const;
	/** get the files scaleFactor, this corresponds to scalingFactor by 10^(scaleFactor) == scalingFactor */
	int scaleFactor() const;
	/** return scalingFactor */
	double getScalingFactor() const;
	boost::shared_ptr<felt::FeltGridDefinition> getGridDefinition() const;
	int getGridType() const;
    const boost::shared_ptr<felt::FeltField> getField(boost::posix_time::ptime time, LevelPair levelPair) const throw(Felt_File_Error);
};

} // end namespace MetNoFelt
#endif /*FELT_ARRAY2_H_*/
