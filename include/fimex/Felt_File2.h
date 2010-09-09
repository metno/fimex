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

#ifndef FELT_FILE2_H_
#define FELT_FILE2_H_

#include <map>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include "fimex/Data.h"
#include "fimex/Felt_Types.h"
#include "fimex/Felt_File_Error.h"
#include "fimex/FeltParameters.h"
#include "fimex/Logger.h"
#include "felt/FeltTypes.h"

namespace MetNoFelt {

class Felt_Array2; // forward decl.

/// Felt File access
/**
 * Felt_File2 gives c++ style access to felt files. It uses internally
 * libmi and caches the table of contents
 *
 */
class Felt_File2
{
public:
	/// constructor
	/**
	 * open an empty felt file, just a default constructor, no useful information
	 */
	Felt_File2() {}
	/**
	 * open and read toc of a felt file
	 * \param filename name of felt file
	 */
	explicit Felt_File2(const std::string& filename) throw(Felt_File_Error);
	/**
	 * open and read toc of a felt file
	 * \param paramList a list of known parameters (in diana format, e.g. 17,2,1000:prod=74), only the known parameters will be read
	 * \warning The diana format is extended by dataType=short|float|double and fillValue=(number in short|float|double) to add the return type of the data.
	 * Autoscaling will be turned on for 'getDataSlice'. default is dataType=short:fillValue=-32767
	 */
	explicit Felt_File2(const std::string& filename, const std::vector<std::string>& dianaParamList, const std::map<std::string, std::string>& options) throw(Felt_File_Error);
	virtual ~Felt_File2();
	//float* getData(const string& compName);

	/// retrieve a Felt_Array2
	/**
	 * \param compName parameter name of felt file as named in diana setup
	 */
	const boost::shared_ptr<Felt_Array2> getFeltArray(const std::string& compName) const throw(Felt_File_Error);
	/// retrieve a data slice
	/**
	 * retrieve the data prescaled (if float or double) and replaced with the new fill value
	 *
	 * @param compName parameter name of felt file
	 * @param time time of slice
	 * @param level level of slice
	 */
	boost::shared_ptr<MetNoFimex::Data> getScaledDataSlice(boost::shared_ptr<Felt_Array2> feltArray, const boost::posix_time::ptime time, const LevelPair level) throw(Felt_File_Error);

	/**
	 *  retrieve all felt arrays
	 */
	std::vector<boost::shared_ptr<Felt_Array2> > listFeltArrays() const;

	/**
	 *  Z-axis types and values
	 * @return map consisting of felt level-ids and a sorted vector of level values
	 */
	//std::map<short, std::vector<short> > getFeltLevels() const;
	/**
	 *  Z-axis types and values
	 * @return map consisting of felt level-ids and a sorted vector of level-pairs of values
	 */
	std::map<short, std::vector<LevelPair> > getFeltLevelPairs() const;
	const std::map<LevelPair, int>& getHybridLevels() const {return hybridLevels_;}
	/// all time values, sorted
	std::vector<boost::posix_time::ptime> getFeltTimes() const;
	/**
	 * get the unique reference time of the felt file
	 * @return a unique reference time
	 * @throw exception if no unique reference time exists
	 */
	boost::shared_ptr<boost::posix_time::ptime> getUniqueReferenceTime() const;
	/// get size in x direction
	int getNX() const;
	/// get size in y direction
	int getNY() const;
	/// get the values of the x axis
	boost::shared_ptr<MetNoFimex::Data> getXData() const throw(Felt_File_Error);
	/// get the values of the y axis
	boost::shared_ptr<MetNoFimex::Data> getYData() const throw(Felt_File_Error);

    /**
     *  assumes one set of grid-parameters for the whole file, returns parameter between 1 and 6, without extra definition
     */
    int getGridType() const throw(Felt_File_Error);
	/// assumes one set of grid-parameters for the whole file
	boost::shared_ptr<felt::FeltGridDefinition> getGridDefinition() const throw(Felt_File_Error);

private:
    std::string filename_;
    boost::shared_ptr<felt::FeltFile> feltFile_;
    std::map<std::string, boost::shared_ptr<Felt_Array2> > feltArrayMap_;
    FeltParameters feltParameters;
    std::map<LevelPair, int> hybridLevels_; // only set for files with idx[10] = 11
    boost::array<float, 6> gridParameterDelta_; // allowed deviation between two grids

    bool findOrCreateFeltArray(const boost::shared_ptr<felt::FeltField>);
    /// actually read the data with the parameters from the felt_file, should be called from constructors
    void init(const std::map<std::string, std::string>& options) throw(Felt_File_Error);
    /// add processing options by strings, called from init()
    void setOptions(const std::map<std::string, std::string>& options);


};


} // end namespace MetNoFelt
#endif /*FELT_FILE2_H_*/
