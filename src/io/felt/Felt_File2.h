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

#include "FeltParameters.h"
#include "Felt_File_Error.h"
#include "Felt_Types.h"

#include "FeltTypes.h"

#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SharedArray.h"
#include "fimex/TimeUtils.h"

#include <map>
#include <memory>
#include <string>
#include <vector>

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
    explicit Felt_File2(const std::string& filename);
    /**
     * open and read toc of a felt file
     * \param filename name of felt file
     */
    explicit Felt_File2(const std::string& filename, const string& filenameSetup);
    /**
     * open and read toc of a felt file
     * \param dianaParamList a list of known parameters (in diana format, e.g. 17,2,1000:prod=74), only the known parameters will be read
     * \warning The diana format is extended by dataType=short|float|double and fillValue=(number in short|float|double) to add the return type of the data.
     * Autoscaling will be turned on for 'getDataSlice'. default is dataType=short:fillValue=-32767
     */
    explicit Felt_File2(const std::string& filename, const std::vector<std::string>& dianaParamList, const std::map<std::string, std::string>& options);
    virtual ~Felt_File2();
    // float* getData(const string& compName);

    /// retrieve a Felt_Array2
    /**
     * \param compName parameter name of felt file as named in diana setup
     */
    const std::shared_ptr<Felt_Array2> getFeltArray(const std::string& compName) const;
    /// retrieve a data slice
    /**
     * retrieve the data prescaled (if float or double) and replaced with the new fill value
     *
     * @param time time of slice
     * @param level level of slice
     */
    MetNoFimex::DataPtr getScaledDataSlice(std::shared_ptr<Felt_Array2> feltArray, const MetNoFimex::FimexTime& time, const LevelPair level);

    /**
     *  retrieve all felt arrays
     */
    std::vector<std::shared_ptr<Felt_Array2>> listFeltArrays() const;

    /**
     *  Z-axis types and values
     * @return map consisting of felt level-ids and a sorted vector of level values
     */
    // std::map<short, std::vector<short> > getFeltLevels() const;
    /**
     *  Z-axis types and values
     * @return map consisting of felt level-ids and a sorted vector of level-pairs of values
     */
    std::map<short, std::vector<LevelPair>> getFeltLevelPairs() const;
    /**
     * get all members of ensembles
     */
    std::vector<short> getEnsembleMembers() const;
    const std::map<LevelPair, int>& getHybridLevels() const { return hybridLevels_; }
    /// all time values, sorted
    std::vector<MetNoFimex::FimexTime> getFeltTimes() const;
    /**
     * get the unique reference time of the felt file
     * @return a unique reference time
     * @throw exception if no unique reference time exists
     */
    MetNoFimex::FimexTime getUniqueReferenceTime() const;
    /// get size in x direction
    int getNX() const;
    /// get size in y direction
    int getNY() const;
    /// get the values of the x axis
    std::shared_ptr<MetNoFimex::Data> getXData() const;
    /// get the values of the y axis
    std::shared_ptr<MetNoFimex::Data> getYData() const;

    /**
     *  assumes one set of grid-parameters for the whole file, returns parameter between 1 and 6, without extra definition
     */
    int getGridType() const;
    /// assumes one set of grid-parameters for the whole file
    std::shared_ptr<felt::FeltGridDefinition> getGridDefinition() const;

private:
    std::string filename_;
    std::string globalParameterOptions_;
    std::shared_ptr<felt::FeltFile> feltFile_;
    std::map<std::string, std::shared_ptr<Felt_Array2>> feltArrayMap_;
    FeltParameters feltParameters_;
    std::map<LevelPair, int> hybridLevels_;   // only set for files with idx[10] = 11
    std::array<float, 6> gridParameterDelta_; // allowed deviation between two grids

    bool findOrCreateFeltArray(const std::shared_ptr<felt::FeltField>);
    /// add processing options by strings, must be set before setOptions
    void setOptions(const std::map<std::string, std::string>& options);
    /// actually read the data with the parameters from the felt_file, should be called from constructors
    void init();
};

} // end namespace MetNoFelt
#endif /*FELT_FILE2_H_*/
