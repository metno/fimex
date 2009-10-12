/*
 * Fimex, GribFileIndex.h
 *
 * (C) Copyright 2009, met.no
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
 *
 *  Created on: Aug 31, 2009
 *      Author: Heiko Klein
 */

#ifndef GRIBFILEINDEX_H_
#define GRIBFILEINDEX_H_
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <vector>
#include "fimex/XMLDoc.h"
#include "fimex/GridDefinition.h"
#include <boost/date_time/posix_time/posix_time_types.hpp>

// forward decl of grib_api
struct grib_handle;

namespace MetNoFimex
{
class GribFileMessage
{
public:
    GribFileMessage();
    GribFileMessage(boost::shared_ptr<grib_handle> gh, const std::string& fileURL, long filePos, long msgPos);
    GribFileMessage(boost::shared_ptr<XMLDoc>, std::string nsPrefix, xmlNodePtr node);
    ~GribFileMessage();

    /// test if this is a proper GribFileMessage or just the default constructor
    bool isValid() const {return fileURL_ != "";}
    /// give a xml-string representation
    std::string toString() const;
    /// accessors
    const long getEdition() const;
    const std::string& getFileURL() const;
    const size_t getFilePosition() const;
    /// messages number within a multi-message
    const size_t getMessageNumber() const;
    const std::string& getName() const;
    const std::string& getShortName() const;
    boost::posix_time::ptime getDateTime() const;
    long getLevelNumber() const;
    long getLevelType() const;
    /**
     * Get the parameter ids as list with the following meanings:
     * @li ed1: indicatorOfParameter, gribTablesVersionNo, identificationOfOriginatingGeneratingCentre;
     * @li ed2: parameterNumber, paramterCategory, discipline
     */
    const std::vector<long>& getParameterIds() const;
    const std::string& getTypeOfGrid() const;
    const GridDefinition& getGridDefinition() const;
private:
    std::string fileURL_;
    size_t filePos_;
    size_t msgPos_; // for multiMessages: multimessages
    std::string parameterName_;
    std::string shortName_;
    // ed1: indicatorOfParameter, gribTablesVersionNo, identificationOfOriginatingGeneratingCentre;
    // ed2: parameterNumber, paramterCategory, discipline
    std::vector<long> gridParmeterIds_;
    long edition_;
    long dataTime_;
    long dataDate_;
    long levelType_;
    long levelNo_;
    std::string typeOfGrid_;
    GridDefinition gridDefinition_;
};

/// Functor to find Messages with equal time
class GribFileMessageEqualTime : public std::unary_function<bool, const GribFileMessage&> {
public:
    GribFileMessageEqualTime(boost::posix_time::ptime time) : time_(time) {}
    ~GribFileMessageEqualTime() {}
    bool operator()(const GribFileMessage& gfm) { return gfm.getDateTime() == time_; }
private:
    boost::posix_time::ptime time_;
};

/// Functor to find messages with equal level and time
class GribFileMessageEqualLevelTime : public std::unary_function<bool, const GribFileMessage&> {
public:
    GribFileMessageEqualLevelTime(long edition, long levelType, long levelNo, boost::posix_time::ptime time) : edition_(edition), levelType_(levelType), levelNo_(levelNo), time_(time) {}
    ~GribFileMessageEqualLevelTime() {}
    bool operator()(const GribFileMessage& gfm) { return (gfm.getEdition() == edition_) && (gfm.getLevelType() == levelType_) && (gfm.getLevelNumber() == levelNo_) && (gfm.getDateTime() == time_); }
private:
    long edition_;
    long levelType_;
    long levelNo_;
    boost::posix_time::ptime time_;
};

/**
 * read the data corresponding to the gfm to the vector data
 * data of at maximum data.size() will be read.
 * @param gfm GribFileMessage to read from
 * @param data the storage the data will be read to
 * @param missingValue the missing- / fill-value the returned data will have
 * @return the actual amount of data read
 */
size_t gribDataRead(const GribFileMessage& gfm, std::vector<double>& data, double missingValue);

class GribFileIndex
{
public:
    GribFileIndex();
    /**
     * Initialize the gribFileIndex for the gribFile gribFilePath.
     * If ignoreExistingXml = false, searches for existing indexes in
     * @li file.grbml
     * @li ENV{GRIB_FILE_INDEX}/file.grbml
     *
     * Otherwise, it parses the grib-file and creates a index.
     *
     * Performance for getting an index of a 150MB grib-file with some 10s of messages:
     * @li remote NFS file, first time: 16s
     * @li file completely in memory: 1.1s
     * @li xml-file: 0.1s
     */
    GribFileIndex(boost::filesystem::path gribFilePath, bool ignoreExistingXml = false);
    virtual ~GribFileIndex();
    const std::vector<GribFileMessage>& listMessages() const {return messages_;}
    const std::string& getUrl() const {return url_;}
private:
    std::string url_;
    std::vector<GribFileMessage> messages_;
    void initByGrib(boost::filesystem::path gribFilePath);
    void initByXML(boost::filesystem::path xmlFilePath);
};

/// outputstream for a GribFileMessage
std::ostream& operator<<(std::ostream& os, const GribFileMessage& gfm);
/// outputstream for a GribFileIndex
std::ostream& operator<<(std::ostream& os, const GribFileIndex& gfm);


}


#endif /* GRIBFILEINDEX_H_ */
