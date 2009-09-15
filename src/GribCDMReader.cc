/*
 * Fimex, GribCDMReader.cc
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
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#include "fimex/CDM.h"
#include "fimex/GribCDMReader.h"
#include "fimex/GridDefinition.h"
#include "fimex/GribFileIndex.h"
#include "fimex/Logger.h"
#include <algorithm>
#include <map>
#include <set>

namespace MetNoFimex
{

using namespace std;

static LoggerPtr logger = getLogger("fimex.GribCDMReader");

GribCDMReader::GribCDMReader(const std::vector<std::string>& fileNames, const std::string& configFile)
    : configFile_(configFile)
{
    doc_ = boost::shared_ptr<XMLDoc>(new XMLDoc(configFile_));


    for (vector<string>::const_iterator fileIt = fileNames.begin(); fileIt != fileNames.end(); ++fileIt) {
        vector<GribFileMessage> messages = GribFileIndex(*fileIt).listMessages();
        copy(messages.begin(), messages.end(), back_inserter(indices_));
    }

    // currently only one gridDefinition for all paramters supported
    // TODO: select one valid gridDefinition in configFile, currently using first one found
    // search indices for params with same gridDefinition (size,start,incr)
    if (indices_.size() == 0) return;

    GridDefinition gd = indices_.at(0).getGridDefinition();
    vector<GribFileMessage> newIndices;
    for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
        if (gd.comparableTo(gfmIt->getGridDefinition(), 0.1)) {
            newIndices.push_back(*gfmIt);
        } else {
            LOG4FIMEX(logger, Logger::INFO, "different gridDefinitions between " << indices_.at(0) << " and " << *gfmIt);
        }
    }
    indices_ = newIndices;

    initAddGlobalAttributes();
    CDMDimension timeDim = initAddTimeDimension();
    map<long, CDMDimension> levelDims = initAddLevelDimensions();
    string projName, coordinates;
    initAddProjection(projName, coordinates);
    initAddVariables(projName, coordinates, timeDim, levelDims);
}

void GribCDMReader::initAddGlobalAttributes()
{
    // TODO: impl
}

map<long, CDMDimension> GribCDMReader::initAddLevelDimensions()
{
    // level-type mapping, only allow one set of levels per levelType
    map<long, set<long> > levelsOfType;
    for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
        levelsOfType[gfmIt->getLevelType()].insert(gfmIt->getLevelNumber());
    }

    map<long, CDMDimension> levelDimsOfType;
    // TODO: implement


    return levelDimsOfType;
}

CDMDimension GribCDMReader::initAddTimeDimension()
{
    // get all times, unique and sorted
    vector<boost::posix_time::ptime> times;
    {
        set<boost::posix_time::ptime> timesSet;
        for (vector<GribFileMessage>::const_iterator gfmIt = indices_.begin(); gfmIt != indices_.end(); ++gfmIt) {
            timesSet.insert(gfmIt->getDateTime());
        }
        times = vector<boost::posix_time::ptime>(timesSet.begin(), timesSet.end());
    }
    CDMDimension timeDim;
    // TODO implement
    return timeDim;
}

void GribCDMReader::initAddProjection(std::string& projName, std::string& coordinates)
{
    // TODO
}

void GribCDMReader::initAddVariables(const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const map<long, CDMDimension>& levelDims)
{
    // TODO
}


GribCDMReader::~GribCDMReader()
{
}

boost::shared_ptr<Data> GribCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
    // TODO: implement
}


}
