/*
 * Fimex, GribCDMReader.h
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

#ifndef GRIBCDMREADER_H_
#define GRIBCDMREADER_H_

#include <vector>
#include <map>
#include <set>
#include "boost/shared_ptr.hpp"
#include "fimex/GribFileIndex.h"
#include "fimex/CDMReader.h"
#include "fimex/ReplaceStringObject.h"

namespace MetNoFimex
{

// forward decl.
class CDM;
class CDMDimension;
class Data;

class GribCDMReader: public MetNoFimex::CDMReader
{
public:
    GribCDMReader(const std::vector<std::string>& fileNames, const std::string& configFile);
    virtual ~GribCDMReader();
    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException);

private:
    std::string configFile_;
    std::vector<GribFileMessage> indices_;
    boost::shared_ptr<XMLDoc> doc_;
    std::string xDimName_;
    std::string yDimName_;
    std::string timeDimName_;
    // store ptimes of all times
    std::vector<boost::posix_time::ptime> times_;
    // store level parameters of level-ids: edition, level-type, level-no
    std::map<std::string, std::vector<std::vector<long> > > levels_;
    /**
     * config attributes may contain template parameters marked with %PARAM%
     * which should be replaced by dynamic values from the felt-file and stored
     * temporary in this map
     *
     * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
     */
    std::map<std::string, boost::shared_ptr<ReplaceStringObject> > templateReplacementAttributes_;
    /**
     * map from cdm variable names to list of gribMessages
     */
    std::map<std::string, std::vector<GribFileMessage> > varName2gribMessages_;

    /** Define which parameters to select
     * @param select can be "all", "definedOnly"
     */
    void initSelectParamters(const std::string& select);
    /**
     * find the node in the xml-config corresponding to the GribFileMessage
     * @return 0 if not found, otherwise a valid node
     */
    xmlNodePtr findVariableXMLNode(const GribFileMessage& msg) const;
    void initAddGlobalAttributes();
    /// key of the levelDimensions is gribEdition_levelType, i.e. 2_100
    void initLevels(long edition, const std::map<long, std::set<long> >& levelsOfType, std::map<std::string, CDMDimension>& levelDimsOfType);
    std::map<std::string, CDMDimension> initAddLevelDimensions();
    void initAddTimeDimension();
    void initAddProjection(std::string& projName, std::string& coordinates);
    void initAddVariables(const std::string& projName, const std::string& coordinates, const std::map<std::string, CDMDimension>& levelDims);

};


}

#endif /* GRIBCDMREADER_H_ */
