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
#include "boost/shared_ptr.hpp"
#include "fimex/GribFileIndex.h"
#include "fimex/CDMReader.h"

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
    void initAddGlobalAttributes();
    std::map<long, CDMDimension> initAddLevelDimensions();
    CDMDimension initAddTimeDimension();
    void initAddProjection(std::string& projName, std::string& coordinates);
    void initAddVariables(const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const std::map<long, CDMDimension>& levelDims);

};

}

#endif /* GRIBCDMREADER_H_ */
