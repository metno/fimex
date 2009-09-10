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

#ifdef __cplusplus
extern "C"
{
#endif

#include "CDMReader.h"
#include <vector>
#include "boost/shared_ptr.hpp"

namespace MetNoFimex
{

// forward decl.
class CDM;

class GribCDMReader: public MetNoFimex::CDMReader
{
public:
    GribCDMReader(std::vector<std::string> fileNames, std::string configFile);
    virtual ~GribCDMReader();

    virtual const boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException);

private:
    std::string configFile_;
    std::vector<GribFileIndex> indices_;

};

}

#ifdef __cplusplus
}
#endif

#endif /* GRIBCDMREADER_H_ */
