/*
 * Fimex, CoordSysBuilder.h
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Mar 28, 2012
 *      Author: Heiko Klein
 */

#ifndef COORDSYSBUILDER_H_
#define COORDSYSBUILDER_H_


#include <boost/shared_ptr.hpp>
#include "fimex/deprecated.h"
#include <string>
#include <vector>

namespace MetNoFimex
{

// forward decl
class CDM;
class CDMReader;
class CoordinateSystem;

class CoordSysBuilder
{
public:
    CoordSysBuilder() {};
    virtual ~CoordSysBuilder() {};
    virtual std::string getName() = 0;
    virtual bool isMine(const CDM& cdm) = 0;
    virtual std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(boost::shared_ptr<CDMReader> reader) = 0;
    virtual DEPRECATED(std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(CDM& cdm)) = 0;
    virtual void enhanceVectorProperties(boost::shared_ptr<CDMReader> reader) = 0;
};

} /* namespace MetNoFimex */
#endif /* COORDSYSBUILDER_H_ */
