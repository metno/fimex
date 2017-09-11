/*
 * Fimex, CF1xCoordSysBuilder.h
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

#ifndef CF1XCOORDSYSBUILDER_H_
#define CF1XCOORDSYSBUILDER_H_

#include "coordSys/CoordSysBuilder.h"

namespace MetNoFimex
{

/**
 * coordinate system builder to handle CF-1.x and compatible coordinate systems (COARDS)
 */
class CF1_xCoordSysBuilder : public CoordSysBuilder
{
public:
    CF1_xCoordSysBuilder();
    virtual ~CF1_xCoordSysBuilder();
    virtual std::string getName() {return "CF-1.x";}
    virtual bool isMine(const CDM& cdm);
    virtual std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(CDM& cdm);
    virtual std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(CDMReader_p reader);
    virtual void enhanceVectorProperties(CDMReader_p reader);

};

} /* namespace MetNoFimex */
#endif /* CF1XCOORDSYSBUILDER_H_ */
