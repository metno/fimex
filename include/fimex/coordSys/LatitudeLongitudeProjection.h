/*
 * Fimex, LatitudeLongitudeProjection.h
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: Apr 28, 2010
 *      Author: Heiko Klein
 */

#ifndef LATITUDELONGITUDEPROJECTION_H_
#define LATITUDELONGITUDEPROJECTION_H_

#include "fimex/coordSys/ProjectionImpl.h"

namespace MetNoFimex
{

/**
 * @headerfile fimex/coordSys/LatitudeLongitudeProjection.h
 */
class LatitudeLongitudeProjection: public MetNoFimex::ProjectionImpl
{

public:
    LatitudeLongitudeProjection();
    virtual ~LatitudeLongitudeProjection() {}
    static bool acceptsProj4(const std::string& proj4Str);
    static std::vector<CDMAttribute> parametersFromProj4(const std::string& proj4);
protected:
    virtual std::ostream& getProj4ProjectionPart(std::ostream& oproj) const {
        return oproj << "+proj=latlong";
    }


};

}


#endif /* LATITUDELONGITUDEPROJECTION_H_ */
