/*
 * Fimex, ObliqueMercatorProjection.h
 *
 * Copyright (C) 2017-2019, SMHI, met.no
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
 *  Created on: May 12, 2017
 *      Author: Martin Raspaud
 */

#ifndef OBLIQUEMERCATORPROJECTION_H_
#define OBLIQUEMERCATORPROJECTION_H_

#include "fimex/coordSys/ProjectionImpl.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/coordSys/ObliqueMercatorProjection.h
 */
class ObliqueMercatorProjection: public MetNoFimex::ProjectionImpl
{
public:
    ObliqueMercatorProjection();
    ~ObliqueMercatorProjection();
    static bool acceptsProj4(const std::string& proj4Str);
    static std::vector<CDMAttribute> parametersFromProj4(const std::string& proj4);

protected:
    std::ostream& getProj4ProjectionPart(std::ostream& oproj) const override;
};

} // namespace MetNoFimex

#endif /* OBLIQUEMERCATORPROJECTION_H_ */
