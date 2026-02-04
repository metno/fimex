/*
 * Fimex, MercatorProjection.h
 *
 * (C) Copyright 2010-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 *  Created on: Apr 30, 2010
 *      Author: Heiko Klein
 */

#ifndef MERCATORPROJECTION_H_
#define MERCATORPROJECTION_H_

#include "fimex/coordSys/ProjectionImpl.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/coordSys/MercatorProjection.h
 */

class MercatorProjection : public ProjectionImpl
{
public:
    MercatorProjection();
    ~MercatorProjection();

    static bool acceptsProj4(const std::string& proj4Str);
    static std::vector<CDMAttribute> parametersFromProj4(const std::string& proj4);

protected:
    std::ostream& getProj4ProjectionPart(std::ostream& oproj) const override;
};

} // namespace MetNoFimex

#endif /* MERCATORPROJECTION_H_ */
