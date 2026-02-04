/*
 * Fimex, VerticalPerspectiveProjection.h
 *
 * (C) Copyright 2011-2026, met.no
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
 *  Created on: Mar 01, 2011
 *      Author: Heiko Klein
 */

#ifndef VERTICALPERSPECTIVEPROJECTION_H_
#define VERTICALPERSPECTIVEPROJECTION_H_

#include "fimex/coordSys/ProjectionImpl.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/coordSys/VerticalPerspectiveProjection.h
 */
class VerticalPerspectiveProjection : public ProjectionImpl
{
public:
    VerticalPerspectiveProjection();
    ~VerticalPerspectiveProjection();

    static bool acceptsProj4(const std::string& proj4Str);
    static std::vector<CDMAttribute> parametersFromProj4(const std::string& proj4);

protected:
    VerticalPerspectiveProjection(const std::string& name)
        : ProjectionImpl(name, false)
    {
    }
    std::ostream& getProj4ProjectionPart(std::ostream& oproj) const override;
};

} // namespace MetNoFimex

#endif /* VERTICALPERSPECTIVEPROJECTION_H_ */
