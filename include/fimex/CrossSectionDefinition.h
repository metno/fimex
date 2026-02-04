/*
 * Fimex, CrossSectionDefinition.h
 *
 * (C) Copyright 2013-2026, met.no
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
 *  Created on: Aug 9, 2013
 *      Author: heikok
 */

#ifndef CROSSSECTIONDEFINITION_H_
#define CROSSSECTIONDEFINITION_H_

namespace MetNoFimex
{

/**
 * @headerfile fimex/CrossSectionDefinition.h
 */
/**
 * Definition of a named cross-section.
 */
class CrossSectionDefinition
{
public:
    std::string name;
    std::vector<std::pair<double, double> > lonLatCoordinates;
    /**
     *
     * @param name the name of the cross-section
     * @param lonLatCoords a list of latitude/longitude points defining consecutive
     *        points of the cross-section
     */
    CrossSectionDefinition(std::string name, std::vector<std::pair<double, double> > lonLatCoords)
        : name(name), lonLatCoordinates(lonLatCoords) {}
    ~CrossSectionDefinition() {}
};

} /* namespace MetNoFimex */
#endif /* CROSSSECTIONDEFINITION_H_ */
