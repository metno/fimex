/*
 * Fimex, VerticalTransformation.h
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Aug 6, 2013
 *      Author: Heiko Klein
 */

#ifndef VERTICALTRANSFORMATION_H_
#define VERTICALTRANSFORMATION_H_

#include <string>
#include <vector>
#include <iostream>

/**
 * @headerfile "fimex/coordSys/verticalTransform/VerticalTransformation.h"
 */


namespace MetNoFimex
{

/// base class for vertical transformations
class VerticalTransformation
{
public:
    virtual ~VerticalTransformation() {}
    /// the indentifier of the vertical transformation
    virtual std::string getName() const = 0;
    /// list the paramters
    virtual std::string getParamterString() const = 0;
    /**
     *  Indicate if all parameters are given. In some cases
     *  a transformation might be used as indicator, even if
     *  transformations to other vertical transformations are not
     *  possible.
     */
    virtual bool isComplete() const = 0;
    // TODO: add a function to get a ToVLevelConverter, e.g.
    // virtual boost::shared_ptr<ToVLevelConvert> getConverter(const boost::shared_ptr<CDMReader>& reader, boost::shared_ptr<const CoordinateSystem> cs, int verticalType) const
};

std::ostream& operator<<(std::ostream& out, const VerticalTransformation& vt);

} /* namespace MetNoFimex */



#endif /* VERTICALTRANSFORMATION_H_ */
