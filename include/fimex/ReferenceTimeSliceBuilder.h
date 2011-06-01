/*
 * Fimex, ReferenceTimeSliceBuilder.h
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Jun 1, 2011
 *      Author: Heiko Klein
 */

#ifndef REFERENCETIMESLICEBUILDER_H_
#define REFERENCETIMESLICEBUILDER_H_


#include "SliceBuilder.h"

namespace MetNoFimex
{

// forward decl;
class CoordinateSystem;

class ReferenceTimeSliceBuilder: public MetNoFimex::SliceBuilder
{
public:
    ReferenceTimeSliceBuilder(const CDM& cdm, const std::string& varName, boost::shared_ptr<const CoordinateSystem> cs);
    virtual ~ReferenceTimeSliceBuilder() {};
    void setReferenceTimePos(size_t refTimePos);
    void setTimeStartAndSize(size_t start, size_t size);
    SliceBuilder getTimeVariableSliceBuilder();
private:
    boost::shared_ptr<const CoordinateSystem> cs_;
    std::vector<std::string> tShape_;
};



}

#endif /* REFERENCETIMESLICEBUILDER_H_ */
