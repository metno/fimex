/*
 * Fimex, UnitsConverter.h
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
 *  Created on: Nov 6, 2013
 *      Author: heikok
 */
#ifndef UNITSCONVERTER_H_
#define UNITSCONVERTER_H_

#include "fimex/UnitsConverterDecl.h"

namespace MetNoFimex
{

/**
 * The UnitsConverter interface is used to convert values from one unit
 * to another.
 *
 * Implementations of this class can only be retrieved from Units().
 */
class UnitsConverter {
public:
    virtual ~UnitsConverter() {}
    /**
     * convert a value from the input unit to an output-unit
     * @param from value in the 'from' unit
     * @return value in the 'to' unit
     */
    virtual double convert(double from) = 0;
    /**
     * check if the converter is linear (representable by scale & offset)
     */
    virtual bool isLinear() = 0;
    /**
     * get the scale and offset
     * @throw UnitException if not linear
     */
    virtual void getScaleOffset(double& scale, double& offset) = 0;
    /**
     * convert() as functor.
     */
    double operator()(double from) {return convert(from);};
};

} /* namespace MetNoFimex */

#endif /* UNITSCONVERTER_H_ */
