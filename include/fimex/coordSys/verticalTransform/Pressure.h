/*
 * Fimex, Pressure.h
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
 *      Author: heikok
 */

#ifndef PRESSURE_H_
#define PRESSURE_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex
{

// forward decl
class ToVLevelConverter;


/**
 * @headerfile fimex/coordSys/verticalTransform/Pressure.h
 */

/// Pressure class for vertical transformation
class Pressure : public VerticalTransformation
{
public:
    /// name of the pressure variable
    const std::string pressure;
    Pressure(std::string pressure) : pressure(pressure) {}
    virtual ~Pressure() {}
    /**
     * @return pressure
     */
    virtual std::string getName() const { return "pressure"; }
    virtual std::string getParamterString() const { return "p="+pressure; }
    virtual bool isComplete() const {return pressure != "";}
protected:
    virtual boost::shared_ptr<ToVLevelConverter> getPressureConverter(const boost::shared_ptr<CDMReader>& reader, size_t unLimDimPos, boost::shared_ptr<const CoordinateSystem> cs, size_t nx, size_t ny, size_t nt) const;
};

} /* namespace MetNoFimex */
#endif /* PRESSURE_H_ */
