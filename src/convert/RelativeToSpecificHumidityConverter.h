/*
 * Fimex, RelativeToSpecificHumidityConverter.h
 *
 * (C) Copyright 2015, met.no
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
 *  Created on: May 19, 2015
 *      Author: heikok
 */
#ifndef RELATIVETOSPECIFICHUMIDITYCONVERTER_H_
#define RELATIVETOSPECIFICHUMIDITYCONVERTER_H_

#include "ConverterImpl.h"
#include "fimex/Utils.h"

namespace MetNoFimex
{

class RelativeToSpecificHumidityConverter: public ConverterImpl
{
public:
    static const std::string& NAME();
    static std::vector<std::string> INPUT_STANDARD_NAMES() {return Varargs<std::string>("relative_humidity")("air_temperature")("pressure").args;}
    static std::vector<std::string> INPUT_UNITS() {return Varargs<std::string>("1")("K")("hPa").args;}
    static std::string OUTPUT_STANDARD_NAME() {return "specific_humidity";}
    static std::string OUTPUT_UNITS() {return "kg/kg";}
    RelativeToSpecificHumidityConverter();
    virtual ~RelativeToSpecificHumidityConverter();

    virtual const std::string& getName() const {return NAME();}
    virtual std::vector<std::string> getInputStandardNames() const {return INPUT_STANDARD_NAMES();}
    virtual std::vector<std::string> getInputUnits() const {return INPUT_UNITS();}
    virtual std::string getOutputStandardName() const {return OUTPUT_STANDARD_NAME();}
    virtual std::string getOutputUnits() const {return OUTPUT_UNITS();}

    /**
     * retrieve a slicebuilder, e.g. axes and size for the converted parameter
     * @return slicebuilder for the output-variable (as standard-name)
     */
    virtual SliceBuilder getSliceBuilder() const {} // TODO

    /**
     * Accessor-function to the converted data.
     * @param unlimPos position of the unlimited dimension
     * @return converted data (or data with size 0)
     */
    virtual DataPtr getData(size_t unlimDimPos) const {} // TODO
    /**
     * Accessor-function to the converted data.
     * @param sb
     * @return converted data (or data with size 0)
     */
    virtual DataPtr getData(SliceBuilder sb) const {} // TODO

    virtual ConverterPtr clone() const {return ConverterPtr(new RelativeToSpecificHumidityConverter(*this));}

};

} /* namespace MetNoFimex */

#endif /* RELATIVETOSPECIFICHUMIDITYCONVERTER_H_ */
