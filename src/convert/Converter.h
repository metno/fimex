/*
 * Fimex, Converter.h
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
 *  Created on: May 15, 2015
 *      Author: heikok
 */
#ifndef CONVERTER_H_
#define CONVERTER_H_

#include "fimex/Utils.h"
#include "fimex/Data.h"
#include "fimex/SliceBuilder.h"
#include "fimex/CDMNamedEntity.h"
#include "ConverterSrc.h"
#include "CDMReaderVar.h"
#include <vector>
#include <boost/shared_ptr.hpp>



namespace MetNoFimex
{

//forward decl.
class Converter;
class ConverterImpl;

typedef boost::shared_ptr<Converter> ConverterPtr;

class Converter : public CDMNamedEntity
{
public:
    /**
     * @return the standard_names of the input variables to the conversion
     */
    virtual std::vector<std::string> getInputStandardNames() const = 0;
    /**
     * @return the units to read from the input-source
     */
    virtual std::vector<std::string> getInputUnits() const = 0;
    /**
     * @return the semi-static standard-name of the output-variable
     */
    virtual std::string getOutputStandardName() const = 0;
    /**
     * @return the units of the output
     */
    virtual std::string getOutputUnits() const = 0;

    /**
     * retrieve a slicebuilder, e.g. axes and size for the converted parameter
     * @return slicebuilder for the output-variable (as standard-name)
     */
    virtual SliceBuilder getSliceBuilder() const = 0;

    /**
     * Accessor-function to the converted data.
     * @param unlimPos position of the unlimited dimension
     * @return converted data (or data with size 0)
     */
    virtual DataPtr getData(size_t unlimDimPos) const = 0;
    /**
     * Accessor-function to the converted data.
     * @param sb
     * @return converted data (or data with size 0)
     */
    virtual DataPtr getData(SliceBuilder sb) const = 0;

    /**
     * Create a deep copy of the object. Must be implemented by all inheriting classes.
     * @return a clone of the object
     */
    virtual ConverterPtr clone() const = 0;

protected:
    /**
     * add real data-source to the variable, one variable per one standard_name as of getInputStandardNames()
     * @param vars
     * @return false if something goes easily detectable wrong
     */
    virtual bool charge(std::vector<CDMReaderVar> vars) = 0;
    /**
     * check if the converter is already charged.
     * @return true if charged, false otherwise
     */
    virtual bool isCharged() const = 0;
    /**
     * remove all connections to real data from this converter
     */
    virtual void clear() = 0;

    /**
     * The general constructor for Converters.
     * @param conversionName the name of the conversion
     * @param srcs the resources to find the input in.
     * @return a list of converters which are able to find the required input data in the sources
     */
    static std::vector<ConverterPtr> findByName(std::string conversionName, Varargs<ConverterSrcPtr> srcs);
    /**
     * Find a converter for a special output-variable
     * @param outputStdName the desired output standard name
     * @param srcs the resources to find the input in.
     * @return a list of converters which provide the desired output and find the required
     * input data in the sources
     */
    static std::vector<ConverterPtr> findByOutput(std::string outputStdName, Varargs<ConverterSrcPtr> srcs);

protected:
    Converter() {}
    virtual ~Converter() {}
};

} /* namespace MetNoFimex */

#endif /* CONVERTER_H_ */
