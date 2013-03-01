/* -*- c++ -*-
 * Fimex, CDMReaderWriter.h
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
 *  Created on: Feb 4, 2013
 *      Author: heikok
 */

#ifndef CDMREADERWRITER_H_
#define CDMREADERWRITER_H_

#include "fimex/CDMReader.h"

namespace MetNoFimex
{
/**
 * The CDMReaderWriter is an interface allowing to write data to an opened CDMReader.
 */
class CDMReaderWriter : public MetNoFimex::CDMReader
{
public:
    CDMReaderWriter();
    virtual ~CDMReaderWriter();

    /**
     * @brief data-writing function
     *
     * This methods needs to be implemented by the CDMReaderWriter. It will write back the data
     * to a variable at unLimPos, without any changes (except eventually required change of datatypes).
     *
     * This function will write the whole data for a dataset without unlimited dimension if
     * the unLimDimPos == 0.
     *
     * @param varName name of the variable to read
     * @param unLimDimPos (optional) if the variable contains a unlimited dimension (max one allowed) an slice of this position is returned
     * @param data Data to be written to unLimDimPos
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual void putDataSlice(const std::string& varName, size_t unLimDimPos, const DataPtr data) = 0;
    /**
     * @brief data-writing  function for general slices
     *
     * @param varName name of the variable to read
     * @param sb a SliceBuilder generated from this CDMReaders CDM
     * @param data Data to be written at the slicePositions
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     * @warn This method must be implemented for performance reasons.
     */
    virtual void putDataSlice(const std::string& varName, const SliceBuilder& sb, const DataPtr data) = 0;

    virtual void putScaledDataSlice(const std::string& varName, size_t unLimDimPos, const DataPtr data);
    virtual void putScaledDataSliceInUnit(const std::string& varName, const std::string& unit, size_t unLimDimPos, const DataPtr data);
    virtual void putScaledDataSlice(const std::string& varName, const SliceBuilder& sb, const DataPtr data);
    virtual void putScaledDataSliceInUnit(const std::string& varName, const std::string& unit, const SliceBuilder& sb, const DataPtr data);

private:
    DataPtr unscaleDataOf(const std::string& varName, DataPtr data, double unitScale=1, double unitOffset=0);
    DataPtr unscaleDataFromUnitOf(const std::string& varName, DataPtr data, const std::string& newUnit);
}; /* class */

} /* MetNoFimex */
#endif /* CDMREADERWRITER_H_ */
