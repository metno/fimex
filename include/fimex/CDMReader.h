/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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
 */

#ifndef CDMREADER_H_
#define CDMREADER_H_

#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include "fimex/DataDecl.h"
#include "fimex/CDMException.h"
#include "fimex/SliceBuilder.h"

namespace MetNoFimex
{
/* forward declarations */
class CDM;
class CDMVariable;

/**
 * @brief Basic interface for CDM reading and manipulation classes
 *
 * The CDMReader is the basic interface for reading and manipulation of
 * the cdm datastructure. The {@link CDMWriter} will work with an implementation
 * of the CDMReader and read the included data in the cdm or the data provided
 * through the implementation of the {@link CDMReader#getDataSlice}
 *
 * @see FeltCDMReader
 */
class CDMReader : boost::noncopyable
{
public:
    CDMReader();
    virtual ~CDMReader() {}

    /**
     * Retrieve the cdm structure of this reader.
     */
    virtual const CDM& getCDM() const;
    /**
     * Retrieve the cdm structure of this reader, non-constant.
     *
     * Only in rare cases, this might be required.
     */
    virtual CDM& getInternalCDM();

    /**
     * @brief Read the sizes of the dimensions belonging to a variable.
     *
     * Read the dimension-sizes of a variable. This can be used to set the index
     * of the Data as received with on of the getData() functions, e.g.
     *
     * @code
     *  DataPtr d = reader->getData("my_var");
     *  if (d.size() != 0) {
     *     d.setDims(reader->getDims("my_var"));
     *  }
     * @endcode
     * @param varName the variables name
     * @return a vector with the dimension sizes usable with MetNoFimex::Index
     */
    virtual std::vector<std::size_t> getDims(std::string varName);
    /**
     * @brief Read the sizes of the dimensions belonging to a variable slice.
     *
     * Read the dimension-sizes of a variable. This can be used to set the index
     * of the Data as received with on of the getData() functions, e.g.
     *
     * @code
     *  DataPtr d = reader->getScaledDataSliceInUnit("my_var", "m", 3);
     *  if (d.size() != 0) {
     *     d.setDims(reader->getDimsSlice("my_var"));
     *  }
     * @endcode
     *
     * When using getDataSlice with a MetNoFimex::SliceBuilder, use SliceBuilder::getDimensionSizes().
     *
     * @param varName the variables name
     * @return a vector with the dimension sizes usable with MetNoFimex::Index
     */
    virtual std::vector<std::size_t> getDimsSlice(std::string varName);
    /**
     * @brief data-reading function to be called from the CDMWriter
     *
     * This methods needs to be implemented by the CDMReader. It should provide the data
     * for each variable, either by reading from disk, converting from another CDMReader or
     * reading from an in-memory data-section.
     *
     * This function should retrieve the whole data for a dataset without unlimited dimension if
     * the unLimDimPos == 0.
     *
     * @param varName name of the variable to read
     * @param unLimDimPos (optional) if the variable contains a unlimited dimension (max one allowed) an slice of this position is returned
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos) = 0;

    /**
     * @brief data-reading function to be called from the CDMWriter
     *
     * @param varName name of the variable to read
     * @param sb a SliceBuilder generated from this CDMReaders CDM
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     * @warn This method has a default implementation depending on #getDataSlice(varName, unLimDimPos),
     *       but should be implemented for performance reasons.
     */
    virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);

    /**
     * @brief data-reading function to be called from the CDMWriter
     *
     * The getData function is a convenient function to retrieve all data from a file.
     * It is implemented using getDataSlice. It should be used with care, since a complete
     * variable might be bigger than available memory.
     *
     * @param varName name of the variable to read
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual DataPtr getData(const std::string& varName);

    /**
     * @brief read and scale a dataslice
     *
     * This function uses getDataSlice internally. It tries to read
     * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
     * to the read data. Output-datatype will be double, output _FillValue
     * will be MIFI_UNDEFINED_D
     *
     * @param varName name of the variable to read
     * @param unLimDimPos (optional) if the variable contains a unlimited dimension (max one allowed) a slice of this position is returned
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual DataPtr getScaledDataSlice(const std::string& varName, size_t unLimDimPos);
    /**
     * @brief read and scale a dataslice to a known unit
     *
     * This function uses getDataSlice internally. It tries to read
     * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
     * to the read data. Output-datatype will be double, output _FillValue
     * will be MIFI_UNDEFINED_D. The data will be converted to match unit.
     *
     * @param varName name of the variable to read
     * @param unit unit-string
     * @param unLimDimPos (optional) if the variable contains a unlimited dimension (max one allowed) a slice of this position is returned
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual DataPtr getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, size_t unLimDimPos);

    /**
     * @brief read and scale a dataslice
     * @param varName name of the variable to read
     * @param sb SliceBuilder to restrict the data
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     * @see getScaledDataSlice(varName, unLimDimPos)
     */
    virtual DataPtr getScaledDataSlice(const std::string& varName, const SliceBuilder& sb);
    /**
     * @brief read and scale a dataslice to a set unit
     * @param varName name of the variable to read
     * @param unit unit string to scale to
     * @param sb SliceBuilder to restrict the data
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     * @see getScaledDataSlice(varName, unLimDimPos)
     */
    virtual DataPtr getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, const SliceBuilder& sb);

    /**
     * @brief read and scale the complete data
     *
     * This function uses getData internally. It tries to read
     * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
     * to the read data. Output-datatype will be double, output _FillValue
     * will be MIFI_UNDEFINED_D.
     *
     * @param varName name of the variable to read
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual DataPtr getScaledData(const std::string& varName);
    /**
     * @brief read and scale the complete data to a set unit
     *
     * This function uses getData internally. It tries to read
     * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
     * to the read data. Output-datatype will be double, output _FillValue
     * will be MIFI_UNDEFINED_D. The data will be converted to match unit.
     *
     * @param varName name of the variable to read
     * @param unit the unit-string to convert the data to
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual DataPtr getScaledDataInUnit(const std::string& varName, const std::string& unit);

protected:
    boost::shared_ptr<CDM> cdm_;
    /**
     * Read the data from the variable.hasData() and select the correct unLimDimPos.
     * This function should be used internally from getDataSlice.
     * @param variable the variable to read data from
     * @param unLimDimPos (optional) the unlimited position
     */
    virtual DataPtr getDataSliceFromMemory(const CDMVariable& variable, size_t unLimDimPos = 0);

    void getScaleAndOffsetOf(const std::string& varName, double& scale, double& offset);

private:
    DataPtr scaleDataOf(const std::string& varName, DataPtr data, double unitScale = 1., double unitOffset = 0.);
    DataPtr scaleDataToUnitOf(const std::string& varName, DataPtr data, const std::string& unit);
};

}

#endif /*CDMREADER_H_*/
