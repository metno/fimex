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
#include "fimex/CDMException.h"
#include "fimex/SliceBuilder.h"

namespace MetNoFimex
{
/* forward declarations */
class Data;
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
class CDMReader
{
public:
	CDMReader();
	virtual ~CDMReader() {}

	/**
	 * Retrieve the cdm structure of this reader.
	 */
	virtual const CDM& getCDM() const;
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
	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) = 0;

    /**
     * @brief data-reading function to be called from the CDMWriter
     *
     * @param varName name of the variable to read
     * @param sb a SliceBuilder generated from this CDMReaders CDM
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     * @warn This method has a default implementation depending on #getDataSlice(varName, unLimDimPos),
     *       but should be implemented for performance reasons.
     */
	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

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
	virtual boost::shared_ptr<Data> getData(const std::string& varName);

	/**
	 * @brief read and scale a dataslice
	 *
	 * This functions uses getDataSlice internally. It tries to read
	 * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
	 * to the read data. Output-datatype will be double, output _FillValue
	 * will be MIFI_UNDEFINED_D
	 *
	 * @param varName name of the variable to read
	 * @param unLimDimPos (optional) if the variable contains a unlimited dimension (max one allowed) an slice of this position is returned
	 * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
	 */
	virtual boost::shared_ptr<Data> getScaledDataSlice(const std::string& varName, size_t unLimDimPos);
	/**
     * @brief read and scale a dataslice to a known unit
     *
     * This functions uses getDataSlice internally. It tries to read
     * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
     * to the read data. Output-datatype will be double, output _FillValue
     * will be MIFI_UNDEFINED_D. The data will be converted to match unit.
     *
     * @param varName name of the variable to read
     * @param unit unit-string
     * @param unLimDimPos (optional) if the variable contains a unlimited dimension (max one allowed) an slice of this position is returned
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
	virtual boost::shared_ptr<Data> getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, size_t unLimDimPos);

	/**
	 * @brief read and scale a dataslice
	 * @param varName name of the variable to read
	 * @param sb SliceBuilder to restrict the data
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
	 * @see getScaledDataSlice(varName, unLimDimPos)
	 */
	virtual boost::shared_ptr<Data> getScaledDataSlice(const std::string& varName, const SliceBuilder& sb);
    /**
     * @brief read and scale a dataslice to a set unit
     * @param varName name of the variable to read
     * @param unit unit string to scale to
     * @param sb SliceBuilder to restrict the data
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     * @see getScaledDataSlice(varName, unLimDimPos)
     */
    virtual boost::shared_ptr<Data> getScaledDataSliceInUnit(const std::string& varName, const std::string& unit, const SliceBuilder& sb);

	/**
	 * @brief read and scale the complete data
	 *
	 * This functions uses getData internally. It tries to read
	 * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
	 * to the read data. Output-datatype will be double, output _FillValue
	 * will be MIFI_UNDEFINED_D.
	 *
	 * @param varName name of the variable to read
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
	 */
	virtual boost::shared_ptr<Data> getScaledData(const std::string& varName);
    /**
     * @brief read and scale the complete data to a set unit
     *
     * This functions uses getData internally. It tries to read
     * "scale_factor" "add_offset" and "_FillValue" and apply the scaling
     * to the read data. Output-datatype will be double, output _FillValue
     * will be MIFI_UNDEFINED_D. The data will be converted to match unit.
     *
     * @param varName name of the variable to read
     * @param unit the unit-string to convert the data to
     * @throw CDMException on errors related to the CDM in combination with the underlying data-structure. It might also throw other (IO-)exceptions.
     */
    virtual boost::shared_ptr<Data> getScaledDataInUnit(const std::string& varName, const std::string& unit);

protected:
	boost::shared_ptr<CDM> cdm_;
	/**
	 * Read the data from the variable.hasData() and select the correct unLimDimPos.
	 * This function should be used internally from getDataSlice.
	 * @param variable the variable to read data from
	 * @param unLimDimPos (optional) the unlimited position
	 */
	virtual boost::shared_ptr<Data> getDataSliceFromMemory(const CDMVariable& variable, size_t unLimDimPos = 0);
private:
	boost::shared_ptr<Data> scaleDataOf(const std::string& varName, boost::shared_ptr<Data> data, double unitScale = 1., double unitOffset = 0.);
	boost::shared_ptr<Data> scaleDataToUnitOf(const std::string& varName, boost::shared_ptr<Data> data, const std::string& unit);
	void getScaleAndOffsetOf(const std::string& varName, double& scale, double& offset);
};

}

#endif /*CDMREADER_H_*/
