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

#ifndef CDMEXTRACTOR_H_
#define CDMEXTRACTOR_H_

#include <map>
#include <boost/array.hpp>
#include <boost/shared_ptr.hpp>

#include "fimex/CDMReader.h"
#include "fimex/CDMDataType.h"
#include "fimex/TimeUnit.h"

namespace MetNoFimex
{

class CDMExtractor : public MetNoFimex::CDMReader
{
private:
	boost::shared_ptr<CDMReader> dataReader;
	typedef std::map<std::string, boost::array<size_t, 2> > DimChangeMap; 
	DimChangeMap dimChanges;
	/**
	 * all extractors need to have another Reader with input-data
	 */
	CDMExtractor();

public:
	CDMExtractor(boost::shared_ptr<CDMReader> dataReader);
	virtual ~CDMExtractor();
	
	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);

	/**
	 * @brief Remove a variable from the CDM
	 * 
	 * @param name of the variable
	 * @throw CDMException if variable doesn't exist
	 */
	virtual void removeVariable(std::string variable) throw(CDMException);
	/**
	 * @brief Reduce a dimension of the file
	 * 
	 * @param name  dimension to change
	 * @param start start-position corresponding to the original dimension
	 * @param size  size of the new dimension
	 * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
	 */
	virtual void reduceDimension(std::string dimName, size_t start, size_t length) throw(CDMException);
	/**
	 * @brief Reduce a dimension of the file
	 * 
	 * @param name  dimension to change
	 * @param start start-position corresponding to the original dimension, defaults to 0
	 * @param end end-position of dimension, 0 means full size, negative values start from end
	 * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
	 */
	virtual void reduceDimensionStartEnd(std::string dimName, size_t start = 0, long end = 0) throw(CDMException);
	/**
	 * @brief reduce the time explicitly by a timestamp
	 *
	 * In contrast to #reduceDimension, this method allows the usage of
	 * absolute times. It will try to detect the reduction of dimensions as needed
	 *
	 * @warning reduceTime requires the times to be monotonic growing
	 * @warning reduceTime requires the file to come with a known convention, e.g. CF, see listCoordinateSystems()
	 * @warning reduceTime is not able to reduce multi-dimensional time-dimensions, e.g. time(time, station), yet
	 *
	 */
	virtual void reduceTime(const FimexTime& startTime, const FimexTime& endTime) throw(CDMException);

	/**
	 * @brief change the datatype of the variable
	 * 
	 * a change of the variable will also change the datatype of the _FillValue attribute
	 * 
	 * @param variable name of the variable
	 * @param datatype new datatype
	 * @throw CDMException if variable doesn't exist or conversion to datatype is not supported
	 */
	virtual void changeDataType(std::string variable, CDMDataType datatype) throw(CDMException);
	
};

}

#endif /*CDMEXTRACTOR_H_*/
