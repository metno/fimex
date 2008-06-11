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

#ifndef DATATYPECHANGER_H_
#define DATATYPECHANGER_H_
#include "CDMDataType.h"
#include "boost/shared_ptr.hpp"
#include "CDMException.h"

namespace MetNoFimex
{
class Data; // forward decl.


/** brief wrapper class around data->convertType */
class DataTypeChanger
{
	CDMDataType oldType;
	CDMDataType newType;
	double oldFill;
	double newFill;
	double oldScale;
	double newScale;
	double oldOffset;
	double newOffset;
public:
	/**
	 * initialize data with the oldType
	 * convertData will do nothing in this case
	 */
	explicit DataTypeChanger(CDMDataType oldType);
	/**
	 * initialize with the old and new settings
	 * 
	 * @param oldType datatype of original data
	 * @param oldFill fill value of the original data
	 * @param oldScale scale_factor of the original data
	 * @param oldOffset scale_factor of the original data
	 * @param newType datatype of converted data
	 * @param newFill fill value of converted data
	 * @param newScale scale_factor of the converted data
	 * @param newOffset add_offset of the converted data
	 * @param unitScale scale_factor for the unpacked data, i.e. for unit changes, default 1.
	 * @param unitOffset offset for the unpacked data, i.e. for unit changes, default 0.
	 */
	explicit DataTypeChanger(CDMDataType oldType, double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset, double unitScale = 1., double unitOffset = 0.);
	virtual ~DataTypeChanger();
	/**
	 * convert the data to the new scale/fill/offset
	 */
	boost::shared_ptr<Data> convertData(boost::shared_ptr<Data>) const throw(CDMException);
	/**
	 * return the datatype of the converted data 
	 */
	CDMDataType getDataType() const;
	
};

}

#endif /*DATATYPECHANGER_H_*/
