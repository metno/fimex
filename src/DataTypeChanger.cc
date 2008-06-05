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

#include "DataTypeChanger.h"
#include "Data.h"

namespace MetNoFimex
{

DataTypeChanger::DataTypeChanger(CDMDataType oldType)
: oldType(oldType), newType(CDM_NAT)
{
}

DataTypeChanger::DataTypeChanger(CDMDataType oldType, double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset)
: oldType(oldType), newType(newType), oldFill(oldFill), newFill(newFill), oldScale(oldScale), newScale(newScale), oldOffset(oldOffset), newOffset(newOffset)
{
}

DataTypeChanger::~DataTypeChanger()
{
}

boost::shared_ptr<Data> DataTypeChanger::convertData(boost::shared_ptr<Data> data) const throw(CDMException)
{
	if (newType == CDM_NAT) {
		return data; // null - initialization, do nothing
	}
	return data->convertDataType(oldFill, oldScale, oldOffset,newType, newFill, newScale, newOffset);
}

CDMDataType DataTypeChanger::getDataType() const
{
	if (newType == CDM_NAT) {
		return oldType;
	} else {
		return newType;
	}
}


}
