/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#include "ValueParameterHandler.h"
#include "WdbIndex.h"
#include "Wdb2CdmBuilder.h"
#include "gridInformation/GridInformation.h"
#include "config/GlobalWdbConfiguration.h"
#include <fimex/Data.h>


namespace MetNoFimex
{
namespace wdb
{

ValueParameterHandler::ValueParameterHandler(const WdbIndex & index, const GlobalWdbConfiguration & config) :
		index_(index), config_(config)
{
}

ValueParameterHandler::~ValueParameterHandler()
{
}

void ValueParameterHandler::addToCdm(CDM & cdm) const
{
}

boost::shared_ptr<Data> ValueParameterHandler::getData(const CDMVariable & variable, size_t unLimDimPos) const
{
	return boost::shared_ptr<Data>();
//	const std::vector<std::string> & dimensions = variable.getShape();
//
//	unsigned size = 1;
//	for ( std::vector<std::string>::const_iterator it = dimensions.begin(); it != dimensions.end(); ++ it )
//	{
//		const CDMDimension & dimension = cdm_->getDimension(* it);
//		if ( not dimension.isUnlimited() )
//			size *= dimension.getLength();
//	}
//
//	boost::shared_ptr<Data> ret = createData(variable.getDataType(), size);
//
//	std::vector<wdb::Wdb2CdmBuilder::gid> fieldIdentifiers = index_.getData(config_.wdbName(variable.getName()), unLimDimPos);
//
//	float * dataIdx = reinterpret_cast<float *>(ret->getDataPtr());
//	for ( std::vector<wdb::Wdb2CdmBuilder::gid>::const_iterator it = fieldIdentifiers.begin(); it != fieldIdentifiers.end(); ++ it )
//		if ( * it != wdb::WdbIndex::UNDEFINED_GID )
//			dataIdx = wdbConnection_->getGrid(dataIdx, * it);
//		else
//		{
//			const wdb::GridInformation & gridInfo = index_.gridInformation();
//			dataIdx += gridInfo.numberX() * gridInfo.numberY();
//		}
//
//	return ret;
}

bool ValueParameterHandler::canHandle(const std::string & wdbName) const
{
	return false;
	return index_.hasParameter(wdbName);
}

}
}
