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

#include "fimex/WdbCDMReader.h"

#include <sstream>
#include <ctime>
#include <iterator>

#include <boost/foreach.hpp>

#include "fimex/WdbCDMReaderParser.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "wdb/WdbConnection.h"
#include "wdb/Wdb2CdmBuilder.h"
#include "wdb/CdmNameTranslator.h"
#include "wdb/GridInformation.h"

namespace MetNoFimex
{

class GxWdbCDMReader::InternalData
{
public:
	InternalData(const std::string& source, const std::string& configfilename, CDM & cdm) :
		wdbConnection(0), dataIndex(0), translator(0)
	{
		try
		{
			WdbCDMReaderParser configParser;
			config = configParser.parse(source, configfilename);

			translator = new wdb::CdmNameTranslator;

			wdbConnection = new wdb::WdbConnection(config);

			std::vector<wdb::GridData> data;
			wdbConnection->readGid(data, config);

			dataIndex = new wdb::Wdb2CdmBuilder(data, * translator);
			dataIndex->populate(cdm);
		}
		catch (...)
		{
			delete dataIndex;
			delete wdbConnection;
			delete translator;
			throw;
		}
	}

	~InternalData()
	{
		delete dataIndex;
		delete wdbConnection;
		delete translator;
	}

	wdb::WdbConnection * wdbConnection;
	wdb::Wdb2CdmBuilder * dataIndex;
	wdb::CdmNameTranslator * translator;

	WdbCDMReaderParserInfo config;
};


GxWdbCDMReader::GxWdbCDMReader(const std::string& source, const std::string& configfilename)
{
	d_ = new InternalData(source, configfilename, * cdm_);
	//cdm_->toXMLStream(std::cout);
}

GxWdbCDMReader::~GxWdbCDMReader()
{
	delete d_;
}

boost::shared_ptr<Data> GxWdbCDMReader::getDataSlice(
		const std::string& varName, size_t unLimDimPos)
{
	std::cout << __func__ << "(\"" << varName << "\", " << unLimDimPos << ");";

	boost::shared_ptr<Data> ret;
	const CDMVariable& variable = cdm_->getVariable(varName);
	const std::string & wdbName = d_->translator->toWdbName(varName);

	if ( d_->dataIndex->isDatabaseField(wdbName) )
	{
		const std::vector<std::string> & dimensions = variable.getShape();
		unsigned size = 1;
		for ( std::vector<std::string>::const_iterator it = dimensions.begin(); it != dimensions.end(); ++ it )
		{
			const CDMDimension & dimension = cdm_->getDimension(* it);
			if ( not dimension.isUnlimited() )
				size *= dimension.getLength();
		}

		ret = createData(variable.getDataType(), size/*, std::numeric_limits<float>::quiet_NaN()*/);

		std::vector<wdb::Wdb2CdmBuilder::gid> fieldIdentifiers = d_->dataIndex->getGridIdentifiers(wdbName, unLimDimPos);

		float * dataIdx = reinterpret_cast<float *>(ret->getDataPtr());
		for ( std::vector<wdb::Wdb2CdmBuilder::gid>::const_iterator it = fieldIdentifiers.begin(); it != fieldIdentifiers.end(); ++ it )
			dataIdx = d_->wdbConnection->getGrid(dataIdx, * it);
	}
	else if ( varName.substr(0, 11) == "projection_" )
	{
		ret = createData(variable.getDataType(), 0);
	}
	else if ( varName == "x" )
	{
		const wdb::GridInformation & grid = d_->dataIndex->gridInformation();
		ret = createData(variable.getDataType(), grid.numberX());
		for ( unsigned i = 0; i < grid.numberX(); ++ i )
			ret->setValue(i, grid.startX() + (grid.incrementX() * i));
	}
	else if ( varName == "y" )
	{
		const wdb::GridInformation & grid = d_->dataIndex->gridInformation();
		ret = createData(variable.getDataType(), grid.numberY());
		for ( unsigned i = 0; i < grid.numberY(); ++ i )
			ret->setValue(i, grid.startY() + (grid.incrementY() * i));
	}
	else if ( varName == "longitude" )
	{
		const wdb::GridInformation & grid = d_->dataIndex->gridInformation();
		ret = createData(variable.getDataType(), grid.numberX() * grid.numberY());
	}
	else if ( varName == "latitude" )
	{
		const wdb::GridInformation & grid = d_->dataIndex->gridInformation();
		ret = createData(variable.getDataType(), grid.numberX() * grid.numberY());
	}
	else if ( varName == "forecast_reference_time" )
	{
		ret = createData(variable.getDataType(), 1);
		std::tm t = to_tm(d_->dataIndex->referenceTime());
		ret->setValue(0, std::mktime(& t));
	}
	else if ( varName == "time" )
	{
		// fix time functions: TimeUnit.h
		const std::set<wdb::GridData::Time> & allTimes = d_->dataIndex->allTimes();
		std::set<wdb::GridData::Time>::const_iterator thisTime = allTimes.begin();
		std::advance(thisTime, unLimDimPos -1);
		std::tm t = to_tm(* thisTime);
		ret = createData(variable.getDataType(), 1, std::mktime(& t));
	}
	else
	{
		throw CDMException("internal error: " + varName + ": unrecognized variable");
	}
	std::cout << "\tdone" << std::endl;
	return ret;
}


}
