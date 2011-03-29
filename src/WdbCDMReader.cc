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
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "wdb/WdbConnection.h"
#include "wdb/DataIndex.h"
#include "wdb/CdmNameTranslator.h"

namespace MetNoFimex
{



GxWdbCDMReader::GxWdbCDMReader(const std::string& source, const std::string& configfilename) :
		wdbConnection_(0), dataIndex_(0), translator_(0)
{
	try
	{
		translator_ = new wdb::CdmNameTranslator;

		wdbConnection_ = new wdb::WdbConnection("dbname=wdb");

		std::vector<wdb::GridData> data;
		wdbConnection_->readGid(data, "met.no eceps modification");

		dataIndex_ = new wdb::DataIndex(data, * translator_);
		dataIndex_->populate(* cdm_);

		cdm_->toXMLStream(std::cout);
	}
	catch (...)
	{
		delete translator_;
		delete wdbConnection_;
		delete dataIndex_;
		throw;
	}
}

GxWdbCDMReader::~GxWdbCDMReader()
{
	delete translator_;
	delete wdbConnection_;
	delete dataIndex_;
}

boost::shared_ptr<Data> GxWdbCDMReader::getDataSlice(
		const std::string& varName, size_t unLimDimPos) throw (CDMException)
{
	boost::shared_ptr<Data> ret = createData(CDM_FLOAT, 100 * 100, std::numeric_limits<float>::quiet_NaN());
	return ret;
}



}
