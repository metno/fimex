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

#include "VersionHandler.h"
#include "WdbIndex.h"
#include <fimex/CDM.h>
#include <fimex/Data.h>
#include <boost/foreach.hpp>

namespace MetNoFimex
{

namespace wdb
{

VersionHandler::VersionHandler(const WdbIndex & index) :
		index_(index)
{
}

VersionHandler::~VersionHandler()
{
}

void VersionHandler::addToCdm(CDM & cdm) const
{
	BOOST_FOREACH(const std::string & parameter, index_.allParameters())
	{
		const std::vector<int> & versions = index_.versionsForParameter(parameter);
		if ( versions.size() > 1 )
		{
			std::string dimension = "version";
			cdm.addDimension(CDMDimension(dimension, versions.size()));

			cdm.addVariable(CDMVariable(dimension, CDM_INT, std::vector<std::string>(1, dimension)));

			cdm.addAttribute(dimension, CDMAttribute("long_name", "data version"));
			cdm.addAttribute(dimension, CDMAttribute("standard_name", "version"));
			cdm.addAttribute(dimension, CDMAttribute("axis", "Ensemble"));

			versions_ = versions;

			return;
		}
	}
}

boost::shared_ptr<Data> VersionHandler::getData(const CDMVariable & variable, size_t unLimDimPos) const
{
	boost::shared_ptr<Data> ret = createData(CDM_INT, versions_.size());

	int * dataIdx = reinterpret_cast<int *>(ret->getDataPtr());
	std::copy(versions_.begin(), versions_.end(), dataIdx);

	return ret;

}

bool VersionHandler::canHandle(const std::string & wdbName) const
{
	return wdbName == "version";
}


}
}
