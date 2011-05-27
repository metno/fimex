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

#ifndef VALUEPARAMETERHANDLER_H_
#define VALUEPARAMETERHANDLER_H_

#include "DataHandler.h"

namespace MetNoFimex
{
namespace wdb
{
class GlobalWdbConfiguration;


class ValueParameterHandler : public DataHandler
{
public:
	ValueParameterHandler(const WdbIndex & index, const GlobalWdbConfiguration & config);
	virtual ~ValueParameterHandler();

	virtual void addToCdm(CDM & cdm) const;

	virtual boost::shared_ptr<Data> getData(const CDMVariable & variable, size_t unLimDimPos) const;

	virtual bool canHandle(const std::string & wdbName) const;


private:
	const WdbIndex & index_;
	const GlobalWdbConfiguration & config_;
};

}

}

#endif /* VALUEPARAMETERHANDLER_H_ */
