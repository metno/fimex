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

#include "WdbConnection.h"

namespace MetNoFimex
{
namespace wdb
{


WdbConnection::WdbConnection(const std::string & connectString)
{
	connection_ = PQconnectdb(connectString.c_str());
	if ( !isConnected() )
		throw WdbException(connection_);

	PQclear(call_("SELECT wci.begin('wdb')"));
}

WdbConnection::~WdbConnection()
{
	if (connection_)
		PQfinish(connection_);
}

bool WdbConnection::isConnected()
{
    return ( CONNECTION_OK == PQstatus(connection_) ) ;
}

namespace
{
/// Automatic freeing of PGresult objects
class Scoped_PGresult : boost::noncopyable
{
	PGresult * result_;
public:
	explicit Scoped_PGresult(PGresult * result) : result_(result) {}
	~Scoped_PGresult()
	{
		PQclear(result_);
	}
	PGresult * get()
	{
		return result_;
	}
};
}

void WdbConnection::readGid(std::vector<GridData> & out, const std::string & dataProvider)
{
	std::string query = GridData::query(dataProvider);
	Scoped_PGresult result(call_(query));

	int tuples = PQntuples(result.get());
	for ( int i = 0; i < tuples; ++ i )
		out.push_back(GridData(result.get(), i));
}

PGresult * WdbConnection::call_(const std::string & query)
{
	PGresult * result = PQexec(connection_, query.c_str());
	if ( PQresultStatus(result) != PGRES_TUPLES_OK )
	{
		PQclear(result);
		throw WdbException(connection_);
	}
	return result;
}


WdbException::WdbException(PGconn * connection) :
		CDMException(PQerrorMessage(connection))
{}

}
}
