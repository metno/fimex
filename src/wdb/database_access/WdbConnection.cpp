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
#include "DataSanitizer.h"
#include "WciReadQuerySpecification.h"
#include "../gridInformation/GridInformation.h"
#include <fimex/Logger.h>
#include <boost/scoped_array.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

extern "C"
{
#include <arpa/inet.h>
}

namespace MetNoFimex
{
namespace wdb
{
namespace
{
LoggerPtr logger = getLogger("WdbConnection");
}

// a cache holding references to all connections
static std::map<std::string, boost::weak_ptr<PGconn> > sharedConnections;

enum QueryResultFormat
{
    TextResult, BinaryResult
};

/**
 * Make a database call.
 *
 * You should call PQclear on result to free memory.
 *
 * @throws WdbException on error
 */
static PGresult * call(boost::shared_ptr<PGconn> connection, const std::string & query, QueryResultFormat resultFormat  = TextResult)
{
    using namespace boost::posix_time;
    ptime startTime;
    if (logger->isEnabledFor(Logger::DEBUG)) {
        startTime = microsec_clock::universal_time();
    }

    PGresult * result = PQexecParams(connection.get(), query.c_str(), 0, NULL, NULL, NULL, NULL, resultFormat == BinaryResult ? 1 : 0);
    if (logger->isEnabledFor(Logger::DEBUG)) {
        time_duration diff = microsec_clock::universal_time() - startTime;
        LOG4FIMEX(logger, Logger::DEBUG, "wdb called '"<< query << "' in " << (diff.total_milliseconds() / 1000.) <<"s");
    }
    if ( PQresultStatus(result) != PGRES_TUPLES_OK )
    {
        PQclear(result);
        throw WdbException(connection.get());
    }
    return result;
}

static boost::shared_ptr<PGconn> createConnection(const std::string & connectString, const std::string & wciUser)
{
    boost::shared_ptr<PGconn> connection(PQconnectdb(connectString.c_str()), PQfinish);
    if ( CONNECTION_OK != PQstatus(connection.get()) )
        throw WdbException(connection.get());

    std::ostringstream begin;
    begin << "SELECT wci.begin('" << DataSanitizer(connection.get())(wciUser) << "')";

    PQclear(call(connection, begin.str()));
    return connection;
}

WdbConnection::WdbConnection(const std::string & connectString, const std::string & wciUser)
{
    std::string connectionId = connectString + ";wciUser="+wciUser;
    // retrieve connecton from sharedConnections
    std::map<std::string, boost::weak_ptr<PGconn> >::iterator shared_conn = sharedConnections.find(connectionId);
    if (shared_conn != sharedConnections.end()) {
        connection_ = shared_conn->second.lock();
    }
    if (connection_.get() == 0) {
        // update sharedConnections if connection not present or out of date
        connection_ = createConnection(connectString, wciUser);
        sharedConnections[connectionId] = boost::weak_ptr<PGconn>(connection_);
    }
}

WdbConnection::~WdbConnection()
{}


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

void WdbConnection::readGid(std::vector<GridData> & out, const WciReadQuerySpecification & readParameters)
{
	std::string query = readParameters.query(DataSanitizer(connection_.get()));

	Scoped_PGresult result(call(connection_, query));

	int tuples = PQntuples(result.get());

	for ( int i = 0; i < tuples; ++ i )
	{
		GridData gridData(result.get(), i);
		out.push_back(gridData);
	}

	// Add grid information to data
	for ( unsigned i = out.size() - tuples; i < out.size(); ++ i )
	{
		GridData & gridData = out[i];
		GridInformationPtr gridInfo = readGridInformation(gridData.placeName());
		gridData.setGridInformation(gridInfo);
	}
}

WdbConnection::GridInformationPtr WdbConnection::readGridInformation(const std::string & gridName)
{
	GridList::const_iterator find = gridsInUse_.find(gridName);
	if ( find == gridsInUse_.end() )
	{
		Scoped_PGresult result(call(connection_, GridInformation::query(gridName, DataSanitizer(connection_.get()))));

		int tuples = PQntuples(result.get());
		if ( tuples == 0 )
			throw WdbException("Unknown grid name: " + gridName);
		else if ( tuples > 1 )
			throw WdbException("Ambiguous grid name: " + gridName);

		GridInformationPtr ret = GridInformation::get(result.get(), 0);
		find = gridsInUse_.insert(std::make_pair(gridName, ret)).first;
	}
	return find->second;
}

namespace
{
struct convert_to_host_order : public std::unary_function<float, float>
{
	float operator () (float f)
	{
		union
		{
			int32_t i;
			float f;
		} data;
		data.f = f;

		data.i = ntohl(data.i);

		return data.f;
	}
};
}

float * WdbConnection::getGrid(float * buffer, GridData::gid gridIdentifier)
{
	std::ostringstream query;
	query << "SELECT grid::bytea FROM wci.fetch(" << gridIdentifier << ", NULL::wci.grid)";

	Scoped_PGresult result(call(connection_, query.str(), BinaryResult));

	int tuples = PQntuples(result.get());
	if ( tuples == 0 )
		throw WdbException("Unknown grid id");
	if ( tuples > 1 )
		throw WdbException("Internal error (got more than one grid from query");

	float * data = (float *) PQgetvalue(result.get(), 0, 0);
	int dataLength = PQgetlength(result.get(), 0, 0);

	if ( dataLength % sizeof(float) )
		throw WdbException("Invalid field size");

// If this is defined, expect wdb to send back floats in network order, instead of it's native order.
#ifdef EXPECT_NETWORK_ORDER_BYTES
	float * ret = std::transform(data, data + (dataLength / 4), buffer, convert_to_host_order());
#else
	float * ret = std::copy(data, data + (dataLength / 4), buffer);
#endif

	return ret;
}


WdbException::WdbException(PGconn * connection) :
		CDMException(PQerrorMessage(connection))
{}
WdbException::WdbException(const std::string & msg) :
		CDMException(msg)
{}


}
}
