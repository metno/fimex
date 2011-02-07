#include "gridexer/GxWdbExplorer.h"
#include "gridexer/GxWdbDataTypes.h"

// fimex
//
#include "fimex/DataImpl.h"
#include "fimex/CDMDataType.h"

// standard
//
#include <cstring>
#include <assert.h>
#include <iostream>
#include <sstream>
#include <netinet/in.h>
#include <arpa/inet.h>

// boost
//
#include <boost/algorithm/string/join.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>


void GxWdbExplorer::getValueAsString(PGresult* pgResult, const int rowPosition, const int columnPosition, std::string& stringHolder)
{
    stringHolder = std::string(PQgetvalue(pgResult, rowPosition, columnPosition));
}

std::string GxWdbExplorer::getValueAsString(PGresult* pgResult, const int rowPosition, const int columnPosition)
{
    return std::string(PQgetvalue(pgResult, rowPosition, columnPosition));
}

void GxWdbExplorer::getValueAsInt4(PGresult* pgResult, const int rowPosition, const int columnPosition, int& intHolder)
{
    intHolder = ntohl(*((uint32_t *) PQgetvalue(pgResult, rowPosition, columnPosition)));
}

int GxWdbExplorer::getValueAsInt4(PGresult* pgResult, const int rowPosition, const int columnPosition)
{
    return ntohl(*((uint32_t *) PQgetvalue(pgResult, rowPosition, columnPosition)));
}

void GxWdbExplorer::getValueAsInt8(PGresult* pgResult, const int rowPosition, const int columnPosition, long long& llHolder)
{
    char *ptrToBuffer = PQgetvalue(pgResult, rowPosition, columnPosition);

    llHolder = *(reinterpret_cast<long long*>(ptrToBuffer));
    uint32_t      h32;
    uint32_t      l32;
    std::memcpy((char*)&h32, (char*)&llHolder, sizeof(uint32_t));
    std::memcpy((char*)&l32, ((char*)&llHolder + sizeof(uint32_t)), sizeof(uint32_t));

    h32 = ntohl(h32);
    l32 = ntohl(l32);

    llHolder = h32;
    llHolder <<= 32;
    llHolder |= l32;
}

long long GxWdbExplorer::getValueAsInt8(PGresult* pgResult, const int rowPosition, const int columnPosition)
{
    long long llHolder;
    getValueAsInt8(pgResult, rowPosition, columnPosition, llHolder);
    return llHolder;
}

void GxWdbExplorer::getValueAsFloat4(PGresult* pgResult, const int rowPosition, const int columnPosition, float& floatHolder)
{
    union
    {
        int   i;
        float f;
    } swap;

    swap.i = ntohl(*((uint32_t *) PQgetvalue(pgResult, rowPosition, columnPosition)));

    floatHolder = swap.f;
}

float GxWdbExplorer::getValueAsFloat4(PGresult* pgResult, const int rowPosition, const int columnPosition)
{
    float floatHolder;
    getValueAsFloat4(pgResult, rowPosition, columnPosition, floatHolder);
    return floatHolder;
}

void GxWdbExplorer::getValueAsFloat8(PGresult* pgResult, const int rowPosition, const int columnPosition, double& doubleHolder)
{
    union
    {
        double    d;
        long long ll;
    } swap;

    char *ptrToBuffer = PQgetvalue(pgResult, rowPosition, columnPosition);

    swap.ll = *(reinterpret_cast<long long*>(ptrToBuffer));
    uint32_t      h32;
    uint32_t      l32;
    std::memcpy((char*)&h32, (char*)&swap.ll, sizeof(uint32_t));
    std::memcpy((char*)&l32, ((char*)&swap.ll + sizeof(uint32_t)), sizeof(uint32_t));

    h32 = ntohl(h32);
    l32 = ntohl(l32);

    swap.ll = h32;
    swap.ll <<= 32;
    swap.ll |= l32;

    doubleHolder = swap.d;
}

double GxWdbExplorer::getValueAsFloat8(PGresult* pgResult, const int rowPosition, const int columnPosition)
{
    double doubleHolder;
    getValueAsFloat8(pgResult, rowPosition, columnPosition, doubleHolder);
    return doubleHolder;
}

GxWdbExplorer::GxWdbExplorer()
    : wdbPGConn_(0)
{

}

GxWdbExplorer::GxWdbExplorer(const std::string& dbHost, const std::string& dbName, const std::string& dbUser, const unsigned dbPort)
    : dbHost_(dbHost), dbName_(dbName), dbUser_(dbUser), dbPort_(dbPort)
{

}

GxWdbExplorer::~GxWdbExplorer()
{
    deinit();
}

void GxWdbExplorer::setDbHost(const std::string& dbHost)
{
    dbHost_ = dbHost;
}

void GxWdbExplorer::setDbName(const std::string& dbName)
{
    this->dbName_ = dbName;
}
void GxWdbExplorer::setDbUser(const std::string& dbUser)
{
    this->dbUser_ = dbUser;
}

void GxWdbExplorer::setDbPort(const unsigned int dbPort)
{
    this->dbPort_ = dbPort;
}

std::string GxWdbExplorer::dbName()
{
    return dbName_;
}

std::string GxWdbExplorer::dbName() const
{
    return dbName();
}

std::string GxWdbExplorer::dbHost()
{
    return dbHost_;
}

std::string GxWdbExplorer::dbHost() const
{
    return dbHost();
}

std::string GxWdbExplorer::dbUser()
{
    return dbUser_;
}

std::string GxWdbExplorer::dbUser() const
{
    return dbUser();
}

unsigned int GxWdbExplorer::dbPort()
{
    return dbPort_;
}

unsigned int GxWdbExplorer::dbPort() const
{
    return dbPort();
}

std::string GxWdbExplorer::wciUser() const
{
    return dbUser();
}

std::string GxWdbExplorer::wciUser()
{
    return dbUser();
}

std::string GxWdbExplorer::connectString()
{
    // make the string used
    // to initialise db conn
    //
    std::ostringstream ost;
    ost << "dbname=" << dbName() << " port=" << dbPort() << " user=" << dbUser() << " host=" << dbHost();
    return ost.str();
}

std::string GxWdbExplorer::connectString() const
{
    return connectString();
}

// TODO:
// throw exception
//
bool GxWdbExplorer::connectToWdb()
{
    // init libpq connection
    //
    assert(wdbPGConn_ == 0);
    wdbPGConn_ = PQconnectdb(connectString().c_str());
    assert(wdbPGConn_);

    if (PQstatus(wdbPGConn_) != CONNECTION_OK)
    {
        std::cerr << "Connection to database failed: " << PQerrorMessage(wdbPGConn_);
        PQfinish(wdbPGConn_);
        wdbPGConn_ = 0;
        return false;
    }

    // init wci context
    //
    PGresult   *res;
    const char *paramValues[1];
    paramValues[0] = wciUser().c_str();

    // init wci user
    //
    res = PQexecParams(wdbPGConn_,
                       "SELECT wci.begin($1)",
                       1,       /* one param */
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       0);

    if (PQresultStatus(res) != PGRES_TUPLES_OK)
    {
        std::cerr << "wci.begin failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQfinish(wdbPGConn_);
        wdbPGConn_ = 0;
        return false;
    }

    // TODO:
    // clear res in case of failire
    //
    PQclear(res);

    return true;
}

std::string GxWdbExplorer::wdbVersion()
{
    if(!wdbVersion_.empty())
        return wdbVersion_;

    PGresult *res;

    // query
    //
    res = PQexecParams(wdbPGConn_,
                       "select version from wci.version()",
                       0,
                       NULL,    /* let the backend deduce param type */
                       NULL,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(res) != PGRES_TUPLES_OK)
    {
        std::cerr << "wci.version failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(res);
        PQfinish(wdbPGConn_);
        return std::string(); // empty
    }

    int record_count = PQntuples(res);
    int version_fnum = PQfnumber(res, "version");

    //extract rows
    //
    for (int i = 0; i < record_count; ++i)
    {
        wdbVersion_ = std::string(PQgetvalue(res, i, version_fnum));

        std::cerr << wdbVersion_.c_str() << std::endl;
    }

    PQclear(res);

    return wdbVersion_;
}

// ATM very primitive
void GxWdbExplorer::clearCache()
{
    gids_.clear();
    gids_.resize(0);
    valueparameters_.clear();
    valueparameters_.resize(0);
    levelparameters_.clear();
    levelparameters_.resize(0);
    validtimes_.clear();
    validtimes_.resize(0);
//    referencetimes_.clear();
//    referencetimes_.resize(0);
    places_.clear();
    places_.resize(0);
    providers_.clear();
    providers_.resize(0);
}

// TODO:
// throw exception
//
bool GxWdbExplorer::init()
{
    // connect to wdb
    //
    assert(connectToWdb());

    // intialize variable at begining
    //
    wdbVersion();

    return true;
}

bool GxWdbExplorer::deinit()
{
    std::cerr << __FUNCTION__ << " don't forget cleanup " << std::endl;
    clearCache();
    return true;
}

// TODO: parse version number
//    to comapre actual value
//
void GxWdbExplorer::getGridDescription(const std::string& placeName, std::vector<GxPlaceRegularGridRow>& gridDescriptionRows)
{
    // common part
    //
    PGresult *pgResult;
    const char *paramValues[1];

    paramValues[0] = placeName.empty() ? 0 : placeName.c_str();

    if(wdbVersion() == std::string("WDB 0.9.2")) {
        pgResult = PQexecParams(wdbPGConn_,
                           "SELECT placename, projdefinition, inumber::float4, jnumber::float4, startlongitude::float4, startlatitude::float4, iincrement::float4, jincrement::float4 FROM wci.placespecification() where placename = $1",
                           1,
                           NULL,    /* let the backend deduce param type */
                           paramValues,
                           NULL,    /* don't need param lengths since text */
                           NULL,    /* default to all text params */
                           1);      /* ask for binary results */
    } else {
        pgResult = PQexecParams(wdbPGConn_,
                           "SELECT placename, projdefinition, numberx::float4, numbery::float4, startx::float4, starty::float4, incrementx::float4, incrementy::float4 from wci.getplaceregulargrid($1)",
                           1,
                           NULL,    /* let the backend deduce param type */
                           paramValues,
                           NULL,    /* don't need param lengths since text */
                           NULL,    /* default to all text params */
                           1);      /* ask for binary results */
    }

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << "wci.placespecification() failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        gridDescriptionRows.clear();
        return;
    }

    // common part
    //
    int record_count = PQntuples(pgResult);
    gridDescriptionRows.resize(record_count);

    std::cerr << record_count << std::endl;

    int placename_fnum = -1;
    int projdefinition_fnum = -1;
    int numberx_fnum = -1;
    int numbery_fnum = -1;
    int startx_fnum = -1;
    int starty_fnum = -1;
    int incrementx_fnum = -1;
    int incrementy_fnum = -1;

    if(wdbVersion() == std::string("WDB 0.9.2")) {
        placename_fnum = PQfnumber(pgResult, "placename");
        projdefinition_fnum = PQfnumber(pgResult, "projdefinition");
        numberx_fnum = PQfnumber(pgResult, "inumber");
        numbery_fnum = PQfnumber(pgResult, "jnumber");
        startx_fnum = PQfnumber(pgResult, "startlongitude");
        starty_fnum = PQfnumber(pgResult, "startlatitude");
        incrementx_fnum = PQfnumber(pgResult, "iincrement");
        incrementy_fnum = PQfnumber(pgResult, "jincrement");
    } else {
        placename_fnum = PQfnumber(pgResult, "placename");
        projdefinition_fnum = PQfnumber(pgResult, "projdefinition");
        numberx_fnum = PQfnumber(pgResult, "numberx");
        numbery_fnum = PQfnumber(pgResult, "numbery");
        startx_fnum = PQfnumber(pgResult, "startx");
        starty_fnum = PQfnumber(pgResult, "starty");
        incrementx_fnum = PQfnumber(pgResult, "incrementx");
        incrementy_fnum = PQfnumber(pgResult, "incrementy");
    }

    //extract rows
    //
    for (int i = 0; i < record_count; ++i)
    {
        GxPlaceRegularGridRow row;
        GxPlaceRow placeRow;
        placeRow.setName(GxWdbExplorer::getValueAsString(pgResult, i, placename_fnum));
        row.setPlace(placeRow);
        row.setProjectionDefinition(GxWdbExplorer::getValueAsString(pgResult, i, projdefinition_fnum));
        row.setStartX(GxWdbExplorer::getValueAsFloat4(pgResult, i, startx_fnum));
        row.setStartY(GxWdbExplorer::getValueAsFloat4(pgResult, i, starty_fnum));
        row.setNumberX(GxWdbExplorer::getValueAsFloat4(pgResult, i, numberx_fnum));
        row.setNumberY(GxWdbExplorer::getValueAsFloat4(pgResult, i, numbery_fnum));
        row.setIncrementX(GxWdbExplorer::getValueAsFloat4(pgResult, i, incrementx_fnum));
        row.setIncrementY(GxWdbExplorer::getValueAsFloat4(pgResult, i, incrementy_fnum));

        gridDescriptionRows[i] = row;
    }
}

void GxWdbExplorer::getGids(const std::vector<std::string>& providers,
                            const std::string& place,
                            const std::string& referenceTime,
                            const std::string& validTime,
                            const std::vector<std::string>& values,
                            const std::string& level,
                            const std::vector<std::string>& version,
                            std::vector<GxGidRow>& gidRows)
{
    if(!gids_.empty()) {
        gidRows.clear();
        gidRows.insert(gidRows.begin(), gids_.begin(), gids_.end());
    }

    // prepare data
    std::string strdataproviders = "NULL";
    std::vector<std::string> tmp_dataproviders(providers);
    if(!tmp_dataproviders.empty()) {
        strdataproviders = "{";
        strdataproviders.append(boost::algorithm::join(tmp_dataproviders, std::string(",")));
        strdataproviders.append("}");
    }

    std::string strvalueparameters = "NULL";
    std::vector<std::string> tmp_valueparameters(values);
    if(!tmp_valueparameters.empty()) {
        strvalueparameters = "{";
        strvalueparameters.append(boost::algorithm::join(tmp_valueparameters, std::string(",")));
        strvalueparameters.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(version);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = providers.empty() ? 0 : strdataproviders.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = referenceTime.empty() ? 0 : referenceTime.c_str();
    paramValues[3] = validTime.empty() ? 0 : validTime.c_str();
    paramValues[4] = values.empty() ? 0 : strvalueparameters.c_str();
    paramValues[5] = level.empty() ? 0 : level.c_str();
    paramValues[6] = version.empty() ? 0 : strdataversion.c_str();

    std::cout << __FUNCTION__ << "@" << __LINE__ << std::endl;
    paramValues[0] ? std::cout << "paramValues[0] : " << std::string(paramValues[0]) << std::endl : std::cout << "paramValues[0] : NULL" << std::endl;
    paramValues[1] ? std::cout << "paramValues[1] : " << std::string(paramValues[1]) << std::endl : std::cout << "paramValues[1] : NULL" << std::endl;
    paramValues[2] ? std::cout << "paramValues[2] : " << std::string(paramValues[2]) << std::endl : std::cout << "paramValues[2] : NULL" << std::endl;
    paramValues[3] ? std::cout << "paramValues[3] : " << std::string(paramValues[3]) << std::endl : std::cout << "paramValues[3] : NULL" << std::endl;
    paramValues[4] ? std::cout << "paramValues[4] : " << std::string(paramValues[4]) << std::endl : std::cout << "paramValues[4] : NULL" << std::endl;
    paramValues[5] ? std::cout << "paramValues[5] : " << std::string(paramValues[5]) << std::endl : std::cout << "paramValues[5] : NULL" << std::endl;
    paramValues[6] ? std::cout << "paramValues[6] : " << std::string(paramValues[6]) << std::endl : std::cout << "paramValues[6] : NULL" << std::endl;


    pgResult = PQexecParams(wdbPGConn_,
                       "select value::int4, valuetype, dataprovidername, placename, valueparametername, levelparametername, levelfrom::float4, levelto::float4, extract(epoch from referencetime)::float8 as referencetime, extract(epoch from validtimefrom)::float8 as validtimefrom, extract(epoch from validtimeto)::float8 as validtimeto, extract(epoch from storetime)::float8 as storetime from wci.read($1, $2, $3, $4, $5, $6, $7, NULL::wci.returngid) order by value desc, referencetime desc, validtimeto asc, levelfrom asc, storetime desc, validtimeindeterminatecode asc, levelindeterminatecode asc",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cout << __FUNCTION__ << " wci.read failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        gidRows.clear();
        return;
    }

    int record_count = PQntuples(pgResult);
    gidRows.resize(record_count);

    int value_fnum = PQfnumber(pgResult, "value");
    int valuetype_fnum = PQfnumber(pgResult, "valuetype");
    int referencetime_fnum = PQfnumber(pgResult, "referencetime");
    int dataprovidername_fnum = PQfnumber(pgResult, "dataprovidername");
    int placename_fnum = PQfnumber(pgResult, "placename");
    int valueparametername_fnum = PQfnumber(pgResult, "valueparametername");
    int levelparametername_fnum = PQfnumber(pgResult, "levelparametername");
    int levelfrom_fnum = PQfnumber(pgResult, "levelfrom");
    int levelto_fnum = PQfnumber(pgResult, "levelto");
    int validtimefrom_fnum = PQfnumber(pgResult, "validtimefrom");
    int validtimeto_fnum = PQfnumber(pgResult, "validtimeto");
    int storetime_fnum = PQfnumber(pgResult, "storetime");

    for (int i = 0; i < record_count; ++i)
    {
        GxGidRow row;
        /* Get the field values (ATM ignore possibility they are null!) */
        row.setValue(GxWdbExplorer::getValueAsInt4(pgResult, i, value_fnum));

        std::cout << __FUNCTION__ << "@" << __LINE__ << " :" << "Found GID = " << row.value() << std::endl;

        row.setValueType(GxWdbExplorer::getValueAsString(pgResult, i, valuetype_fnum));

        GxDataProviderRow providerRow;
        providerRow.setName(GxWdbExplorer::getValueAsString(pgResult, i, dataprovidername_fnum));
        row.setProvider(providerRow);

        GxPlaceRow placeRow;
        placeRow.setName(GxWdbExplorer::getValueAsString(pgResult, i, placename_fnum));
        row.setPlace(placeRow);

        GxTimeRow storeTimeRow;
        storeTimeRow.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, i, storetime_fnum));
        storeTimeRow.setUnitName("seconds since epoch");
        row.setStoreTime(storeTimeRow);

        GxReferenceTimeRow referenceTimeRow;
        referenceTimeRow.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, i, referencetime_fnum));
        referenceTimeRow.setUnitName("seconds since epoch");
        row.setReferenceTime(referenceTimeRow);

        GxValidTimeRow validTimeRow;
        GxTimeRow validTimeFromRow;
        validTimeFromRow.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, i, validtimefrom_fnum));
        validTimeFromRow.setUnitName("seconds since epoch");
        validTimeRow.setFrom(validTimeFromRow);
        GxTimeRow validTimeToRow;
        validTimeToRow.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, i, validtimeto_fnum));
        validTimeToRow.setUnitName("seconds since epoch");
        validTimeRow.setTo(validTimeToRow);
        row.setValidTime(validTimeRow);

        GxLevelParameterRow levelRow;
        levelRow.setName(GxWdbExplorer::getValueAsString(pgResult, i, levelparametername_fnum));
        levelRow.setFrom(GxWdbExplorer::getValueAsFloat4(pgResult, i, levelfrom_fnum));
        levelRow.setTo(GxWdbExplorer::getValueAsFloat4(pgResult, i, levelto_fnum));
        row.setLevelParameter(levelRow);

        GxValueParameterRow valueRow;
        valueRow.setName(GxWdbExplorer::getValueAsString(pgResult, i, valueparametername_fnum));
        row.setValueParameter(valueRow);

        gidRows[i] = row;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getDataProviders(const std::string& place,
                                     const std::string& referencetime,
                                     const std::string& validtime,
                                     const std::vector<std::string>& values,
                                     const std::string& level,
                                     const std::vector<std::string>& version,
                                     std::vector<GxDataProviderRow>& providerRows)
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !providers_.empty()) {
        providerRows.clear();
        providerRows.insert(providerRows.begin(), providers_.begin(), providers_.end());
        return;
    }

    // prepare data
    std::string strvalueparameters = "NULL";
    std::vector<std::string> tmp_valueparameters(values);
    if(!tmp_valueparameters.empty()) {
        strvalueparameters = "{";
        strvalueparameters.append(boost::algorithm::join(tmp_valueparameters, std::string(",")));
        strvalueparameters.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(version);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = 0; // query has to be invariant to this
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = referencetime.empty() ? 0 : referencetime.c_str();
    paramValues[3] = validtime.empty() ? 0 : validtime.c_str();
    paramValues[4] = values.empty() ? 0 : strvalueparameters.c_str();
    paramValues[5] = level.empty() ? 0 : level.c_str();
    paramValues[6] = version.empty() ? 0 : strdataversion.c_str();

    pgResult = PQexecParams(wdbPGConn_,
                       "select dataprovidername, numberoftuples::int4 from wci.browse($1, $2, $3, $4, $5, $6, $7, NULL::wci.browsedataprovider)",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << " wci.browse failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        providerRows.clear();
        return; // empty
    }

    int record_count = PQntuples(pgResult);
    providerRows.resize(record_count);

    int dataprovidername_fnum = PQfnumber(pgResult, "dataprovidername");
    int numberoftuples_fnum = PQfnumber(pgResult, "numberoftuples");

    for (int i = 0; i < record_count; ++i)
    {
        GxDataProviderRow row;
        row.setName(GxWdbExplorer::getValueAsString(pgResult, i, dataprovidername_fnum));
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(pgResult, i, numberoftuples_fnum));
        providerRows[i] = row;
//        std::cerr << row.dataprovidername_.c_str() << std::endl;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getPlaces(const std::vector<std::string>& providers,
                              const std::string& referencetime,
                              const std::string& validtime,
                              const std::vector<std::string>& values,
                              const std::string& level,
                              const std::vector<std::string>& versions,
                              std::vector<GxPlaceRow>& placeRows)
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !places_.empty()){
        placeRows.clear();
        placeRows.insert(placeRows.begin(), places_.begin(), places_.end());
        return;
    }

    // prepare data
    std::string strdataproviders = "NULL";
    std::vector<std::string> tmp_dataproviders(providers);
    if(!tmp_dataproviders.empty()) {
        strdataproviders = "{";
        strdataproviders.append(boost::algorithm::join(tmp_dataproviders, std::string(",")));
        strdataproviders.append("}");
    }

    std::string strvalueparameters = "NULL";
    std::vector<std::string> tmp_valueparameters(values);
    if(!tmp_valueparameters.empty()) {
        strvalueparameters = "{";
        strvalueparameters.append(boost::algorithm::join(tmp_valueparameters, std::string(",")));
        strvalueparameters.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(versions);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = providers.empty() ? 0 : strdataproviders.c_str();
    paramValues[1] = 0;
    paramValues[2] = referencetime.empty() ? 0 : referencetime.c_str();
    paramValues[3] = validtime.empty() ? 0 : validtime.c_str();
    paramValues[4] = values.empty() ? 0 : strvalueparameters.c_str();
    paramValues[5] = level.empty() ? 0 : level.c_str();
    paramValues[6] = versions.empty() ? 0 : strdataversion.c_str();

    pgResult = PQexecParams(wdbPGConn_,
                       "select placename, referencetimefrom, referencetimeto, numberoftuples::int4 from wci.browse($1, $2, $3, $4, $5, $6, $7, NULL::wci.browseplace)",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << " wci.browse failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        placeRows.clear();
        return; // empty
    }

    int record_count = PQntuples(pgResult);
    placeRows.resize(record_count);

    int placename_fnum = PQfnumber(pgResult, "placename");
    int numberoftuples_fnum = PQfnumber(pgResult, "numberoftuples");

    for (int i = 0; i < record_count; ++i)
    {
        GxPlaceRow row;
        row.setName(GxWdbExplorer::getValueAsString(pgResult, i, placename_fnum));
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(pgResult, i, numberoftuples_fnum));
        placeRows[i] = row;
//        std::cerr << row.placename_.c_str() << std::endl;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getValueParameters(const std::vector<std::string>& providers,
                                       const std::string& place,
                                       const std::string& referencetime,
                                       const std::string& validtime,
                                       const std::string& level,
                                       const std::vector<std::string>& versions,
                                       std::vector<GxValueParameterRow>& valueRows)
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !valueparameters_.empty()) {
        valueRows.clear();
        valueRows.insert(valueRows.begin(), valueRows.begin(), valueRows.end());
        return;
    }

    // prepare data
    std::string strdataproviders = "NULL";
    std::vector<std::string> tmp_dataproviders(providers);
    if(!tmp_dataproviders.empty()) {
        strdataproviders = "{";
        strdataproviders.append(boost::algorithm::join(tmp_dataproviders, std::string(",")));
        strdataproviders.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(versions);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = providers.empty() ? 0 : strdataproviders.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = referencetime.empty() ? 0 : referencetime.c_str();
    paramValues[3] = validtime.empty() ? 0 : validtime.c_str();
    paramValues[4] = 0;
    paramValues[5] = level.empty() ? 0 : level.c_str();
    paramValues[6] = versions.empty() ? 0 : strdataversion.c_str();

    std::cout << __FUNCTION__ << "@" << __LINE__ << std::endl;
    paramValues[0] ? std::cout << "paramValues[0] : " << std::string(paramValues[0]) << std::endl : std::cout << "paramValues[0] : NULL" << std::endl;
    paramValues[1] ? std::cout << "paramValues[1] : " << std::string(paramValues[1]) << std::endl : std::cout << "paramValues[1] : NULL" << std::endl;
    paramValues[2] ? std::cout << "paramValues[2] : " << std::string(paramValues[2]) << std::endl : std::cout << "paramValues[2] : NULL" << std::endl;
    paramValues[3] ? std::cout << "paramValues[3] : " << std::string(paramValues[3]) << std::endl : std::cout << "paramValues[3] : NULL" << std::endl;
    paramValues[4] ? std::cout << "paramValues[4] : " << std::string(paramValues[4]) << std::endl : std::cout << "paramValues[4] : NULL" << std::endl;
    paramValues[5] ? std::cout << "paramValues[5] : " << std::string(paramValues[5]) << std::endl : std::cout << "paramValues[5] : NULL" << std::endl;
    paramValues[6] ? std::cout << "paramValues[6] : " << std::string(paramValues[6]) << std::endl : std::cout << "paramValues[6] : NULL" << std::endl;

    pgResult = PQexecParams(wdbPGConn_,
                       "select valueparametername, valueunitname, numberoftuples::int4 from wci.browse($1, $2, $3, $4, $5, $6, $7, NULL::wci.browsevalueparameter)",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << "wci.browse failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        valueRows.clear();
        return; // empty
    }


    int record_count = PQntuples(pgResult);
    valueRows.resize(record_count);

    int valueparametername_fnum = PQfnumber(pgResult, "valueparametername");
    int valueunitname_fnum = PQfnumber(pgResult, "valueunitname");
    int numberoftuples_fnum = PQfnumber(pgResult, "numberoftuples");

    for (int i = 0; i < record_count; i++)
    {
        GxValueParameterRow row;
        row.setName(GxWdbExplorer::getValueAsString(pgResult, i, valueparametername_fnum));
        row.setUnitName(GxWdbExplorer::getValueAsString(pgResult, i, valueunitname_fnum));
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(pgResult, i, numberoftuples_fnum));
        valueRows[i] = row;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getLevelParameters(const std::vector<std::string>& providers,
                                       const std::string& place,
                                       const std::string& referencetime,
                                       const std::string& validtime,
                                       const std::vector<std::string>& values,
                                       const std::vector<std::string>& versions,
                                       std::vector<GxLevelParameterRow>& levelRows)
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !levelparameters_.empty()) {
        levelRows.clear();
        levelRows.insert(levelRows.begin(), levelparameters_.begin(), levelparameters_.end());
        return;
    }


    // prepare data
    std::string strdataproviders = "NULL";
    std::vector<std::string> tmp_dataproviders(providers);
    if(!tmp_dataproviders.empty()) {
        strdataproviders = "{";
        strdataproviders.append(boost::algorithm::join(tmp_dataproviders, std::string(",")));
        strdataproviders.append("}");
    }

    std::string strvalueparameters = "NULL";
    std::vector<std::string> tmp_valueparameters(values);
    if(!tmp_valueparameters.empty()) {
        strvalueparameters = "{";
        strvalueparameters.append(boost::algorithm::join(tmp_valueparameters, std::string(",")));
        strvalueparameters.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(versions);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = providers.empty() ? 0 : strdataproviders.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = referencetime.empty() ? 0 : referencetime.c_str();
    paramValues[3] = validtime.empty() ? 0 : validtime.c_str();
    paramValues[4] = values.empty() ? 0 : strvalueparameters.c_str();
    paramValues[5] = 0; // we are browsing for levels .. so do not need this one
    paramValues[6] = versions.empty() ? 0 : strdataversion.c_str();

    pgResult = PQexecParams(wdbPGConn_,
                       "select levelparametername, levelunitname, levelfrom::float4, levelto::float4, numberoftuples::int4 from wci.browse($1, $2, $3, $4, $5, $6, $7, NULL::wci.browselevelparameter)",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << " wci.read failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        levelRows.clear();
        return; // empty
    }

    int record_count = PQntuples(pgResult);
    levelRows.resize(record_count);

    int levelparametername_fnum = PQfnumber(pgResult, "levelparametername");
    int levelunitname_fnum = PQfnumber(pgResult, "levelunitname");
    int levelfrom_fnum = PQfnumber(pgResult, "levelfrom");
    int levelto_fnum = PQfnumber(pgResult, "levelto");
    int numberoftuples_fnum = PQfnumber(pgResult, "numberoftuples");

    for (int i = 0; i < record_count; i++)
    {
        GxLevelParameterRow row;
        row.setName(GxWdbExplorer::getValueAsString(pgResult, i, levelparametername_fnum));
        row.setUnitName(GxWdbExplorer::getValueAsString(pgResult, i, levelunitname_fnum));
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(pgResult, i, numberoftuples_fnum));
        row.setFrom(GxWdbExplorer::getValueAsFloat4(pgResult, i, levelfrom_fnum));
        row.setTo(GxWdbExplorer::getValueAsFloat4(pgResult, i, levelto_fnum));
        levelRows[i] = row;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getValidTimes(const std::vector<std::string>& providers,
                                  const std::string& place,
                                  const std::string& referencetime,
                                  const std::vector<std::string>& values,
                                  const std::string& level,
                                  const std::vector<std::string>& versions,
                                  std::vector<GxValidTimeRow>& validTimeRows)
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !validtimes_.empty()) {
        validTimeRows.clear();
        validTimeRows.insert(validTimeRows.begin(), validtimes_.begin(), validtimes_.end());
        return;
    }


    // prepare data
    std::string strdataproviders = "NULL";
    std::vector<std::string> tmp_dataproviders(providers);
    if(!tmp_dataproviders.empty()) {
        strdataproviders = "{";
        strdataproviders.append(boost::algorithm::join(tmp_dataproviders, std::string(",")));
        strdataproviders.append("}");
    }

    std::string strvalueparameters = "NULL";
    std::vector<std::string> tmp_valueparameters(values);
    if(!tmp_valueparameters.empty()) {
        strvalueparameters = "{";
        strvalueparameters.append(boost::algorithm::join(tmp_valueparameters, std::string(",")));
        strvalueparameters.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(versions);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = providers.empty() ? 0 : strdataproviders.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = referencetime.empty() ? 0 : referencetime.c_str();
    paramValues[3] = 0;
    paramValues[4] = values.empty() ? 0 : strvalueparameters.c_str();
    paramValues[5] = level.empty() ? 0 : level.c_str();
    paramValues[6] = versions.empty() ? 0 : strdataversion.c_str();

    pgResult = PQexecParams(wdbPGConn_,
                       "SELECT DISTINCT(validtimefrom)::float8, validtimeto::float8, numberoftuples FROM (select extract(epoch from validtimefrom) as validtimefrom, extract(epoch from validtimeto) as validtimeto, numberoftuples as numberoftuples from wci.browse($1, $2, $3, $4, $5, $6, $7, NULL::wci.browsevalidtime) where validtimefrom != validtimeto order by validtimefrom) AS VALIDTIMES",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << " wci.browse failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        validTimeRows.clear();
        return; // empty
    }

    int record_count = PQntuples(pgResult);
    validTimeRows.resize(record_count);

    int validtimefrom_fnum = PQfnumber(pgResult, "validtimefrom");
    int validtimeto_fnum = PQfnumber(pgResult, "validtimeto");
    int numberoftuples_fnum = PQfnumber(pgResult, "numberoftuples");

    for (int i = 0; i < record_count; i++)
    {
        GxValidTimeRow row;
        GxTimeRow from;
        from.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, i, validtimefrom_fnum));
        from.setUnitName("seconds since epoch");
        row.setFrom(from);
        GxTimeRow to;
        to.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, i, validtimeto_fnum));
        to.setUnitName("seconds since epoch");
        row.setTo(to);
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(pgResult, i, numberoftuples_fnum));
        validTimeRows[i] = row;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getReferenceTimes
        (
                const std::vector<std::string>& providers,
                const std::string& place,
                const std::string& validtime,
                const std::vector<std::string>& values,
                const std::string& level,
                const std::vector<std::string>& versions,
                std::vector<GxReferenceTimeRow>& referenceTimeRows
        )
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !referencetimes_.empty()) {
        referenceTimeRows.clear();
        referenceTimeRows.insert(referenceTimeRows.begin(), referencetimes_.begin(), referencetimes_.end());
        return;
    }

    // prepare data
    std::string strdataproviders = "NULL";
    std::vector<std::string> tmp_dataproviders(providers);
    if(!tmp_dataproviders.empty()) {
        strdataproviders = "{";
        strdataproviders.append(boost::algorithm::join(tmp_dataproviders, std::string(",")));
        strdataproviders.append("}");
    }

    std::string strvalueparameters = "NULL";
    std::vector<std::string> tmp_valueparameters(values);
    if(!tmp_valueparameters.empty()) {
        strvalueparameters = "{";
        strvalueparameters.append(boost::algorithm::join(tmp_valueparameters, std::string(",")));
        strvalueparameters.append("}");
    }

    std::string strdataversion = "NULL";
    std::vector<std::string> tmp_dataversion(versions);
    if(!tmp_dataversion.empty()) {
        strdataversion = "{";
        strdataversion.append(boost::algorithm::join(tmp_dataversion, std::string(",")));
        strdataversion.append("}");
    }

    // execute the query
    PGresult *pgResult;
    const char *paramValues[7];

    paramValues[0] = providers.empty() ? 0 : strdataproviders.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = 0;
    paramValues[3] = validtime.empty() ? 0 : validtime.c_str();
    paramValues[4] = values.empty() ? 0 : strvalueparameters.c_str();
    paramValues[5] = level.empty() ? 0 : level.c_str();
    paramValues[6] = versions.empty() ? 0 : strdataversion.c_str();

    std::cout << __FUNCTION__ << "@" << __LINE__ << std::endl;
    paramValues[0] ? std::cout << "paramValues[0] : " << std::string(paramValues[0]) << std::endl : std::cout << "paramValues[0] : NULL" << std::endl;
    paramValues[1] ? std::cout << "paramValues[1] : " << std::string(paramValues[1]) << std::endl : std::cout << "paramValues[1] : NULL" << std::endl;
    paramValues[2] ? std::cout << "paramValues[2] : " << std::string(paramValues[2]) << std::endl : std::cout << "paramValues[2] : NULL" << std::endl;
    paramValues[3] ? std::cout << "paramValues[3] : " << std::string(paramValues[3]) << std::endl : std::cout << "paramValues[3] : NULL" << std::endl;
    paramValues[4] ? std::cout << "paramValues[4] : " << std::string(paramValues[4]) << std::endl : std::cout << "paramValues[4] : NULL" << std::endl;
    paramValues[5] ? std::cout << "paramValues[5] : " << std::string(paramValues[5]) << std::endl : std::cout << "paramValues[5] : NULL" << std::endl;
    paramValues[6] ? std::cout << "paramValues[6] : " << std::string(paramValues[6]) << std::endl : std::cout << "paramValues[6] : NULL" << std::endl;

    pgResult = PQexecParams(wdbPGConn_,
                       "select DISTINCT(extract(epoch from referencetime)) as referencetime, numberoftuples from wci.browse($1, $2, $3, $4, $5, $6, $7, NULL::wci.browsereferencetime) order by referencetime",
                       7,
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << " wci.browse failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        referenceTimeRows.clear();
        return; // empty
    }

    int record_count = PQntuples(pgResult);
    std::cout << __FUNCTION__ << "@" << __LINE__ << " record_count "<< record_count << std::endl;
    referenceTimeRows.resize(record_count);

    int referencetime_fnum = PQfnumber(pgResult, "referencetime");
    int numberoftuples_fnum = PQfnumber(pgResult, "numberoftuples");

    for (int rowIndex = 0; rowIndex < record_count; rowIndex++)
    {
        GxReferenceTimeRow row;
        row.setSinceEpochInSeconds(GxWdbExplorer::getValueAsFloat8(pgResult, rowIndex, referencetime_fnum));
        row.setUnitName("seconds since epoch");
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(pgResult, rowIndex, numberoftuples_fnum));
        referenceTimeRows[rowIndex] = row;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getGridDataAsFimexData(const std::string& gid, const std::string& strdatatype, boost::shared_ptr<MetNoFimex::Data>& data)
{
    PGresult   *pgResult;
    const char *paramValues[1];

    paramValues[0] = gid.c_str();

//    qDebug() << "SELECT numberx::int4, numbery::int4, projdefinition::text, grid::bytea FROM wci.fetch("<< gid.c_str() <<", NULL::wci.grid)";

    pgResult = PQexecParams(wdbPGConn_,
                       "SELECT numberx::int4, numbery::int4, grid::bytea FROM wci.fetch($1, NULL::wci.grid)",
                       1,       /* one param */
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << "wci.fetch failed: " << PQerrorMessage(wdbPGConn_);
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        data = boost::shared_ptr<MetNoFimex::Data>();
        return;
    }

    int grid_fnum = PQfnumber(pgResult, "grid");
    int numberx_fnum = PQfnumber(pgResult, "numberx");
    int numbery_fnum = PQfnumber(pgResult, "numbery");

    int    row_position = 0;
    int    blen;
    float* ptrbinary;

    /* Get the field values (we ignore possibility they are null!) */
    int numberX = GxWdbExplorer::getValueAsInt4(pgResult, row_position, numberx_fnum);
    int numberY = GxWdbExplorer::getValueAsInt4(pgResult, row_position, numbery_fnum);
    ptrbinary = reinterpret_cast<float *>(PQgetvalue(pgResult, row_position, grid_fnum));

    blen = PQgetlength(pgResult, row_position, grid_fnum) / sizeof(float);

//    boost::shared_ptr<MetNoFimex::Data> tmpdata =
//            MetNoFimex::createData(MetNoFimex::string2datatype(strdatatype), ptrbinary, ptrbinary + blen);

//    for(int index = 0; index < blen; ++index) {
//        std::cerr << *(ptrbinary + index) << "    ";
//    }

//    data.swap(tmpdata);

    data = MetNoFimex::createData(MetNoFimex::string2datatype(strdatatype), ptrbinary, ptrbinary + blen);

    std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
              << "\tnumberX = "   << numberX
              << "\tnumberY = "   << numberY
              << "\tblen = "      << blen
              << "\tdata size = " << data->size()
              << std::endl;

    PQclear(pgResult);
}

void GxWdbExplorer::getGridData(const std::string& gid, GxGridDataRow& dataRow)
{
    PGresult *pgResult;
    const char *paramValues[1];

    paramValues[0] = gid.c_str();

    pgResult = PQexecParams(wdbPGConn_,
                       "SELECT numberx::float4, numbery::float4, grid::bytea FROM wci.fetch($1, NULL::wci.grid)",
                       1,       /* one param */
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << "wci.fetch failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        dataRow = GxGridDataRow();
        return;
    }

    int grid_fnum = PQfnumber(pgResult, "grid");
    int numberx_fnum = PQfnumber(pgResult, "numberx");
    int numbery_fnum = PQfnumber(pgResult, "numbery");

    int     i = 0;
    char*   ptrbinary;
    float*  ptrdata;
    int     blen;

    ptrbinary = PQgetvalue(pgResult, i, grid_fnum);
    blen = PQgetlength(pgResult, i, grid_fnum);
    std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << "blen = " << blen << std::endl;

    ptrdata = reinterpret_cast<float*>(ptrbinary);
    boost::shared_ptr<std::vector<float> > tmpData =
            boost::shared_ptr<std::vector<float> >(new std::vector<float>(ptrdata, ptrdata + (blen/sizeof(float))));

    dataRow.setData(tmpData);
    dataRow.setNumberX(GxWdbExplorer::getValueAsFloat4(pgResult, i, numberx_fnum));
    dataRow.setNumberY(GxWdbExplorer::getValueAsFloat4(pgResult, i, numbery_fnum));

    PQclear(pgResult);
}

void GxWdbExplorer::getLevelParameterFromToPairs(const std::string& provider,
                                                 const std::string& place,
                                                 const std::string& level,
                                                 std::vector<std::pair<double, double> >& levelPairs)
{
    assert( !(provider.empty() || place.empty() || level.empty()) );

    std::string strdataprovider = std::string("{").append(provider).append("}");

    const char *paramValues[3];

    paramValues[0] = provider.empty() ? 0 : strdataprovider.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = level.empty() ? 0 : level.c_str();

    PGresult *pgResult;
    pgResult = PQexecParams(wdbPGConn_,
                       "select DISTINCT(levelfrom)::float8, levelto::float8, levelparametername from wci.read($1, $2, NULL, NULL, NULL, NULL, NULL, NULL::wci.returngid) where levelparametername = $3 order by levelfrom",
                       3,       /* three param */
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(pgResult) != PGRES_TUPLES_OK)
    {
        std::cerr << __FUNCTION__ << " wci.read failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(pgResult);
        PQfinish(wdbPGConn_);
        levelPairs.clear();
        return; // empty
    }

    int levelfrom_fnum = PQfnumber(pgResult, "levelfrom");
    int levelto_fnum = PQfnumber(pgResult, "levelto");

    int record_count = PQntuples(pgResult);
    levelPairs.resize(record_count);

    for (int i = 0; i < record_count; i++)
    {
        std::pair<double, double> pair;
        pair.first = GxWdbExplorer::getValueAsFloat8(pgResult, i, levelfrom_fnum);
        pair.second = GxWdbExplorer::getValueAsFloat8(pgResult, i, levelto_fnum);
        levelPairs[i] = pair;
    }

    PQclear(pgResult);
}

void GxWdbExplorer::getLevelParametersForValueParameter(const std::string& provider,
                                         const std::string& place,
                                         const std::string& valueParameterName,
                                         std::vector<GxLevelParameterRow>& levelRows)
{
    if(cachePolicy_ == GxWdbCachePolicy(GxWdbCachePolicy::Cache) && !levelparameters_.empty()) {
        levelRows.clear();
        levelRows.insert(levelRows.begin(), levelparameters_.begin(), levelparameters_.end());
    }

//    assert( !(dataprovider.empty() || place.empty() || valueparameter.empty()) );

    // prepare data
    std::string strdataprovider = std::string("{").append(provider).append("}");
    std::string strvalueparameter = std::string("{").append(valueParameterName).append("}");

    const char *paramValues[3];

    paramValues[0] = provider.empty() ? 0 : strdataprovider.c_str();
    paramValues[1] = place.empty() ? 0 : place.c_str();
    paramValues[2] = valueParameterName.empty() ? 0 : strvalueparameter.c_str();

    PGresult *res;
    res = PQexecParams(wdbPGConn_,
                       "select levelparametername, levelunitname, levelfrom::float8, levelto::float8, numberoftuples::int4 from wci.browse($1, $2, NULL, NULL, $3, NULL, ARRAY[-1], NULL::wci.browselevelparameter)",
                       3,       /* three param */
                       NULL,    /* let the backend deduce param type */
                       paramValues,
                       NULL,    /* don't need param lengths since text */
                       NULL,    /* default to all text params */
                       1);      /* ask for binary results */

    if (PQresultStatus(res) != PGRES_TUPLES_OK)
    {
        std::cerr << "wci.browse failed: " << PQerrorMessage(wdbPGConn_) << std::endl;
        PQclear(res);
        PQfinish(wdbPGConn_);
        levelRows.clear();
        return; // empty
    }

    int record_count = PQntuples(res);
    levelRows.resize(record_count);

    int levelparametername_fnum = PQfnumber(res, "levelparametername");
    int levelunitname_fnum = PQfnumber(res, "levelunitname");
    int levelfrom_fnum = PQfnumber(res, "levelfrom");
    int levelto_fnum = PQfnumber(res, "levelto");
    int numberoftuples_fnum = PQfnumber(res, "numberoftuples");

    for (int i = 0; i < record_count; i++)
    {
        GxLevelParameterRow row;
        row.setName(GxWdbExplorer::getValueAsString(res, i, levelparametername_fnum));
        row.setUnitName(GxWdbExplorer::getValueAsString(res, i, levelunitname_fnum));
        row.setFrom(GxWdbExplorer::getValueAsFloat8(res, i, levelfrom_fnum));
        row.setTo(GxWdbExplorer::getValueAsFloat8(res, i, levelto_fnum));
        row.setNumberOfTuples(GxWdbExplorer::getValueAsInt4(res, i, numberoftuples_fnum));
        levelRows[i] = row;
    }

    PQclear(res);
}
