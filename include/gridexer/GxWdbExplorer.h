#ifndef GXWDBEXPLORER_H
#define GXWDBEXPLORER_H

#include "gridexer/GxWdbDataTypes.h"

#include "fimex/Data.h"

//boots
#include <boost/shared_array.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

//pqlib C-library
#include <postgresql/libpq-fe.h>

//standard
#include <string>
#include <vector>

class GxWdbCachePolicy
{
public:
    enum Policy {
        NoCache = 0,
        Cache
    };

    GxWdbCachePolicy(Policy cachePolicy = NoCache)
        : data(cachePolicy) { }

    bool operator==(const GxWdbCachePolicy& s) const { return data == s.data; }

private:
    unsigned int data;
};

class GxWdbExplorer
{
public:
    explicit GxWdbExplorer();
    explicit GxWdbExplorer(const std::string& dbHost, const std::string& dbName, const std::string& dbUser, const unsigned dbPort = 5432);
    ~GxWdbExplorer();

    bool init();
    bool deinit();

    void setDbHost(const std::string& dbHost);
    void setDbName(const std::string& dbName);
    void setDbUser(const std::string& dbUser);
    void setDbPort(const unsigned int dbPort);

    std::string  dbHost() const;
    std::string  dbHost();
    std::string  dbName() const;
    std::string  dbName();
    std::string  dbUser() const;
    std::string  dbUser();
    unsigned int dbPort() const;
    unsigned int dbPort();
    std::string  wciUser() const;
    std::string  wciUser();
    std::string  connectString() const;
    std::string  connectString();

    void clearCache();

    // fetch from wdb
    //
    std::string  wdbVersion();

    // get grid description for given place
    // (x, y, increments ...)
    //
    void getGridDescription(const std::string& placename, std::vector<GxPlaceRegularGridRow>& gridDescriptionRows);

    void getDataProviders(const std::string& place,
                          const std::string& referencetime,
                          const std::string& validtime,
                          const std::vector<std::string>& values,
                          const std::string& level,
                          const std::vector<std::string>& version,
                          std::vector<GxDataProviderRow>& providerRows);

    void getPlaces(const std::vector<std::string>& providers,
                   const std::string& referencetime,
                   const std::string& validtime,
                   const std::vector<std::string>& values,
                   const std::string& level,
                   const std::vector<std::string>& versions,
                   std::vector<GxPlaceRow>& placeRows);

    void getValueParameters(const std::vector<std::string>& providers,
                            const std::string& place,
                            const std::string& referencetime,
                            const std::string& validtime,
                            const std::string& level,
                            const std::vector<std::string>& versions,
                            std::vector<GxValueParameterRow>& valueRows);

    void getLevelParameters(const std::vector<std::string>& providers,
                            const std::string& place,
                            const std::string& referencetime,
                            const std::string& validtime,
                            const std::vector<std::string>& values,
                            const std::vector<std::string>& versions,
                            std::vector<GxLevelParameterRow>& levelRows);

    void getLevelParameterFromToPairs(const std::string& provider,
                                      const std::string& place,
                                      const std::string& level,
                                      std::vector<std::pair<double, double> >& levelPairs);

    void getLevelParametersForValueParameter(const std::string& provider,
                                             const std::string& place,
                                             const std::string& valueParameterName,
                                             std::vector<GxLevelParameterRow>& levelRows);

    void getValidTimes(const std::vector<std::string>& providers,
                       const std::string& place,
                       const std::string& referencetime,
                       const std::vector<std::string>& values,
                       const std::string& level,
                       const std::vector<std::string>& versions,
                       std::vector<GxValidTimeRow>& validTimeRows);

    void getGids(const std::vector<std::string>& providers,
                 const std::string& place,
                 const std::string& referenceTime,
                 const std::string& validTime,
                 const std::vector<std::string>& values,
                 const std::string& level,
                 const std::vector<std::string>& version,
                 std::vector<GxGidRow>& gidRows);

    void getGridDataAsFimexData(const std::string& gid,
                                const std::string& datatype,
                                boost::shared_ptr<MetNoFimex::Data>& data);

    void getGridData(const std::string& gid, GxGridDataRow& dataRow);

protected:
    bool connectToWdb();
    static void getValueAsString(PGresult* pgResult, const int rowPosition, const int columnPosition, std::string& stringHolder);
    static std::string getValueAsString(PGresult* pgResult, const int rowPosition, const int columnPosition);
    static void getValueAsInt4(PGresult* pgResult, const int rowPosition, const int columnPosition, int& intHolder);
    static int getValueAsInt4(PGresult* pgResult, const int rowPosition, const int columnPosition);
    static void getValueAsInt8(PGresult* pgResult, const int rowPosition, const int columnPosition, long long& longLongHolder);
    static long long getValueAsInt8(PGresult* pgResult, const int rowPosition, const int columnPosition);
    static void getValueAsFloat4(PGresult* pgResult, const int rowPosition, const int columnPosition, float& floatHolder);
    static float getValueAsFloat4(PGresult* pgResult, const int rowPosition, const int columnPosition);
    static void getValueAsFloat8(PGresult* pgResult, const int rowPosition, const int columnPosition, double& doubleHolder);
    static double getValueAsFloat8(PGresult* pgResult, const int rowPosition, const int columnPosition);

private:
    std::string         dbHost_;
    std::string         dbName_;
    std::string         dbUser_; // ATM wciUser effectively
    unsigned int        dbPort_;
    std::string         wdbVersion_;

    PGconn*             wdbPGConn_;

    // cached data  --
    // ATM primitive version
    //
    GxWdbCachePolicy                 cachePolicy_;
    std::vector<GxDataProviderRow>   providers_;
    std::vector<GxPlaceRow>          places_;
    std::vector<GxValidTimeRow>      validtimes_;
    std::vector<GxLevelParameterRow> levelparameters_;
    std::vector<GxValueParameterRow> valueparameters_;
    std::vector<GxGidRow>            gids_;
};

#endif // GXWDBEXPLORER_H
