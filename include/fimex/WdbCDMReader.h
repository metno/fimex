#ifndef WDBCDMREADER_H
#define WDBCDMREADER_H

#include "gridexer/GxWdbDataTypes.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMDimension.h"

// standard
//
#include <string>
#include <vector>

// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

class GxWdbExplorer;

namespace MetNoFimex {

    class GxWdbCDMReader : public CDMReader
    {
    public:

        explicit GxWdbCDMReader(const std::string& source, const std::string& configfilename);

        void init() throw(CDMException);
        bool deinit();

        void setDbHost(const std::string& dbHost);
        void setDbName(const std::string& dbName);
        void setDbUser(const std::string& dbUser);
        void setDbPort(const unsigned int dbPort);

        const std::string & dbHost() const;
        const std::string & dbName() const;
        const std::string & dbUser() const;
        unsigned int dbPort() const;
        const std::string & wciUser() const;
        std::string connectString() const;

//        void setWdbToCFNamesMap(const std::map<std::string, std::string>& wdb2cfmap);
//        void addWdbToCFNames(const stdmap<std::string, std::string>& wdbmap);
        void addWdbNameToCFName(const std::string& wdbname, const std::string& cfname);

//        void addWdbNameToFillValueMap(const boost::bimap<std::string, double>& map);
        void addWdbNameToFillValue(const std::string& wdbname, const double value);

        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException);
//        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb) throw(CDMException);

    protected:
        boost::shared_ptr<GxWdbExplorer> wdbExplorer()  {
            return wdbExplorer_;
        }

        const boost::shared_ptr<GxWdbExplorer> & wdbExplorer() const {
            return wdbExplorer_;
        }

        void setWdbExplorer(const boost::shared_ptr<GxWdbExplorer>& wdbExplorer) {
            wdbExplorer_ = wdbExplorer;
        }

        bool addDataProvider();
        bool addPlace();
        void addGlobalCDMAttributes();
        CDMDimension addTimeDimension();
        void addReferenceTimeVariable();
        CDMDimension addReferenceTimeDimension();
        std::map<short, CDMDimension> addLevelDimensions();
        std::string getStandardNameForDimension(const std::string& name);
        // returning projName and coordinates for given place name
        boost::tuple<std::string, std::string> addProjection(const std::string& strplace);

        void addVariables
                (
                        const std::string& projName,
                        const std::string& coordinates,
                        const CDMDimension& timeDim,
                        /*const CDMDimension& referenceTimDim,*/
                        const std::map<short, CDMDimension>& levelDim
                );

    private:
        std::string                         source_;
        std::string                         configFileName_;
        boost::shared_ptr<GxWdbExplorer>    wdbExplorer_;

        // data to build the reader on
        // todo: use smart pointers
        CDMDimension xDim;
        CDMDimension yDim;

        std::map<std::string, std::string> wdb2cfnamesmap_;
        std::map<std::string, std::string> cf2wdbnamesmap_;
        std::map<std::string, double> wdbname2fillvaluemap_;

        std::vector<GxDataProviderRow> providers_; // we support only one ATM
        std::vector<GxPlaceRow> places_; // we support only one ATM
        std::vector<GxLevelParameterRow> levelparameters_;
        std::vector<GxValueParameterRow> valueparameters_;
        std::vector<GxValidTimeRow> validtimes_; // this should be UNLIMITED dimension
        std::vector<GxReferenceTimeRow> referencetimes_;
        std::vector<std::pair<boost::posix_time::ptime, boost::posix_time::ptime> > timeVec;
        std::vector<boost::posix_time::ptime > referenceTimeVec;
        std::map<std::string, std::vector<std::pair<double, double> > > levelNamesToPairsMap;
    };

} // end namespace

#endif // WDBCDMREADER_H
