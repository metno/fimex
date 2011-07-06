#ifndef METGM_CDM_READERIMPL_H
#define METGM_CDM_READERIMPL_H

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMDimension.h"

// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/composite_key.hpp>

// standard
//
#include <cstdio>
#include <string>
#include <vector>


/**
  * TODO:

       1. parameters can have different vertical profile -- make it properly

       2. one of the winds parameters has to be multiplied by -1
          maybe to have slope as -1?

  */

namespace MetNoFimex {

    /* forward declarations */
    class XMLDoc;
    class MetGmVersion;
    class MetGmHandlePtr;
    class MetGmGroup3Ptr;
    class MetGmFileHandlePtr;

    struct METGM_ZProfile
    {
      std::string name_;
      short       pr_;
      int         pid_;

      explicit METGM_ZProfile()
          :name_(std::string()), pr_(0), pid_(0) {}

      explicit METGM_ZProfile(const std::string& name , const short pr, const short pid)
          :name_(name), pr_(pr), pid_(pid) {}

      bool isValid() {
          return !name_.empty();
      }

      bool operator<(const METGM_ZProfile& profile)const{return name_ < profile.name_;}

    };

    class MetGmCDMReaderImpl : public CDMReader
    {
    public:

        explicit MetGmCDMReaderImpl(const std::string& metgmsource, const std::string& configfilename, const boost::shared_ptr<CDM>& cdm);
        ~MetGmCDMReaderImpl();

        void init() throw(CDMException);
        bool deinit();

        int getPidForMetgmName(const std::string& metgm_name);
        int getPidForCdmName(const std::string& cdm_name);

        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);
        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

    protected:

        std::string dataTypeToString(short data_type);
        void readMetgmHeader();
        void readMetgmVersion();

        /**
          * Fimex is having issues with names that have space
          */
        std::string spaceToUnderscore(const std::string& text);

        /**
          * only pid 0, 2, ... 7 are important for artilery
          */
        void fillPidToMetNoNameMap(const std::auto_ptr<XMLDoc>& doc);
        void fillPidToCdmNameMap(const std::auto_ptr<XMLDoc>& doc);
        void fillPidToFillValueMap(const std::auto_ptr<XMLDoc>& doc);

        void addGlobalCDMAttributes();

        CDMDimension addTimeDimension();
        CDMDimension addUniqueForecastReferenceTime();
        void addLevelDimensions();

        // returning projName and coordinates for given place name
        boost::tuple<std::string, std::string> addProjection();

        void addVariables(const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim);

    private:
        std::string                            configFileName_;
        boost::shared_ptr<MetGmVersion>        metgmVersion_;
        boost::shared_ptr<MetGmHandlePtr>      metgmHandle_;
        boost::shared_ptr<MetGmFileHandlePtr>  metgmFileHandle_;

        // data to build the reader on
        // todo: use smart pointers

        std::map<int, std::string>            pid2metnonamesmap_;
        std::map<int, std::string>            pid2cdmnamesmap_;
        std::map<int, double>                 pid2fillvaluemap_;
        std::map<std::string, boost::shared_ptr<MetGmGroup3Ptr> > cdmvariable2mgm_group3map_;
        typedef boost::multi_index::multi_index_container<
          METGM_ZProfile,
          boost::multi_index::indexed_by<
              boost::multi_index::ordered_non_unique <
                  boost::multi_index::composite_key <
                      METGM_ZProfile,
                      boost::multi_index::member<METGM_ZProfile,short,&METGM_ZProfile::pr_>,
                      boost::multi_index::member<METGM_ZProfile,int,&METGM_ZProfile::pid_>
                  >
              >
              >
          > metgm_profile_set;


        metgm_profile_set prXpidXname_;
        /**
          * cache some dimensions
          */
        CDMDimension xDim_;
        CDMDimension yDim_;
        std::vector<boost::posix_time::ptime> timeVec_;
    };

} // end namespace

#endif // METGM_CDM_READERIMPL_H

