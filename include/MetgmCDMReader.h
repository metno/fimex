#ifndef METGM_CDM_READER_H
#define METGM_CDM_READER_H

#include "metgm.h"


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
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/composite_key.hpp>


/**
  * TODO:
       1. DONE ----- see if we are dealing with time axis properly
          should we calculate
                startTime+1*td; startTime+2*td;
                or
                startTime+0*td; startTime+1*td;

Clarification for dt. If the METGM has only one time slice, dt is the
number of seconds the data are valid for from the YS,MS,DS,hS,mS
DTG provided. Where a number of time slice volumes are provided,
all slices are separated by dt seconds.

          Have in mind that dataslice is 0 indexed
          so
          startTime+0*td; startTime+1*td; shoul be used

       2. DONE ----- am I using properly long_0 and lat_0 (centers of grid)

       3. parameters can have different vertical profile -- make it properly

       4. DONE ----- when we have MSL .. should we add it to the vertical profiles
          Nope, no adding

       5. DONE ----- how to handle unitless parameters (what is the unit -- now is "1")
          valueunitname should be "ratio"

       6. one of the winds parameters has to be multiplied by -1
          maybe to have slope as -1?

       7. DONE ----- introduce confoguration file parsing (pids, cfnames, fillvalues ...)

       8. cacheing of data as data is read at once for all points in time

       9. DONE ----- SliceBuilder

       10. use comment to set all metadata as well (start, analysis time, country)
  */

namespace MetNoFimex {
    /* forward declarations */
    class XMLDoc;

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

    struct phonebook_entry
    {
      std::string family_name;
      std::string given_name;
      std::string phone_number;

      phonebook_entry(
        std::string family_name,
        std::string given_name,
        std::string phone_number):
        family_name(family_name),given_name(given_name),phone_number(phone_number)
      {}
    };

    class METGM_CDMReader : public CDMReader
    {
    public:

        explicit METGM_CDMReader(const std::string& metgmsource, const std::string& configfilename);
        ~METGM_CDMReader();

        void init() throw(CDMException);
        bool deinit();

        mgm_version getMetgmVersion();
        std::string getMetgmVersionAsString();

        int getPidForMetgmName(const std::string& metgm_name);
        int getPidForCdmName(const std::string& cdm_name);

        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException);
        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb) throw(CDMException);

    protected:

        FILE* openMetgmFileHandle();
        FILE* resetMetgmFileHandle();

        std::string dataTypeToString(short data_type);
        mgm_handle* openMetgmHandle();
        mgm_handle* resetMetgmHandle();
        int         readMetgmHeader();

        /**
          * read metgm version as part of init process
          * not public as we will read from file
          */
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

        void addVariables
                (
                        const std::string& projName,
                        const std::string& coordinates,
                        const CDMDimension& timeDim //,
                        /*const CDMDimension& referenceTimDim,*/
//                        const std::map<short, CDMDimension>& levelDim
//                        const CDMDimension& levelDim
                );

    private:
        std::string                         metgmSource_;
        FILE*                               metgmFileHandle_;
        fpos_t                              metgmFileHandleStartPos_;
        mgm_handle*                         metgmHandle_;
        std::string                         configFileName_;
        mgm_version                         metgmVersion_;
//
        // data to build the reader on
        // todo: use smart pointers

        std::map<int, std::string>    pid2metnonamesmap_;
        std::map<int, std::string>    pid2cdmnamesmap_;
        std::map<int, double>         pid2fillvaluemap_;
        std::map<std::string, mgm_group3*> cdmvariable2mgm_group3map_;
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

#endif // METGM_CDM_READER_H

