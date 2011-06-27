#include "../../include/metgm/MetGmCDMWriterImpl.h"

#include "metgm.h"

// fimex
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/CDMReaderUtils.h"

// private/implementation code
#include "../../include/metgm/MetGmVersion.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"
#include "../../include/metgm/MetGmDimensionalTag.h"

// udunits
#include <udunits2.h>

// libxml2
#include <libxml/tree.h>
#include <libxml/xpath.h>

// boost
#include <boost/shared_array.hpp>
#include <boost/scoped_array.hpp>
#include <boost/bind.hpp>

// standard
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <auto_ptr.h>
#include <map>
#include <set>
#include <deque>
#include <numeric>
#include <limits>

namespace MetNoFimex {

#define FREE_TEXT "metgm_free_text"
#define VERSION   "metgm_version"
#define ANALYSIS_DATE_TIME "metgm_analysis_date_time"
#define START_DATE_TIME "metgm_start_date_time"
#define DATA_TYPE "metgm_data_type"
#define MODEL_TYPE "metgm_model_type"
#define PRODUCTION_NATION "metgm_production_nation"

    /**
      * static mgm_ wrapper functions
      */
    static int fimex_mgm_write_group3(FILE* fh, mgm_handle* mh, const mgm_group3* gp3)
    {
        short int callResult = mgm_write_group3(fh, mh, gp3);
        if(callResult != MGM_OK)
            throw CDMException(mgm_string_error(callResult));
        else
            return callResult; // that is MGM_OK
    }

    static void fimex_dump_group3(const mgm_group3* gp3)
    {
//        std::cerr << "dumping group3 [START]" << std::endl
//                  << "p_id = " << mgm_get_p_id (gp3) << std::endl
//                  << "nz = "   << mgm_get_nz (gp3)  << std::endl
//                  << "nx = "   << mgm_get_nx (gp3)  << std::endl
//                  << "ny = "   << mgm_get_ny (gp3)  << std::endl
//                  << "nt = "   << mgm_get_nt (gp3)  << std::endl
//                  << "dx = "   << mgm_get_dx (gp3)  << std::endl
//                  << "dy = "   << mgm_get_dy (gp3)  << std::endl
//                  << "dt = "   << mgm_get_dt (gp3)  << std::endl
//                  << "cx = "   << mgm_get_cx (gp3)  << std::endl
//                  << "cy = "   << mgm_get_cy (gp3)  << std::endl
//                  << "pr = "   << mgm_get_pr (gp3)  << std::endl
//                  << "pz = "   << mgm_get_pz (gp3)  << std::endl
//                  << "dumping group3 [END]" << std::endl;
    }

    void MetGmCDMWriterImpl::mapKildeVariablesToMetgmPids(const std::auto_ptr<XMLDoc>& doc)
    {
        const CDM& cdmRef = cdmReader->getCDM();

        /**
          * it is really expected that config will be used
          */
        if(doc.get() != 0) {
            XPathObjPtr xpathObj = doc->getXPathObject("/metgm/variable");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            size_t size = (nodes) ? nodes->nodeNr : 0;
            for (size_t i = 0; i < size; ++i) {
                    xmlNodePtr node = nodes->nodeTab[i];
                    std::string metnoName = getXmlProp(node, "name");
                    // get the metgm p_id value
                    XPathObjPtr xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+metnoName+"\"]/attribute[@name=\"metgm_p_id\"]");
                    std::string str_metgm_p_id;
                    if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                            str_metgm_p_id = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                            if(str_metgm_p_id == std::string("")) {
                                continue;
                            } else {
                                size_t metgm_p_id = boost::lexical_cast<size_t>(str_metgm_p_id);
                                pid2kildemap_.insert(std::make_pair<short, std::string>(metgm_p_id, metnoName));
//                                pidToExportVector_.push_back(metgm_p_id);
                            }
                    }
            }
        } else {
            /**
              * some arbitrary default values
              */
            pid2kildemap_.insert(std::pair<int, std::string>(0, "topography"));
            /**
              * this component is given as north to south so the values will be multiplied -1
              * to reflect standard name
              */
            pid2kildemap_.insert(std::pair<int, std::string>(2, std::string("horizontal_wind_speed_from_west_to_east")));

            pid2kildemap_.insert(std::pair<int, std::string>(3, std::string("horizontal_wind_speed_from_north_to_south")));

            pid2kildemap_.insert(std::pair<int, std::string>(4, std::string("vertical_wind_speed")));
            pid2kildemap_.insert(std::pair<int, std::string>(5, std::string("air_temperature")));
            pid2kildemap_.insert(std::pair<int, std::string>(6, std::string("relative_humidity")));

            /**
              * check units to differentiate between pressure and geopotential height
              */
            if(cdmRef.hasVariable("pressure"))
                pid2kildemap_.insert(std::pair<int, std::string>(7, std::string("pressure")));
            else if(cdmRef.hasVariable("geopotential_height"))
                pid2kildemap_.insert(std::pair<int, std::string>(7, std::string("geopotential_height")));

            for(int p_id = 0; p_id < 8; ++p_id) {
                /**
                  * this should also reflect the order
                  * in which we want to have parameters
                  */
//                pidToExportVector_.push_back(p_id);
            }
        }
    }

    void MetGmCDMWriterImpl::mapKildeNamesToFillValues(const std::auto_ptr<XMLDoc>& doc)
    {
        /**
          * it is really expected that config will be used
          */
        if(doc.get() != 0) {
            XPathObjPtr xpathObj = doc->getXPathObject("/metgm/variable");
            xmlNodeSetPtr nodes = xpathObj->nodesetval;
            size_t size = (nodes) ? nodes->nodeNr : 0;
            for (size_t i = 0; i < size; ++i) {
                xmlNodePtr node = nodes->nodeTab[i];
                std::string metnoName = getXmlProp(node, "name");
                // get the metgm p_id value
                XPathObjPtr xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+metnoName+"\"]/attribute[@name=\"_FillValue\"]");
                std::string str_FillValue;
                if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                    str_FillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                    if( str_FillValue.empty()) {
                        continue;
                    } else {
                        float fillValue = 0;
                        fillValue = boost::lexical_cast<float>(str_FillValue);
                        kildeName2FillValueMap_.insert(std::make_pair<std::string, float>(metnoName, fillValue));
                    }
                }
            }
        }
    }

    void MetGmCDMWriterImpl::mapMetgmPidToMetgmHDs(const std::auto_ptr<XMLDoc>& doc)
    {
        /**
          * hard coded defaults
          * config.xml will override defaults
          */

        for(size_t index = 0; index < 28; ++index) {
            pid2hdmap_.insert(std::make_pair<short, short>(index, 1));
        }

        // default override for p_id = 0
        pid2hdmap_.insert(std::make_pair<short, short>(0, 4));

        if(doc.get() != 0) {
            /**
              * TODO: support for config file
              */
        }
    }

    void MetGmCDMWriterImpl::openMgmFileHandle()
    {
        /**
          * open file in binary mode for writing
          */
        metgmFileHandle_ = fopen(outputFile.c_str(),"wb");
        if(metgmFileHandle_ == 0) {
            throw CDMException("can't open output file");
        }
    }

    void MetGmCDMWriterImpl::allocateMgmHandle()
    {
        metgmHandle_ = mgm_new_handle();
        if(metgmHandle_ == 0) {
            throw CDMException("can't allocate mgm_handle");
        }
    }

    void MetGmCDMWriterImpl::closeMgmFileHandle()
    {
        if(fclose(metgmFileHandle_) != 0) {
            throw CDMException("can't close output file");
        }
        metgmFileHandle_ = 0;
    }

    void MetGmCDMWriterImpl::freeMgmHandle()
    {
        if(mgm_free_handle(metgmHandle_) != MGM_OK) {
            throw CDMException("can't free mgm_handle");
        }
        metgmHandle_ = 0;
    }

    void MetGmCDMWriterImpl::detectCDMVariables() {
        detectCDMVariablesByName();
//        detectCDMVariablesToExportByStandardName();
    }

    void MetGmCDMWriterImpl::detectCDMVariablesByName() {
        const CDM& cdmRef_ = cdmReader->getCDM();

        std::set<short> p_id_set;

        std::multimap<short, std::string>::const_iterator cmmapIt;
        for(cmmapIt = pid2kildemap_.begin(); cmmapIt != pid2kildemap_.end(); ++cmmapIt) {
            p_id_set.insert(cmmapIt->first);
        }

        std::set<short>::const_iterator cit;

        for(cit = p_id_set.begin(); cit != p_id_set.end(); ++cit) {

            const short p_id = *cit;

            assert(p_id >= 0);


#ifdef METGM_CDM_WRITER_DEBUG
//            std::cerr
//                    << "checking p_id "
//                    << p_id
//                    << std::endl;
#endif
            /**
              * for same p_id we could have more CDM variables
              */
            std::pair<std::multimap<short, std::string>::const_iterator,
                          std::multimap<short, std::string>::const_iterator > findIt = pid2kildemap_.equal_range(p_id);

            if(findIt.first == pid2kildemap_.end()
                    && findIt.second == pid2kildemap_.begin())
            {
#ifdef METGM_CDM_WRITER_DEBUG
//                std::cerr
//                    << "no prenamed variables found for p_id "
//                    << p_id
//                << std::endl;
#endif
                continue;
            }

            std::multimap<short, std::string>::const_iterator cit;
            for(cit = findIt.first; cit != findIt.second; ++cit) {
                std::string variableName = (*cit).second;
#ifdef METGM_CDM_WRITER_DEBUG
//                std::cerr
//                    << "for p_id "
//                    << p_id
//                    << " searching CDMVariable with name "
//                    << variableName
//                << std::endl;
#endif

                if(!cdmRef_.hasVariable(variableName)) {

#ifdef METGM_CDM_WRITER_DEBUG
//                    std::cerr
//                        << "for p_id "
//                        << p_id
//                        << " and name "
//                        << variableName
//                        << " CDM model doesn't have CDMVariable"
//                    << std::endl;
#endif
                    continue;
                }
#ifdef METGM_CDM_WRITER_DEBUG
//                std::cerr
//                    << "for p_id "
//                    << p_id
//                    << " and name "
//                    << variableName
//                    << " CDMVariable found!"
//                << std::endl;
#endif
                const CDMVariable* cdmVariable = &(cdmRef_.getVariable(variableName));

                /**
                  * check if CDMVariable* is in the final list
                  */

                std::pair<std::multimap<short, const CDMVariable*>::iterator,
                              std::multimap<short, const CDMVariable*>::iterator > varRangeIt = pid2CdmVariablesMMap_.equal_range(p_id);
                if(varRangeIt.first == pid2CdmVariablesMMap_.end()
                        && varRangeIt.second == pid2CdmVariablesMMap_.end())
                {
                    // not even one entry
                    pid2CdmVariablesMMap_.insert(std::make_pair<short, const CDMVariable*>(p_id,cdmVariable));
                } else {
                    // some entries -> check them
                    bool bFound = false;
                    std::multimap<short, const CDMVariable*>::const_iterator varIt;
                    for(varIt = varRangeIt.first; varIt != varRangeIt.second; ++varIt) {
                        if(cdmVariable == varIt->second)
                            bFound = true;
                    }
                    if(!bFound)
                        pid2CdmVariablesMMap_.insert(std::make_pair<short, const CDMVariable*>(p_id,cdmVariable));
                }
            }
        }
    }

    void MetGmCDMWriterImpl::detectCDMVariablesByStandardName() {

    }

    void MetGmCDMWriterImpl::writeGroup0Data()
    {
        if(mgm_set_version(metgmHandle_, *metgmVersion_) != MGM_OK)
            throw CDMException("mgm_set_version fails");
    }

    void MetGmCDMWriterImpl::writeGroup1Data()
    {
        const CDM& cdmRef = cdmReader->getCDM();

        std::string metgmFreeText;
        std::string metgmVersion;
        std::string metgmDataType;
        std::string metgmModelType;
        std::string metgmProductNation;
        std::string metgmAnalysisDateTime;
        std::string metgmStartDateTime;

        CDMAttribute metgmMetaData; // encoded within comment
        if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), "comment", metgmMetaData)) {
            try {
                boost::shared_ptr<XMLDoc> doc = XMLDoc::fromString(metgmMetaData.getStringValue());//std::auto_ptr<XMLDoc>(new XMLDoc(metgmMetaData.getStringValue().c_str(), metgmMetaData.getStringValue().size()));

                if(doc.get() != 0) {
                    XPathObjPtr xpathObj = doc->getXPathObject("/metgm_meta_data/attribute");
                    xmlNodeSetPtr nodes = xpathObj->nodesetval;
                    size_t size = (nodes) ? nodes->nodeNr : 0;
                    for (size_t i = 0; i < size; ++i) {
                        xmlNodePtr node = nodes->nodeTab[i];
                        std::string attributeName = getXmlProp(node, "name");
                        if(attributeName == std::string(FREE_TEXT)) {
                            metgmFreeText = getXmlProp(node, "value");
                        } else if(attributeName == std::string(VERSION)) {
                            metgmVersion = getXmlProp(node, "value");
                        } else if(attributeName == std::string(DATA_TYPE)) {
                            metgmDataType = getXmlProp(node, "value");
                        }  else if(attributeName == std::string(MODEL_TYPE)) {
                            metgmModelType = getXmlProp(node, "value");
                        } else if(attributeName == std::string(PRODUCTION_NATION)) {
                            metgmProductNation = getXmlProp(node, "value");
                        } else if(attributeName == std::string(ANALYSIS_DATE_TIME)) {
                            metgmAnalysisDateTime = getXmlProp(node, "value");
                        } else if(attributeName == std::string(START_DATE_TIME)) {
                            metgmStartDateTime = getXmlProp(node, "value");
                        }
                    }
                }
            } catch (CDMException exception) {
                // just ignore
            }
        }

        // set analysis date time
        CDMAttribute metgmAnalysisDateTimeAttribute;
        std::vector<std::string> refVarNames = cdmRef.findVariables("standard_name", "forecast_reference_time");
        if(!refVarNames.empty()) { // we have to honour if there is forecast time in CDM model we get
            analysisTime_ = getUniqueForecastReferenceTime(cdmReader);
        } else if(!metgmAnalysisDateTime.empty()) {
            analysisTime_ = boost::posix_time::from_iso_string(metgmAnalysisDateTime);
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), ANALYSIS_DATE_TIME, metgmAnalysisDateTimeAttribute)) {
            analysisTime_ = boost::posix_time::from_iso_string(metgmAnalysisDateTimeAttribute.getStringValue());
        } else {
            analysisTime_ = boost::posix_time::second_clock::universal_time();
        }

        TimeUnit tu("seconds since 1970-01-01 00:00:00");
        double unit_time = tu.posixTime2unitTime(analysisTime_);
        time_t analysis_t = tu.unitTime2epochSeconds(unit_time);

        if(mgm_set_analysis_date_time(metgmHandle_, analysis_t) != MGM_OK)
            throw CDMException("mgm_set_analysis_date_time fails");

        assert(cdmRef.hasVariable("time"));
        assert(cdmRef.hasDimension("time"));

        // set start date time
        const CDMDimension* tDimension = 0;
        if(cdmRef.getTimeAxis("time").empty()) {
            throw CDMException("something sucks time dimension");
        } else {
            tDimension = &cdmRef.getDimension(cdmRef.getTimeAxis("time"));
        }

        assert(tDimension);
        assert(tDimension->isUnlimited());

        const CDMVariable& timeVariable = cdmRef.getVariable("time");
        boost::shared_ptr<Data> kildeTimeData;
        if(timeVariable.hasData()) {
            kildeTimeData = timeVariable.getData();
        } else {
            kildeTimeData = cdmReader->getData(timeVariable.getName());
        }
        const CDMAttribute& timeUnitAttribute(cdmRef.getAttribute("time", std::string("units")));
        const std::string kilde_time_unit = timeUnitAttribute.getStringValue();
        const TimeUnit kilde_tu(kilde_time_unit);

        const boost::shared_array<double> kildeTimeVector = kildeTimeData->asConstDouble();
        double kildeTime0 = kildeTimeVector[0];

        time_t start_t_0 = kilde_tu.unitTime2epochSeconds(kildeTime0);

        startTime_ = kilde_tu.unitTime2posixTime(start_t_0);

        if(kildeTimeData->size() > 1) {
            double kildeTime1 = kildeTimeVector[1];
            time_t start_t_1 = kilde_tu.unitTime2epochSeconds(kildeTime1);
            dTimeStep_ = start_t_1 - start_t_0;
            std::cerr << "start_t_1 == " << start_t_1 << " and  start_t_0 == " << start_t_0 << " and dTimeStep == " << dTimeStep_ << std::endl;
        } else {
            /**
              * lets use some heuristics
              */
            dTimeStep_ = std::abs(start_t_0 - analysis_t);
        }

        if(mgm_set_start_date_time(metgmHandle_, start_t_0) != MGM_OK)
            throw CDMException("mgm_set_start_date_time fails");

        CDMAttribute metgmFreeTextAttribute;
        if(!metgmFreeText.empty()) { // value from xml
            if(mgm_set_free_text(metgmHandle_, metgmFreeText.c_str()) != MGM_OK)
                throw CDMException("mgm_set_free_text fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), FREE_TEXT, metgmFreeTextAttribute)) { // value from cdm model
            if(mgm_set_free_text(metgmHandle_, metgmFreeTextAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_free_text fails");
        } else { // default
            metgmFreeText = ("comment---------------------------------");
            if(mgm_set_free_text(metgmHandle_, metgmFreeTextAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_free_text fails");
        }

        CDMAttribute metgmDataTypeAttribute;
        if(!metgmDataType.empty()) { // value from xml
            short data_type = boost::lexical_cast<short>(metgmDataType.c_str());
            if(mgm_set_data_type(metgmHandle_, data_type) != MGM_OK)
                throw CDMException("mgm_set_data_type fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), DATA_TYPE, metgmDataTypeAttribute)) { // value from cdm model
            short data_type = boost::lexical_cast<short>(metgmDataTypeAttribute.getStringValue().c_str());
            if(mgm_set_data_type(metgmHandle_, data_type) != MGM_OK)
                throw CDMException("mgm_set_data_type fails");
        } else { // default
            short data_type = 4; // MGM_COMPOUND_DATA = 4;
            if(mgm_set_data_type(metgmHandle_, data_type) != MGM_OK)
                throw CDMException("mgm_set_data_type fails");
        }

        CDMAttribute metgmModelTypeAttribute;
        if(!metgmModelType.empty()) { // value from xml
            if(mgm_set_model_type(metgmHandle_, metgmModelType.c_str()) != MGM_OK)
                throw CDMException("mgm_set_model_type fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), MODEL_TYPE, metgmModelTypeAttribute)) { // value from cdm model
            if(mgm_set_model_type(metgmHandle_, metgmModelTypeAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_model_type fails");
        } else { // default
            metgmModelType = "----------------";
            if(mgm_set_model_type(metgmHandle_, metgmModelType.c_str()) != MGM_OK)
                throw CDMException("mgm_set_model_type fails");
        }

        CDMAttribute metgmProductNationAttribute;
        if(!metgmProductNation.empty()) { // value from xml
            if(mgm_set_production_nation(metgmHandle_, metgmProductNation.c_str()) != MGM_OK)
                throw CDMException("mgm_set_production_nation fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), PRODUCTION_NATION, metgmProductNationAttribute)) { // value from cdm model
            if(mgm_set_production_nation(metgmHandle_, metgmProductNationAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_production_nation fails");
        } else { // default
            metgmProductNation = "NOR";
            if(mgm_set_production_nation(metgmHandle_, metgmProductNation.c_str()) != MGM_OK)
                throw CDMException("mgm_set_production_nation fails");
        }

        //        std::string metgmStartDateTime;

    }

    void MetGmCDMWriterImpl::writeGroup2Data()
    {
        // Set group 2 data, applicable to Edition 1 only
        //  np     : total number of parameters
        //  return : 0 if OK, error code on error
        if(*metgmVersion_ == MGM_Edition1) {
            if(mgm_set_number_of_params(metgmHandle_, pid2CdmVariablesMMap_.size()) != 0)
                throw CDMException("mgm_set_number_of_params fails");
        } else if(*metgmVersion_ == MGM_Edition2) {
            if(mgm_set_number_of_params(metgmHandle_, pid2CdmVariablesMMap_.size()) != 0)
                throw CDMException("mgm_set_number_of_params fails");

            std::set<short> p_id_set;

            std::multimap<short, const CDMVariable*>::const_iterator cmmapIt;
            for(cmmapIt = pid2CdmVariablesMMap_.begin(); cmmapIt != pid2CdmVariablesMMap_.end(); ++cmmapIt) {
                p_id_set.insert(cmmapIt->first);
            }

            const short ndp = p_id_set.size();

            if(mgm_set_number_of_dist_params(metgmHandle_, ndp) != 0)
                throw CDMException("mgm_set_number_of_dist_params fails");

            std::set<short>::const_iterator cit;
            size_t index = 0;
            for(cit = p_id_set.begin(); cit != p_id_set.end(); ++cit) {

                ++index;

                const short p_id = *cit;

                size_t ndpr = pid2CdmVariablesMMap_.count(p_id);

                assert(mgm_set_param_id(metgmHandle_, index, p_id) == MGM_OK);
                assert(mgm_set_ndpr(metgmHandle_, index, ndpr) == MGM_OK);

                /**
                  * TODO: should HD be treated as CDMAttribute?
                  */
                if(pid2hdmap_.find(p_id) != pid2hdmap_.end()) {
                    assert(mgm_set_hd(metgmHandle_, index, pid2hdmap_[p_id]) == MGM_OK);
                } else {
                    throw CDMException("can't find hd value for p_id = " + boost::lexical_cast<std::string>(p_id));
                }
            }

        } else {
            throw CDMException("unknown metgm version");
        }
    }

    void MetGmCDMWriterImpl::writeHeader()
    {
        if(mgm_write_header(metgmFileHandle_, metgmHandle_) != MGM_OK)
            throw CDMException("mgm_write_header fails");
    }

    void MetGmCDMWriterImpl::writeGroup3TimeAxis(mgm_group3* gp3, const CDMVariable* pVar)
    {
        short int callResult = MGM_OK;

        const CDM& cdmRef(cdmReader->getCDM());

        std::string tName = cdmRef.getTimeAxis(pVar->getName());
        if(!tName.empty()) {

            const CDMDimension* tDimension = &cdmRef.getDimension(tName);

            assert(tDimension);

            if(tDimension->getLength() <= 0)
                throw CDMException("given time axis lenght is <= 0, check data manually");

            const boost::shared_ptr<Data> tData = cdmReader->getData(tDimension->getName());
            const boost::shared_array<double> tArray = tData->asConstDouble();

            std::less_equal<float> leq;
            if( std::find_if( &tArray[0], &tArray[tData->size()], boost::bind( leq, _1, 0 ) ) != &tArray[tData->size()])
                throw CDMException("MetgmCDMWriter is not supporting negative values on the time axis");

            time_t dT = tData->size() > 1 ? tArray[1] - tArray[0] : dTimeStep_;

            if(dT <= 0) {
                throw CDMException("MetgmCDMReader is not supporting dt <= 0");
            }

            /**
              * calculate and check if points on time axis are equidistant
              */
            std::deque<double> adjDiff(tData->size());
            std::adjacent_difference(&tArray[0], &tArray[tData->size()], adjDiff.begin());
            adjDiff.pop_front(); // remove first element

            std::deque<double>::iterator it = std::unique_copy(adjDiff.begin(), adjDiff.end(), adjDiff.begin());
            std::sort(adjDiff.begin(), it);

            adjDiff.resize(it - adjDiff.begin());

            if(adjDiff.size() != 1) {
                throw CDMException("time points at time axis are not equidistant [use extractor to split file on boundaries]");
            }

            dT = adjDiff.at(0);

            callResult = mgm_set_nt(gp3, tDimension->getLength());
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            callResult = mgm_set_dt(gp3, dT);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

        } else {
            // default values
            const CDMDimension* tDimension = cdmRef.getUnlimitedDim();

            assert(tDimension);

            boost::shared_ptr<Data> tData = cdmReader->getData(tDimension->getName());
            const boost::shared_array<double> tArray = tData->asConstDouble();

            std::less_equal<float> leq;
            if( std::find_if( &tArray[0], &tArray[tData->size()], boost::bind( leq, _1, 0 ) ) != &tArray[tData->size()])
                throw CDMException("MetgmCDMWriter is not supporting negative values on the time axis");

            time_t dT = tData->size() > 1 ? tArray[tData->size() - 1] - tArray[0] : dTimeStep_;

            if(dT <= 0)
                throw CDMException("MetgmCDMReader is not supporting dt <= 0");

            callResult = mgm_set_nt(gp3, 1);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            callResult = mgm_set_dt(gp3, dT);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));
        }
    }

    void MetGmCDMWriterImpl::writeGroup3HorizontalAxis(mgm_group3* gp3, const CDMVariable* pVar)
    {
        const CDM& cdmRef(cdmReader->getCDM());

        std::string xName = cdmRef.getHorizontalXAxis(pVar->getName());
        std::string yName = cdmRef.getHorizontalYAxis(pVar->getName());

        if(!xName.empty() && !yName.empty()) {
            /**
              * we have horizontal (X|Y) axes
              * let's determine their nature
              */

            std::string xAxisStandardName;
            std::string yAxisStandardName;

            CDMAttribute standardName;
            if(cdmRef.getAttribute(xName, "standard_name", standardName)) {
                xAxisStandardName = standardName.getStringValue();
            }
            if(cdmRef.getAttribute(yName, "standard_name", standardName)) {
                yAxisStandardName = standardName.getStringValue();
            }

            if(xAxisStandardName == std::string("longitude")
                      && yAxisStandardName == std::string("latitude")) {

                boost::shared_ptr<Data> xData = cdmReader->getData(xName);
                const boost::shared_array<double> xArray = xData->asConstDouble();
                std::vector<double> xVector(&xArray[0], &xArray[xData->size()]);

                boost::shared_ptr<Data> yData = cdmReader->getData(yName);;
                const boost::shared_array<double> yArray = yData->asConstDouble();
                std::vector<double> yVector(&yArray[0], &yArray[yData->size()]);

                double dx = 0;
                double cx = 0;
                if(xVector.size() > 1) {
//                    std::cerr << "xVector.size() = " << xVector.size() << std::endl;
                    dx = xVector[1] - xVector[0];
                    cx = xVector[0] + (xVector[xVector.size() - 1] - xVector[0]) / 2.0;
                    if(dx <= 0)
                        throw CDMException("MetgmCDMWriter doesn't support dx <= 0 [use Interpolator to adjust axis]");
                }
                assert(mgm_set_dx(gp3, dx) == MGM_OK);
                assert(mgm_set_nx(gp3, xVector.size()) == MGM_OK);
                assert(mgm_set_cx(gp3, cx) == MGM_OK);

                double dy = 0;
                double cy = 0;
                if(yVector.size() > 1) {
//                    std::cerr << "yVector.size() = " << yVector.size() << std::endl;
                    dy = yVector[1] - yVector[0];
                    cy = yVector[0] + (yVector[yVector.size() - 1] - yVector[0]) / 2.0;
                    if(dy <= 0)
                        throw CDMException("MetgmCDMWriter doesn't support dy <= 0 [use Interpolator to adjust axis]");
                }
                assert(mgm_set_dy(gp3, dy) == MGM_OK);
                assert(mgm_set_ny(gp3, yVector.size()) == MGM_OK);
                assert(mgm_set_cy(gp3, cy) == MGM_OK);

            } else if(xAxisStandardName == std::string("grid_longitude")
                      && yAxisStandardName == std::string("grid_latitude")) {

                throw CDMException("rotated longitude latitide not supported by metgm writer, consider first using Interpolator");

            } else if(xAxisStandardName == "projection_x_coordinate"
                      && yAxisStandardName == "projection_y_coordinate") {

                throw CDMException("projection_[x|y]_coordinate not supported by metgm writer, consider first using Interpolator");

            } else {

                throw CDMException("given projection not supported by metgm writer, consider first using Interpolator");
            }

        }
    }

    void MetGmCDMWriterImpl::writeGroup3VerticalAxis(mgm_group3* gp3, const CDMVariable* pVar)
    {
        short int callResult = MGM_OK;

        const CDM& cdmRef(cdmReader->getCDM());

        const CDMDimension* zDimension = 0;
        if(!cdmRef.getVerticalAxis( pVar->getName() ).empty())
                zDimension = &cdmRef.getDimension(cdmRef.getVerticalAxis( pVar->getName() ));

        if(zDimension) {
            size_t nz = zDimension->getLength();
            callResult = mgm_set_nz(gp3, nz);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            /**
              * sending by default all Z coordinates
              * this is why we are hard coding to 1
              */
            callResult = mgm_set_pz(gp3, 1);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            /**
              * 1. try to find metgm_pr CDMAttribute
              * 2. else try parsing the name for _GND or _MSL
              * 3. if MSL sent exists from before, then check for units to distinguish _GND or _MSL
              */
            CDMAttribute metgmPrAttribute;
            if(cdmRef.getAttribute(pVar->getName(), "metgm_pr", metgmPrAttribute)) {
                short pr = boost::lexical_cast<short>(metgmPrAttribute.getStringValue());
                callResult = mgm_set_pr(gp3, pr);
                if(callResult != MGM_OK)
                    throw CDMException(mgm_string_error(callResult));
            } else if(pVar->getName().find("_MSL") != std::string::npos) {
                callResult = mgm_set_pr(gp3, 0);
                if(callResult != MGM_OK)
                    throw CDMException(mgm_string_error(callResult));
            } else if(pVar->getName().find("_GND") != std::string::npos) {
                callResult = mgm_set_pr(gp3, 1);
                if(callResult != MGM_OK)
                    throw CDMException(mgm_string_error(callResult));
            } else {
                // check unit for the dimension
                CDMAttribute metgmUnitsAttribute;
                if(cdmRef.getAttribute(zDimension->getName(), "units", metgmUnitsAttribute)) {
                    std::string unitsName = metgmUnitsAttribute.getStringValue();
                    if(unitsName.find("Pa") != std::string::npos) {
                        callResult = mgm_set_pr(gp3, 2);
                        if(callResult != MGM_OK)
                            throw CDMException(mgm_string_error(callResult));
                    } else if(unitsName.find("m") != std::string::npos) {
                        if(pid2CdmVariablesMMap_.find(0) != pid2CdmVariablesMMap_.end()) {
                            // we have MSL pr = 1 (we are dealing with GND type)
                            callResult = mgm_set_pr(gp3, 1);
                            if(callResult != MGM_OK)
                                throw CDMException(mgm_string_error(callResult));
                        } else {
                            // no MSL in CDM model (pr = 0 if units not Pa)
                            callResult = mgm_set_pr(gp3, 0);
                            if(callResult != MGM_OK)
                                throw CDMException(mgm_string_error(callResult));
                        }
                    } else {
                        assert(0); // some todo here
                    }
                } else  {
                    assert(0); // some todo here
                }
            }
        } else {
            /**
              * NO vertical axis (allowed case)
              */
            assert(mgm_set_nz(gp3, 1) == MGM_OK);
            assert(mgm_set_pr(gp3, 0) == MGM_OK);
            assert(mgm_set_pz(gp3, 1) == MGM_OK);
        }
    }

    void MetGmCDMWriterImpl::writeGroup3Data(mgm_group3* gp3, const CDMVariable* pVar)
    {
        assert(gp3);
        assert(pVar);

        writeGroup3TimeAxis(gp3, pVar);

        writeGroup3HorizontalAxis(gp3, pVar);

        writeGroup3VerticalAxis(gp3, pVar);

        fimex_dump_group3(gp3);

        fimex_mgm_write_group3(metgmFileHandle_, metgmHandle_, gp3);
    }

    void MetGmCDMWriterImpl::writeGroup4Data(const mgm_group3* gp3, const CDMVariable* pVar)
    {
        assert(gp3);
        assert(pVar);

        const short p_id = mgm_get_p_id(gp3);
        assert(p_id >= 0);

        if(mgm_get_pz(gp3) == 0) {
            /**
              * for now shouldn't get here at all
              */
            assert(0);
            return;
        } else {
            std::string variableName = pVar->getName();
            assert(!variableName.empty());

            const CDM& cdmRef = cdmReader->getCDM();
            assert(cdmRef.hasVariable(variableName));

            std::string zAxisName = cdmRef.getVerticalAxis(variableName);
            if(zAxisName.empty()) {
                float f = 0;
                assert(mgm_write_group4 (metgmFileHandle_, metgmHandle_, &f) == MGM_OK);
                return;
            }
            if(!cdmRef.hasDimension(zAxisName))
                return;

            const CDMDimension& zDimRef = cdmRef.getDimension(zAxisName);

            const CDMVariable& zVarRef = cdmRef.getVariable(zDimRef.getName());
            boost::shared_ptr<Data> zData;
            if(zVarRef.hasData()) {
                zData = zVarRef.getData();
            } else {
                zData = cdmReader->getData(zVarRef.getName());
            }

            float* gp4 = new float[zDimRef.getLength()];
            const boost::shared_array<float> group4Data = zData->asConstFloat();
            for(size_t index = 0; index < zData->size(); ++index) {
//                std::cerr << "p_id " <<  p_id << " index " << index << " has value " << group4Data[index] << std::endl;
                gp4[index] = group4Data[index];
            }

            /**
              * convert data from kilde units to the metgm
              */
            Units un;
            const std::string kildeUnits = cdmRef.getAttribute(zVarRef.getName(), std::string("units")).getStringValue();
            const std::string metgmUnits = mgm_get_param_unit(p_id, metgmHandle_);

            double slope = 1.0;
            CDMAttribute slopeAttribute;
            if(cdmRef.getAttribute(zVarRef.getName(), std::string("scale_factor"), slopeAttribute)) {
                slope = slopeAttribute.getData()->asFloat()[0];
            }

            double offset = 0.0;
//            CDMAttribute offsetttribute;
//            if(cdmRef.getAttribute(zVarRef.getName(), std::string("scale_factor"), slopeAttribute)) {
//                slope = slopeAttribute.getData()->asFloat()[0];
//            }

            if(un.areConvertible(kildeUnits, metgmUnits)) {
                un.convert(kildeUnits, metgmUnits, slope, offset);
                for(size_t index = 0; index < zDimRef.getLength(); ++index)
                    gp4[index] = gp4[index] * slope + offset;
            }

            assert(mgm_write_group4 (metgmFileHandle_, metgmHandle_, gp4) == MGM_OK);

            delete [] gp4;

        }
    }

    void MetGmCDMWriterImpl::writeGroup5Data(const mgm_group3* gp3, const CDMVariable* pVar)
    {
        assert(gp3);
        assert(pVar);

        const short p_id = mgm_get_p_id(gp3);
        assert(p_id >= 0);

        std::string variableName = pVar->getName();

        const CDM& cdmRef = cdmReader->getCDM();

        CDMAttribute fillValueAttribute;
        std::string strFillValue;
        if(cdmRef.getAttribute(pVar->getName(), "_FillValue", fillValueAttribute))
            strFillValue = fillValueAttribute.getStringValue();

        /**
          * TODO: see if unit conversion is needed !
          */
//        size_t totalDataSize;
        boost::scoped_array<float> gp5(0);
        boost::scoped_array<float> gp5T(0);
        boost::shared_ptr<Data> data;
        boost::shared_array<float> group5Data;


//        size_t sliceSize = 1;

        float* currPos = 0;
        const CDMDimension* unLimDim = 0;

        MetGmHDTag hdTag = MetGmHDTag::createMetGmHDTag(&cdmRef, pVar);

        switch(hdTag.asShort()) {
            case MetGmHDTag::HD_2D:
                data = cdmReader->getData(variableName);
                group5Data = data->asConstFloat();

                assert(hdTag.totalSize() == data->size());
                gp5.reset(new float[hdTag.totalSize()]);

                for(size_t index = 0; index < hdTag.totalSize(); ++index) {
//                    std::cerr << " index " << index << " has value " << group5Data[index] << std::endl;
                    if(!strFillValue.empty() &&
                            strFillValue == boost::lexical_cast<std::string>(group5Data[index])) {
                        gp5[index] = 9999.0;
                    } else if(kildeName2FillValueMap_.find(pVar->getName()) != kildeName2FillValueMap_.end()
                              && kildeName2FillValueMap_[pVar->getName()] == group5Data[index]) {
                        gp5[index] = 9999.0;
                    } else if(isnan(group5Data[index])) {
//                        std::cerr << "isNaN triggered" << std::cerr;
                        gp5[index] = 9999.0;
                    } else {
                        gp5[index] = group5Data[index];
                    }
                }
                break;

            case MetGmHDTag::HD_3D_T:
                 // iterate over each unlimited dim (should be time)
                unLimDim = cdmRef.getUnlimitedDim();

                assert( hdTag.totalSize() == hdTag.sliceSize() * unLimDim->getLength() );

                gp5.reset(new float[hdTag.totalSize()]);

                currPos = gp5.get();

                for (size_t i = 0; i < unLimDim->getLength(); ++i) {
                    boost::shared_ptr<Data> data = cdmReader->getDataSlice(variableName, i);
                    const boost::shared_array<float> group5Data = data->asConstFloat();
                    assert(hdTag.sliceSize() == data->size());
                    for(size_t index = 0; index < data->size(); ++index) {
//                        std::cerr << " index " << index << " has value " << group5Data[index] << std::endl;
                        if(!strFillValue.empty() &&
                                strFillValue == boost::lexical_cast<std::string>(group5Data[index])) {
                            *currPos = 9999.0;
//                            std::cerr << "_FillValue triggered" << " and index " << index << " has value " << (*currPos) << std::endl;
                        } else if (!strFillValue.empty() &&
                                   boost::lexical_cast<float>(strFillValue) == group5Data[index]) {
                            *currPos = 9999.0;
//                            std::cerr << "_FillValue triggered" << " and index " << index << " has value " << (*currPos) << std::endl;
                        } else if(kildeName2FillValueMap_.find(pVar->getName()) != kildeName2FillValueMap_.end()
                                  && kildeName2FillValueMap_[pVar->getName()] == group5Data[index]) {
                            *currPos = 9999.0;
//                            std::cerr << "_FillValue triggered" << " and index " << index << " has value " << (*currPos) << std::endl;
                        } else if(isnan(group5Data[index])) {
//                            std::cerr << "isNaN triggered" << std::endl;
                            *currPos = 9999.0;
                        } else {
                            *currPos = group5Data[index];
                        }

//                        std::cerr << __FUNCTION__ << "|"
//                                  << __LINE__     << "|"
//                                  << " index " << index << " has value " << (*currPos)
//                                  << std::endl;

                        ++currPos;
                    }
                }

                {
                    // data re-layout Fimex -> MetGM

                    gp5T.reset(new float[hdTag.totalSize()]);

                    float* slice = gp5.get();
                    float* sliceT = gp5T.get();

                    for(size_t sIndex = 0; sIndex < hdTag.tSize(); ++sIndex) {

                        slice = gp5.get() + sIndex * hdTag.sliceSize();
                        sliceT = gp5T.get() + sIndex * hdTag.sliceSize();

                        for(size_t z_index = 0; z_index < hdTag.zSize(); ++z_index) {

                            for(size_t y_index = 0; y_index < hdTag.ySize(); ++y_index) {
                                for(size_t x_index = 0; x_index < hdTag.xSize(); ++x_index) {
                                    sliceT[z_index + x_index * hdTag.zSize() + y_index * (hdTag.zSize() * hdTag.xSize())] =
                                            slice[z_index * (hdTag.ySize() * hdTag.xSize()) + y_index * hdTag.xSize() + x_index];

                                } // x_index
                            } // y_index
                        } // z_index

                    } // sliceIndex

                    gp5.swap(gp5T);
                }


                break;
            case MetGmHDTag::HD_0D:
            case MetGmHDTag::HD_0D_T:
            case MetGmHDTag::HD_1D:
            case MetGmHDTag::HD_1D_T:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D:
            default:
                throw CDMException(std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :") + hdTag.asString());
        }

        /**
          * apply scale and offset
          */
        double slope = 1.0;
        CDMAttribute slopeAttribute;
        if(cdmRef.getAttribute(pVar->getName(), std::string("scale_factor"), slopeAttribute)) {
            slope = slopeAttribute.getData()->asDouble()[0];
        }

        double offset = 0.0f;

        for(size_t index = 0; index < hdTag.totalSize(); ++index) {
            if(gp5[index] != 9999.0)
                gp5[index] = gp5[index] * slope + offset;
        }

        /**
          * convert data from kilde units to the metgm
          */
        Units un;
        const std::string kildeUnits = cdmRef.getAttribute(variableName, std::string("units")).getStringValue();
        const std::string metgmUnits = mgm_get_param_unit(p_id, metgmHandle_);

        double newSlope = 1.0;
        double newOffset = 0.0;
        if(kildeUnits != std::string("ratio") && metgmUnits != std::string("1")) {
            if(un.areConvertible(kildeUnits, metgmUnits)) {
                un.convert(kildeUnits, metgmUnits, newSlope, newOffset);
                for(size_t index = 0; index < hdTag.totalSize(); ++index)
                    if(gp5[index] != 9999.)
                        gp5[index] = gp5[index] * newSlope + newOffset;
            }
        }

        short callResult = MGM_OK;
        callResult = mgm_write_group5 (metgmFileHandle_, metgmHandle_, gp5.get());
        if(callResult != MGM_OK)
            throw CDMException(mgm_string_error(callResult));
    }

    void MetGmCDMWriterImpl::init()
    {
        detectCDMVariables();

        openMgmFileHandle();

        allocateMgmHandle();

//        const CDM::DimVec& dims = cdmReader->getCDM().getDimensions();
//        CDM::DimVec::const_iterator dimIt;

//        for(dimIt = dims.begin(); dimIt != dims.end(); ++dimIt){
//            CDM& cdmRef = const_cast<CDM&>(cdmReader->getCDM());

//            CDMVariable& dimVarRef = cdmRef.getVariable(dimIt->getName());
//            if(dimVarRef.hasData())
//                continue;
//            boost::shared_ptr<Data> data = cdmReader->getData(dimVarRef.getName());
//            size_t size = data->size();
//            std::cerr << "refilling data for dimension = " << dimVarRef.getName() << std::endl;
////            dimVarRef.setData(data);

//            size = size;

//        }

        std::map<short, const CDMVariable* >::const_iterator cit;

//        for(cit = pid2CdmVariablesMMap_.begin(); cit != this->pid2CdmVariablesMMap_.end(); ++cit) {
//            CDMVariable* pVar = const_cast<CDMVariable*> (cit->second);
//            const std::string varName = pVar->getName();

//            boost::shared_ptr<Data> data = cdmReader->getData(varName);

//            pVar->setData(data);
//        }

        writeGroup0Data();
        writeGroup1Data();
        writeGroup2Data();

        writeHeader();

        for(cit = pid2CdmVariablesMMap_.begin(); cit != this->pid2CdmVariablesMMap_.end(); ++cit) {

            const short p_id = cit->first;
            const CDMVariable* varPtr = cit->second;
            std::string variableName = varPtr->getName();

//            std::cerr
//                    << __FUNCTION__ << ":"
//                    << __LINE__     << ":"
//                    << "p_id = "
//                    << p_id
//                    << " CDMVariable with name = "
//                    << variableName
//                    << std::endl;

            mgm_group3* gp3 = mgm_new_group3();

            assert(gp3);

            mgm_set_p_id(gp3, p_id);

            writeGroup3Data(gp3, varPtr);
            writeGroup4Data(gp3, varPtr);
            writeGroup5Data(gp3, varPtr);

            assert(mgm_free_group3(gp3) == MGM_OK);

        }

        closeMgmFileHandle();
        freeMgmHandle();
    }

    MetGmCDMWriterImpl::MetGmCDMWriterImpl
            (
                    const boost::shared_ptr<CDMReader> cdmReader,
                    const std::string& outputFile,
                    const std::string& configFile
                    )
                        : CDMWriter(cdmReader, outputFile), configFileName_(configFile),
                          metgmFileHandle_(0), metgmHandle_(0)
    {
        std::auto_ptr<XMLDoc> xmlDoc;
        if (configFileName_ == std::string()) {
            xmlDoc = std::auto_ptr<XMLDoc>(0);
        } else {
            xmlDoc = std::auto_ptr<XMLDoc>(new XMLDoc(configFileName_));
        }

        if(metgmVersion_ == 0)
            metgmVersion_ = boost::shared_ptr<MetGmVersion>(new MetGmVersion(MGM_Edition1));

        assert(xmlDoc.get() != 0);

        mapMetgmPidToMetgmHDs(xmlDoc);
        mapKildeNamesToFillValues(xmlDoc);
        mapKildeVariablesToMetgmPids(xmlDoc);
        init();
    }

    MetGmCDMWriterImpl::~MetGmCDMWriterImpl()
    {
        if(metgmHandle_ != 0)
            mgm_free_handle(metgmHandle_);

        if(metgmFileHandle_ != 0)
            fclose(metgmFileHandle_);
    }

} // end namespace


