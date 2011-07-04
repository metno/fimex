/*
 * Fimex
 *
 * (C) Copyright 2011, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "../../include/metgm/MetGmCDMWriterImpl.h"

#include "metgm.h"

// fimex
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/interpolation.h"

// private/implementation code
#include "../../include/metgm/MetGmTags.h"
#include "../../include/metgm/MetGmUtils.h"
#include "../../include/metgm/MetGmVersion.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmGroup1Ptr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"

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
        if(mgm_set_version(*metgmHandle_, *metgmVersion_) != MGM_OK)
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

        if(mgm_set_analysis_date_time(*metgmHandle_, analysis_t) != MGM_OK)
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

        if(mgm_set_start_date_time(*metgmHandle_, start_t_0) != MGM_OK)
            throw CDMException("mgm_set_start_date_time fails");

        CDMAttribute metgmFreeTextAttribute;
        if(!metgmFreeText.empty()) { // value from xml
            if(mgm_set_free_text(*metgmHandle_, metgmFreeText.c_str()) != MGM_OK)
                throw CDMException("mgm_set_free_text fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), FREE_TEXT, metgmFreeTextAttribute)) { // value from cdm model
            if(mgm_set_free_text(*metgmHandle_, metgmFreeTextAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_free_text fails");
        } else { // default
            metgmFreeText = ("comment---------------------------------");
            if(mgm_set_free_text(*metgmHandle_, metgmFreeTextAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_free_text fails");
        }

        CDMAttribute metgmDataTypeAttribute;
        if(!metgmDataType.empty()) { // value from xml
            short data_type = boost::lexical_cast<short>(metgmDataType.c_str());
            if(mgm_set_data_type(*metgmHandle_, data_type) != MGM_OK)
                throw CDMException("mgm_set_data_type fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), DATA_TYPE, metgmDataTypeAttribute)) { // value from cdm model
            short data_type = boost::lexical_cast<short>(metgmDataTypeAttribute.getStringValue().c_str());
            if(mgm_set_data_type(*metgmHandle_, data_type) != MGM_OK)
                throw CDMException("mgm_set_data_type fails");
        } else { // default
            short data_type = 4; // MGM_COMPOUND_DATA = 4;
            if(mgm_set_data_type(*metgmHandle_, data_type) != MGM_OK)
                throw CDMException("mgm_set_data_type fails");
        }

        CDMAttribute metgmModelTypeAttribute;
        if(!metgmModelType.empty()) { // value from xml
            if(mgm_set_model_type(*metgmHandle_, metgmModelType.c_str()) != MGM_OK)
                throw CDMException("mgm_set_model_type fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), MODEL_TYPE, metgmModelTypeAttribute)) { // value from cdm model
            if(mgm_set_model_type(*metgmHandle_, metgmModelTypeAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_model_type fails");
        } else { // default
            metgmModelType = "----------------";
            if(mgm_set_model_type(*metgmHandle_, metgmModelType.c_str()) != MGM_OK)
                throw CDMException("mgm_set_model_type fails");
        }

        CDMAttribute metgmProductNationAttribute;
        if(!metgmProductNation.empty()) { // value from xml
            if(mgm_set_production_nation(*metgmHandle_, metgmProductNation.c_str()) != MGM_OK)
                throw CDMException("mgm_set_production_nation fails");
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), PRODUCTION_NATION, metgmProductNationAttribute)) { // value from cdm model
            if(mgm_set_production_nation(*metgmHandle_, metgmProductNationAttribute.getStringValue().c_str()) != MGM_OK)
                throw CDMException("mgm_set_production_nation fails");
        } else { // default
            metgmProductNation = "NOR";
            if(mgm_set_production_nation(*metgmHandle_, metgmProductNation.c_str()) != MGM_OK)
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
            if(mgm_set_number_of_params(*metgmHandle_, pid2CdmVariablesMMap_.size()) != 0)
                throw CDMException("mgm_set_number_of_params fails");
        } else if(*metgmVersion_ == MGM_Edition2) {
            if(mgm_set_number_of_params(*metgmHandle_, pid2CdmVariablesMMap_.size()) != 0)
                throw CDMException("mgm_set_number_of_params fails");

            std::set<short> p_id_set;

            std::multimap<short, const CDMVariable*>::const_iterator cmmapIt;
            for(cmmapIt = pid2CdmVariablesMMap_.begin(); cmmapIt != pid2CdmVariablesMMap_.end(); ++cmmapIt) {
                p_id_set.insert(cmmapIt->first);
            }

            const short ndp = p_id_set.size();

            if(mgm_set_number_of_dist_params(*metgmHandle_, ndp) != 0)
                throw CDMException("mgm_set_number_of_dist_params fails");

            std::set<short>::const_iterator cit;
            size_t index = 0;
            for(cit = p_id_set.begin(); cit != p_id_set.end(); ++cit) {

                ++index;

                const short p_id = *cit;

                size_t ndpr = pid2CdmVariablesMMap_.count(p_id);

                assert(mgm_set_param_id(*metgmHandle_, index, p_id) == MGM_OK);
                assert(mgm_set_ndpr(*metgmHandle_, index, ndpr) == MGM_OK);

                /**
                  * TODO: should HD be treated as CDMAttribute?
                  */
                if(pid2hdmap_.find(p_id) != pid2hdmap_.end()) {
                    assert(mgm_set_hd(*metgmHandle_, index, pid2hdmap_[p_id]) == MGM_OK);
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
        if(mgm_write_header(*metgmFileHandle_, *metgmHandle_) != MGM_OK)
            throw CDMException("mgm_write_header fails");
    }

    void MetGmCDMWriterImpl::writeGroup3TimeAxis(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
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

            callResult = gp3->set_nt(tDimension->getLength());
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            callResult = gp3->set_dt(dT);
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

            callResult = gp3->set_nt(1);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            callResult = gp3->set_dt(dT);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));
        }
    }

    void MetGmCDMWriterImpl::writeGroup3HorizontalAxis(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
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
                assert(gp3->set_dx(dx) == MGM_OK);
                assert(gp3->set_nx(xVector.size()) == MGM_OK);
                assert(gp3->set_cx(cx) == MGM_OK);

                double dy = 0;
                double cy = 0;
                if(yVector.size() > 1) {
//                    std::cerr << "yVector.size() = " << yVector.size() << std::endl;
                    dy = yVector[1] - yVector[0];
                    cy = yVector[0] + (yVector[yVector.size() - 1] - yVector[0]) / 2.0;
                    if(dy <= 0)
                        throw CDMException("MetgmCDMWriter doesn't support dy <= 0 [use Interpolator to adjust axis]");
                }
                assert(gp3->set_dy(dy) == MGM_OK);
                assert(gp3->set_ny(yVector.size()) == MGM_OK);
                assert(gp3->set_cy(cy) == MGM_OK);

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

    void MetGmCDMWriterImpl::writeGroup3VerticalAxis(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        short int callResult = MGM_OK;

        const CDM& cdmRef(cdmReader->getCDM());

        const CDMDimension* zDimension = 0;
        if(!cdmRef.getVerticalAxis( pVar->getName() ).empty())
                zDimension = &cdmRef.getDimension(cdmRef.getVerticalAxis( pVar->getName() ));

        if(zDimension) {
            size_t nz = zDimension->getLength();
            callResult = gp3->set_nz(nz);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

            /**
              * sending by default all Z coordinates
              * this is why we are hard coding to 1
              */
            callResult = gp3->set_pz(1);
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
                callResult = gp3->set_pr(pr);
                if(callResult != MGM_OK)
                    throw CDMException(mgm_string_error(callResult));
            } else if(pVar->getName().find("_MSL") != std::string::npos) {
                callResult = gp3->set_pr(0);
                if(callResult != MGM_OK)
                    throw CDMException(mgm_string_error(callResult));
            } else if(pVar->getName().find("_GND") != std::string::npos) {
                callResult = gp3->set_pr(1);
                if(callResult != MGM_OK)
                    throw CDMException(mgm_string_error(callResult));
            } else {
                // check unit for the dimension
                CDMAttribute metgmUnitsAttribute;
                if(cdmRef.getAttribute(zDimension->getName(), "units", metgmUnitsAttribute)) {
                    std::string unitsName = metgmUnitsAttribute.getStringValue();
                    if(unitsName.find("Pa") != std::string::npos) {
                        callResult = gp3->set_pr(2);
                        if(callResult != MGM_OK)
                            throw CDMException(mgm_string_error(callResult));
                    } else if(unitsName.find("m") != std::string::npos) {
                        if(pid2CdmVariablesMMap_.find(0) != pid2CdmVariablesMMap_.end()) {
                            // we have MSL pr = 1 (we are dealing with GND type)
                            callResult = gp3->set_pr(1);
                            if(callResult != MGM_OK)
                                throw CDMException(mgm_string_error(callResult));
                        } else {
                            // no MSL in CDM model (pr = 0 if units not Pa)
                            callResult = gp3->set_pr(0);
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
            assert(gp3->set_nz(1) == MGM_OK);
            assert(gp3->set_pr(0) == MGM_OK);
            assert(gp3->set_pz(1) == MGM_OK);
        }
    }

    void MetGmCDMWriterImpl::writeGroup3Data(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        assert(gp3.get());
        assert(pVar);

        writeGroup3TimeAxis(gp3, pVar);

        writeGroup3HorizontalAxis(gp3, pVar);

        writeGroup3VerticalAxis(gp3, pVar);

        gp3->dump();

        MGM_THROW_ON_ERROR(mgm_write_group3(*metgmFileHandle_, *metgmHandle_, *gp3));
    }

    void MetGmCDMWriterImpl::writeGroup4Data(const boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        MetGmTagsPtr tags;

        if(variable2TagsMap_.find(pVar) != variable2TagsMap_.end()) {
            tags = variable2TagsMap_[pVar];
            if(!tags->vTag.get())
                tags->vTag = MetGmVerticalTag::createMetGmVerticalTag(cdmReader, pVar);
        } else {
            tags = MetGmTagsPtr(new MetGmTags());
            variable2TagsMap_.insert(std::make_pair<const CDMVariable*, MetGmTagsPtr>(pVar, tags));
            tags->vTag = MetGmVerticalTag::createMetGmVerticalTag(cdmReader, pVar);
        }

        MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, tags->vTag->points().get()));
    }

    void MetGmCDMWriterImpl::writeGroup5Data(const boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        boost::shared_ptr<MetGmGroup5Ptr> pGp5;

        if(kildeName2FillValueMap_.find(pVar->getName()) != kildeName2FillValueMap_.end())
        {
            const float externalFillValue = kildeName2FillValueMap_[pVar->getName()];
            pGp5 = MetGmGroup5Ptr::createMetGmGroup5Ptr(cdmReader, pVar, gp3, &externalFillValue);
        } else {
            pGp5 = MetGmGroup5Ptr::createMetGmGroup5Ptr(cdmReader, pVar, gp3);
        }

        MGM_THROW_ON_ERROR(mgm_write_group5 (*metgmFileHandle_, *metgmHandle_, *pGp5));
    }

    void MetGmCDMWriterImpl::init()
    {
        detectCDMVariables();

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
//            std::string variableName = varPtr->getName();

//            std::cerr
//                    << __FUNCTION__ << ":"
//                    << __LINE__     << ":"
//                    << "p_id = "
//                    << p_id
//                    << " CDMVariable with name = "
//                    << variableName
//                    << std::endl;

            boost::shared_ptr<MetGmGroup3Ptr> gp3 = boost::shared_ptr<MetGmGroup3Ptr>(new MetGmGroup3Ptr());

            assert(gp3.get());

            gp3->set_p_id(p_id);

            writeGroup3Data(gp3, varPtr);
            writeGroup4Data(gp3, varPtr);
            writeGroup5Data(gp3, varPtr);
        }
    }

    MetGmCDMWriterImpl::MetGmCDMWriterImpl
            (
                    const boost::shared_ptr<CDMReader> cdmReader,
                    const std::string& outputFile,
                    const std::string& configFile
                    )
                        : CDMWriter(cdmReader, outputFile), configFileName_(configFile),
                          metgmVersion_(boost::shared_ptr<MetGmVersion>()),
                          metgmHandle_(boost::shared_ptr<MetGmHandlePtr>()),
                          metgmFileHandle_(boost::shared_ptr<MetGmFileHandlePtr>())
    {
        std::auto_ptr<XMLDoc> xmlDoc;
        if (configFileName_ == std::string()) {
            xmlDoc = std::auto_ptr<XMLDoc>(0);
        } else {
            xmlDoc = std::auto_ptr<XMLDoc>(new XMLDoc(configFileName_));
        }

        metgmVersion_ = boost::shared_ptr<MetGmVersion>(new MetGmVersion(MGM_Edition1));
        metgmFileHandle_ = boost::shared_ptr<MetGmFileHandlePtr>(new MetGmFileHandlePtr(outputFile, MetGmFileHandlePtr::WRITE));
        metgmHandle_ = boost::shared_ptr<MetGmHandlePtr>(new MetGmHandlePtr());

        assert(xmlDoc.get() != 0);

        mapMetgmPidToMetgmHDs(xmlDoc);
        mapKildeNamesToFillValues(xmlDoc);
        mapKildeVariablesToMetgmPids(xmlDoc);

        init();
    }

    MetGmCDMWriterImpl::~MetGmCDMWriterImpl() { }

} // end namespace


