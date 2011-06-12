#include "fimex/MetgmCDMWriter.h"

// fimex
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/CDMReaderUtils.h"

// udunits
#include <udunits2.h>

// libxml2
#include <libxml/tree.h>
#include <libxml/xpath.h>

// boost
#include <boost/shared_array.hpp>

// standard
#include <cstdio>
#include <auto_ptr.h>
#include <map>
#include <set>

namespace MetNoFimex {

#define FREE_TEXT "metgm_free_text"
#define VERSION   "metgm_version"
#define ANALYSIS_DATE_TIME "metgm_analysis_date_time"
#define START_DATE_TIME "metgm_start_date_time"
#define DATA_TYPE "metgm_data_type"
#define MODEL_TYPE "metgm_model_type"
#define PRODUCTION_NATION "metgm_production_nation"

    mgm_version METGM_CDMWriter::getMetgmVersion()
    {
        return this->metgmVersion_;
    }

    void METGM_CDMWriter::mapKildeVariablesToMetgmPids(const std::auto_ptr<XMLDoc>& doc)
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

    void METGM_CDMWriter::mapMetgmPidToMetgmHDs(const std::auto_ptr<XMLDoc>& doc)
    {
        /**
          * hard coded defaults
          * config.xml will override defaults
          */
        pid2hdmap_.insert(std::make_pair<short, short>(0, 4));

        pid2hdmap_.insert(std::make_pair<short, short>(2, 1));
        pid2hdmap_.insert(std::make_pair<short, short>(3, 1));
        pid2hdmap_.insert(std::make_pair<short, short>(4, 1));
        pid2hdmap_.insert(std::make_pair<short, short>(5, 1));
        pid2hdmap_.insert(std::make_pair<short, short>(6, 1));
        pid2hdmap_.insert(std::make_pair<short, short>(7, 1));

        if(doc.get() != 0) {
            /**
              * TODO: support for config file
              */
        }
    }

    void METGM_CDMWriter::openMgmFileHandle()
    {
        /**
          * open file in binary mode for writing
          */
        metgmFileHandle_ = fopen(outputFile.c_str(),"wb");
        if(metgmFileHandle_ == 0) {
            throw CDMException("can't open output file");
        }
    }

    void METGM_CDMWriter::allocateMgmHandle()
    {
        metgmHandle_ = mgm_new_handle();
        if(metgmHandle_ == 0) {
            throw CDMException("can't allocate mgm_handle");
        }
    }

    void METGM_CDMWriter::closeMgmFileHandle()
    {
        if(fclose(metgmFileHandle_) != 0) {
            throw CDMException("can't close output file");
        }
        metgmFileHandle_ = 0;
    }

    void METGM_CDMWriter::freeMgmHandle()
    {
        if(mgm_free_handle(metgmHandle_) != MGM_OK) {
            throw CDMException("can't free mgm_handle");
        }
        metgmHandle_ = 0;
    }

    void METGM_CDMWriter::detectCDMVariables() {
        detectCDMVariablesByName();
//        detectCDMVariablesToExportByStandardName();
    }

    void METGM_CDMWriter::detectCDMVariablesByName() {
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
            std::cerr
                    << "checking p_id "
                    << p_id
                    << std::endl;
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
                std::cerr
                    << "no prenamed variables found for p_id "
                    << p_id
                << std::endl;
#endif
                continue;
            }

            std::multimap<short, std::string>::const_iterator cit;
            for(cit = findIt.first; cit != findIt.second; ++cit) {
                std::string variableName = (*cit).second;
#ifdef METGM_CDM_WRITER_DEBUG
                std::cerr
                    << "for p_id "
                    << p_id
                    << " searching CDMVariable with name "
                    << variableName
                << std::endl;
#endif

                if(!cdmRef_.hasVariable(variableName)) {

#ifdef METGM_CDM_WRITER_DEBUG
                    std::cerr
                        << "for p_id "
                        << p_id
                        << " and name "
                        << variableName
                        << " CDM model doesn't have CDMVariable"
                    << std::endl;
#endif
                    continue;
                }
#ifdef METGM_CDM_WRITER_DEBUG
                std::cerr
                    << "for p_id "
                    << p_id
                    << " and name "
                    << variableName
                    << " CDMVariable found!"
                << std::endl;
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

    void METGM_CDMWriter::detectCDMVariablesByStandardName() {

    }

    void METGM_CDMWriter::writeGroup0Data()
    {
        if(mgm_set_version(metgmHandle_, metgmVersion_) != MGM_OK)
            throw CDMException("mgm_set_version fails");
    }

    void METGM_CDMWriter::writeGroup1Data()
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
            throw CDMException("something sucks with unlimited dimension");
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
        } else {
            dTimeStep_ = 3600.0;
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

    void METGM_CDMWriter::writeGroup2Data()
    {
        // Set group 2 data, applicable to Edition 1 only
        //  np     : total number of parameters
        //  return : 0 if OK, error code on error
        if(metgmVersion_ == MGM_Edition1) {
            if(mgm_set_number_of_params(metgmHandle_, pid2CdmVariablesMMap_.size()) != 0)
                throw CDMException("mgm_set_number_of_params fails");
        } else if(metgmVersion_ == MGM_Edition2) {
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

    void METGM_CDMWriter::writeHeader()
    {
        if(mgm_write_header(metgmFileHandle_, metgmHandle_) != MGM_OK)
            throw CDMException("mgm_write_header fails");
    }

    void METGM_CDMWriter::writeGroup3Data(mgm_group3* gp3, const CDMVariable* pVar)
    {
        assert(gp3);
        assert(pVar);

        const short p_id = mgm_get_p_id(gp3);

        /**
          * there can be more than one variable for same pid
          * depending on the reference of vertical coordinate
          * in metgm it is defined by (pr) parameter
          */

        const CDM& cdmRef(cdmReader->getCDM());

        const std::string kildeVariableName = pVar->getName();

        // quick check
        if(!cdmReader->getCDM().hasVariable(kildeVariableName))
            assert(0);

        const CDMDimension* tDimension = 0;
        const CDMDimension* xDimension = 0;
        const CDMDimension* yDimension = 0;
        const CDMDimension* zDimension = 0;

        std::string tName = cdmRef.getTimeAxis(kildeVariableName);
        if(!tName.empty()) {
            tDimension = &cdmRef.getDimension(tName);

            if(mgm_set_nt(gp3, tDimension->getLength()) != MGM_OK)
                throw CDMException("mgm_set_nt fails");

            assert(mgm_set_dt(gp3, dTimeStep_) == MGM_OK);

        } else {
            if(mgm_set_nt(gp3, 1) != MGM_OK)
                throw CDMException("mgm_set_nt fails");
            assert(mgm_set_dt(gp3, dTimeStep_) == MGM_OK);
        }


        boost::shared_ptr<Projection> proj;
        CDMAttribute gridMapping;
        if(cdmRef.getAttribute(pVar->getName(), "grid_mapping", gridMapping)) {
            std::string projName = gridMapping.getStringValue();
            CDMVariable projVar = cdmRef.getVariable(projName);
            CDMAttribute proj4Attribute;
            if(cdmRef.getAttribute(projVar.getName(), "proj4", proj4Attribute)) {
                proj = Projection::createByProj4(proj4Attribute.getStringValue());
            }
        }

        std::string lonName; // x
        std::string latName; // y
        cdmRef.getLatitudeLongitude(kildeVariableName, latName, lonName);
        std::string xName = cdmRef.getHorizontalXAxis(kildeVariableName); // x
        std::string yName = cdmRef.getHorizontalYAxis(kildeVariableName); // y

        if(!xName.empty() && !yName.empty()) {

            if(cdmRef.hasDimension(xName) && cdmRef.hasDimension(yName)) {

                // TAKE DATA

                boost::shared_ptr<Data> xData = cdmReader->getData(xName);
                const boost::shared_array<double> xArray = xData->asConstDouble();
                std::vector<double> xVector;
                for(size_t index = 0; index < xData->size(); ++index)
                    xVector.push_back(xArray[index]);

                boost::shared_ptr<Data> yData = cdmReader->getData(yName);;
                const boost::shared_array<double> yArray = yData->asConstDouble();
                std::vector<double> yVector;
                for(size_t index = 0; index < yData->size(); ++index)
                    yVector.push_back(yArray[index]);

                // convert by proj4
                if(proj.get()) {
                    if(proj->isDegree()) {
//                        boost::shared_ptr<Projection> projWgs84 = Projection::createByProj4("+proj=latlong +datum=WGS84");
//                        proj->convertFromLonLat(xVector, yVector);
//                        projWgs84->convertToLonLat(xVector, yVector);
                    } else {
                        proj->convertToLonLat(xVector, yVector);
                    }
                }

                float dx = 0;
                if(xData->size() > 1) {
                    dx = xVector[1] - xVector[0];
                }

                float dy = 0;
                if(yData->size() > 1) {
                    dy = yVector[1] - yVector[0];
                }

                short nx = xVector.size();
                assert(mgm_set_nx(gp3, nx) == MGM_OK);
                assert(mgm_set_dx(gp3, dx) == MGM_OK);
                float cx = xVector[xVector.size()/2];
                assert(mgm_set_cx(gp3, cx) == MGM_OK);

                short ny = yVector.size();
                assert(mgm_set_ny(gp3, ny) == MGM_OK);
                assert(mgm_set_dy(gp3, dy) == MGM_OK);
                float cy = yVector[yVector.size()/2];
                assert(mgm_set_cy(gp3, cy) == MGM_OK);


            } else {
                assert(0);
            }
        } else if(!lonName.empty() && !latName.empty()) {
            if(!cdmRef.hasDimension(latName)) {
                xDimension = &cdmRef.getDimension(cdmRef.getHorizontalXAxis(pVar->getName()));
            }

            if(cdmRef.hasVariable(lonName)) {
                xDimension = 0;
            } else {
                if(!cdmRef.getHorizontalXAxis(pVar->getName()).empty()) {
                    zDimension = &cdmRef.getDimension(cdmRef.getHorizontalXAxis(pVar->getName()));
                } else {
                    assert(0);
                }
            }

            if(cdmRef.hasVariable(latName)) {
                yDimension = 0;
            } else {
                if(!cdmRef.getHorizontalYAxis(pVar->getName()).empty()) {
                    yDimension = &cdmRef.getDimension(cdmRef.getHorizontalYAxis(pVar->getName()));
                } else {
                    assert(0);
                }
            }


            // TAKE DATA
            const CDMVariable& xVarRef = xDimension ? cdmRef.getVariable(xDimension->getName()) : cdmRef.getVariable(lonName);
            boost::shared_ptr<Data> xData;
            if(xVarRef.hasData()) {
                xData = xVarRef.getData();
            } else {
                xData = cdmReader->getData(xVarRef.getName());
            }

            const CDMVariable& yVarRef = yDimension ? cdmRef.getVariable(yDimension->getName()) : cdmRef.getVariable(latName);
            boost::shared_ptr<Data> yData;
            if(yVarRef.hasData()) {
                yData = yVarRef.getData();
            } else {
                yData = cdmReader->getData(yVarRef.getName());
            }

            const boost::shared_array<double> xArray = xData->asConstDouble();
            std::vector<double> xVector;
            for(size_t index = 0; index < xData->size(); ++index)
                xVector.push_back(xArray[index]);

            const boost::shared_array<double> yArray = yData->asConstDouble();
            std::vector<double> yVector;
            for(size_t index = 0; index < yData->size(); ++index)
                yVector.push_back(yArray[index]);

            // convert by proj4
            if(xDimension != 0 && yDimension != 0)
                proj->convertToLonLat(xVector, yVector);

            float dx = 0;
            if(xData->size() > 1) {
                dx = xVector[1] - xVector[0];
            }

            float dy = 0;
            if(yData->size() > 1) {
                dy = yVector[1] - yVector[0];
            }

            short nx = xVector.size();
            assert(mgm_set_nx(gp3, nx) == MGM_OK);
            assert(mgm_set_dx(gp3, dx) == MGM_OK);
            float cx = xVector[xVector.size()/2];
            assert(mgm_set_cx(gp3, cx) == MGM_OK);

            short ny = yVector.size();
            assert(mgm_set_ny(gp3, ny) == MGM_OK);
            assert(mgm_set_dy(gp3, dy) == MGM_OK);
            float cy = yVector[yVector.size()/2];
            assert(mgm_set_cy(gp3, cy) == MGM_OK);

            //            assert(mgm_set_pm(gp3, 9999) == MGM_OK);
        }

            std::string zName = cdmRef.getVerticalAxis(kildeVariableName);
            if(!zName.empty()) {

                assert(p_id != 0);

                zDimension = &cdmRef.getDimension(zName);

                size_t nz = zDimension->getLength();
                assert(mgm_set_nz(gp3, nz) == MGM_OK);

                /**
                  * sending by default all Z coordinates
                  * this is why we are hard coding to 1
                  */
                assert(mgm_set_pz(gp3, 1) == MGM_OK);

                /**
                  * 1. try to find metgm_pr CDMAttribute
                  * 2. else try parsing the name for _GND or _MSL
                  * 3. if MSL sent exists from before, then check for units to distinguish _GND or _MSL
                  */
                CDMAttribute metgmPrAttribute;
                if(cdmRef.getAttribute(kildeVariableName, "metgm_pr", metgmPrAttribute)) {
                    short pr = boost::lexical_cast<short>(metgmPrAttribute.getStringValue());
                    assert(mgm_set_pr(gp3, pr) == MGM_OK);
                } else if(kildeVariableName.find("_MSL") != std::string::npos) {
                    assert(mgm_set_pr(gp3, 0) == MGM_OK);
                } else if(kildeVariableName.find("_GND") != std::string::npos) {
                    assert(mgm_set_pr(gp3, 1) == MGM_OK);
                } else {
                    CDMAttribute metgmUnitsAttribute;
                    if(cdmRef.getAttribute(kildeVariableName, "units", metgmUnitsAttribute)) {
                        std::string unitsName = metgmUnitsAttribute.getStringValue();
                        if(unitsName.find("Pa") != std::string::npos) {
                            assert(mgm_set_pr(gp3, 2) == MGM_OK);
                        } else if(unitsName.find("m") != std::string::npos) {
                            if(pid2CdmVariablesMMap_.find(0) != pid2CdmVariablesMMap_.end()) {
                                // we have MSL pr = 1 (we are dealing with GND type)
                                assert(mgm_set_pr(gp3, 1) == MGM_OK);
                            } else {
                                // no MSL in CDM model (pr = 0 if units not Pa)
                                assert(mgm_set_pr(gp3, 0) == MGM_OK);
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
                if(p_id == 0) {
                    assert(mgm_set_nz(gp3, 1) == MGM_OK);
                    assert(mgm_set_pr(gp3, 0) == MGM_OK);
                    assert(mgm_set_pz(gp3, 1) == MGM_OK);
                } else {
                    assert(mgm_set_nz(gp3, 1) == MGM_OK);
                    assert(mgm_set_pr(gp3, 0) == MGM_OK);
                    assert(mgm_set_pz(gp3, 1) == MGM_OK);
                }
            }

            short callResult = mgm_write_group3(metgmFileHandle_, metgmHandle_, gp3);
            if(callResult != MGM_OK)
                throw CDMException(mgm_string_error(callResult));

    }

    void METGM_CDMWriter::writeGroup4Data(const mgm_group3* gp3, const CDMVariable* pVar)
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

    void METGM_CDMWriter::writeGroup5Data(const mgm_group3* gp3, const CDMVariable* pVar)
    {
        assert(gp3);
        assert(pVar);

        const short p_id = mgm_get_p_id(gp3);
        assert(p_id >= 0);

        std::string variableName = pVar->getName();

        const CDM& cdmRef = cdmReader->getCDM();
//        assert(cdmRef.hasVariable(variableName));

//        const CDMVariable& varRef = cdmRef.getVariable(va);

        /**
          * TODO: see if unit conversion is needed !
          */
        size_t totalDataSize;
        float* gp5 = 0;
        if (!cdmRef.hasUnlimitedDim(*pVar)) {
            const boost::shared_ptr<Data> data = cdmReader->getData(variableName);
            const boost::shared_array<float> group5Data = data->asConstFloat();

            totalDataSize = data->size();
            gp5 = new float[totalDataSize];

            for(size_t index = 0; index < totalDataSize; ++index) {
//                std::cerr << " index " << index << " has value " << group5Data[index] << std::endl;
                gp5[index] = group5Data[index];
            }
        } else {
            // iterate over each unlimited dim (usually time)
            size_t sliceSize = 1;
            if(!cdmRef.getHorizontalXAxis(variableName).empty()) {
                CDMDimension xDimension = cdmRef.getDimension(cdmRef.getHorizontalXAxis(variableName));
                sliceSize *= xDimension.getLength();
            }
            if(!cdmRef.getHorizontalYAxis(variableName).empty()) {
                CDMDimension yDimension = cdmRef.getDimension(cdmRef.getHorizontalYAxis(variableName));
                sliceSize *= yDimension.getLength();
            }
            if(!cdmRef.getVerticalAxis(variableName).empty()) {
                CDMDimension zDimension = cdmRef.getDimension(cdmRef.getVerticalAxis(variableName));
                sliceSize *= zDimension.getLength();
            }

            const CDMDimension* unLimDim = cdmRef.getUnlimitedDim();

            totalDataSize = sliceSize * unLimDim->getLength();

            gp5 = new float[totalDataSize];
            float* currPos = gp5;

            for (size_t i = 0; i < unLimDim->getLength(); ++i) {
                boost::shared_ptr<Data> data = cdmReader->getDataSlice(variableName, i);
                const boost::shared_array<float> group5Data = data->asConstFloat();
                assert(sliceSize == data->size());
                for(size_t index = 0; index < data->size(); ++index) {
//                    std::cerr << " index " << index << " has value " << group5Data[index] << std::endl;
                    *currPos = group5Data[index];
                    ++currPos;
                }
            }
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

        for(size_t index = 0; index < totalDataSize; ++index)
            gp5[index] = gp5[index] * slope + offset;

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
                for(size_t index = 0; index < totalDataSize; ++index)
                    gp5[index] = gp5[index] * newSlope + newOffset;
            }
        }

        short callResult = MGM_OK;
        callResult = mgm_write_group5 (metgmFileHandle_, metgmHandle_, gp5);

        assert(callResult == MGM_OK);

        delete [] gp5;
    }

    void METGM_CDMWriter::loadInternalCDMObject()
    {

    }

    //  Sequence when writing:
    //     open output file in binary mode      - user program responsibility
    //     mgm_new_handle                       - allocate handle memory
    //     mgm_set_xxx                          - set group 0, 1 and 2 data
    //     mgm_write_header                     - get handle, write group 0, 1 and 2 data
    //     mgm_new_group3                       - allocate group 3 memory
    //     mgm_set_xxx **                       - set group 3 data
    //     mgm_write_group3                     - write group3 data
    //     mgm_write_group4                     - write group 4 data
    //     mgm_write_group5                     - write group 5 data
    //     ... go back to ** and continue
    //     mgm_free_group3                      - deallocate group 3 memory
    //     mgm_free_handle                      - deallocate handle memory
    //     close output file                    - user program responsibility
    //
    //  Call to function mgm_read_next_group3 and mgm_read_this_group3 can be repeated without
    //  completing the given sequence when reading.
    //
    //  The METGM API only supports lat/long based METGMs.
    //  There is no support for UTM grid, i.e. group 3 pz other than 9999.
    //  All parameters must have the same horizontal grid and number of time steps, i.e all group 3s
    //  must have equal nx, ny, nt, dx, dy, dt, cx and cy. The only exceptions are terrain elevation
    //  (p_id = 0) and land use (p_id = 1) which only has one time step (nt = 1, dt = 0).

    void METGM_CDMWriter::init()
    {
        detectCDMVariables();

        openMgmFileHandle();

        allocateMgmHandle();

        const CDM::DimVec& dims = cdmReader->getCDM().getDimensions();
        CDM::DimVec::const_iterator dimIt;

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

    METGM_CDMWriter::METGM_CDMWriter
            (
                    const boost::shared_ptr<CDMReader> cdmReader,
                    const std::string& outputFile,
                    const std::string& configFile,
                    const mgm_version& version
                    )
                        : CDMWriter(cdmReader, outputFile), metgmVersion_(version), configFileName_(configFile),
                          metgmFileHandle_(0), metgmHandle_(0)
    {
        std::auto_ptr<XMLDoc> xmlDoc;
        if (configFileName_ == std::string()) {
            xmlDoc = std::auto_ptr<XMLDoc>(0);
        } else {
            xmlDoc = std::auto_ptr<XMLDoc>(new XMLDoc(configFileName_));
        }

        assert(xmlDoc.get() != 0);

        mapKildeVariablesToMetgmPids(xmlDoc);
        mapMetgmPidToMetgmHDs(xmlDoc);

        init();
    }

    METGM_CDMWriter::~METGM_CDMWriter()
    {
        if(metgmHandle_ != 0)
            mgm_free_handle(metgmHandle_);

        if(metgmFileHandle_ != 0)
            fclose(metgmFileHandle_);
    }

} // end namespace


