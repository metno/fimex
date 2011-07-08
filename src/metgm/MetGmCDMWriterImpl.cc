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
        MGM_THROW_ON_ERROR(mgm_set_version(*metgmHandle_, *metgmVersion_));
    }

    void MetGmCDMWriterImpl::writeGroup1Data()
    {
        boost::shared_ptr<MetGmGroup1Ptr> pg1 = MetGmGroup1Ptr::createMetGmGroup1Ptr(cdmReader);

        MGM_THROW_ON_ERROR(mgm_set_analysis_date_time(*metgmHandle_, pg1->analysisTime()))

        MGM_THROW_ON_ERROR(mgm_set_start_date_time(*metgmHandle_, pg1->startTime()))

        MGM_THROW_ON_ERROR(mgm_set_free_text(*metgmHandle_, pg1->freeText().c_str()))

        MGM_THROW_ON_ERROR(mgm_set_data_type(*metgmHandle_, pg1->dataType()))

        MGM_THROW_ON_ERROR(mgm_set_model_type(*metgmHandle_, pg1->modelType().c_str()))

        MGM_THROW_ON_ERROR(mgm_set_production_nation(*metgmHandle_, pg1->productNation().c_str()))

    }

    void MetGmCDMWriterImpl::writeGroup2Data()
    {
        short total_number_of_parameters = cdmVariableProfileMap_.size();
        std::cerr << __FUNCTION__ << "@" << __LINE__ << " total_number_of_parameters :" << total_number_of_parameters << std::endl;
        MGM_THROW_ON_ERROR(mgm_set_number_of_params(*metgmHandle_, cdmVariableProfileMap_.size()))

        if(*metgmVersion_ == MGM_Edition2) {

            pidIndex &pidindex = cdmVariableProfileMap_.get<pid_index>();
            const short ndp = pidindex.size();

            MGM_THROW_ON_ERROR(mgm_set_number_of_dist_params(*metgmHandle_, ndp))

            size_t index = 0;
            for(pidIndex::const_iterator cit = pidindex.begin(); cit != pidindex.end(); ++cit) {

                ++index;

                const MetGmCDMVariableProfile& profile = *cit;
                size_t ndpr = pidindex.count(profile.p_id_);

                MGM_THROW_ON_ERROR(mgm_set_param_id(*metgmHandle_, index, profile.p_id_))
                MGM_THROW_ON_ERROR(mgm_set_ndpr(*metgmHandle_, index, ndpr))

                /**
                  * TODO: should HD be treated as CDMAttribute?
                  *
                  * the HD value should be highest for given parameter
                  */


                MGM_THROW_ON_ERROR(mgm_set_hd(*metgmHandle_, index, profile.hd()))

            }

        }
    }

    void MetGmCDMWriterImpl::writeHeader()
    {
        MGM_THROW_ON_ERROR(mgm_write_header(*metgmFileHandle_, *metgmHandle_))
    }

    void MetGmCDMWriterImpl::writeGroup3TimeAxis(const CDMVariable* pVar)
    {
        cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();
        MetGmTagsPtr tags = cdmVariableView.find(pVar)->pTags_;

        if(tags->dimTag()->tTag().get()) {
            MGM_THROW_ON_ERROR(tags->gp3()->set_nt(tags->dimTag()->tTag()->nT()))
            MGM_THROW_ON_ERROR(tags->gp3()->set_dt(tags->dimTag()->tTag()->dT()))
        } else {
            MGM_THROW_ON_ERROR(tags->gp3()->set_nt(1))
            MGM_THROW_ON_ERROR(tags->gp3()->set_dt(metgmTimeTag_->dT() * metgmTimeTag_->nT()))
        }
    }

    void MetGmCDMWriterImpl::writeGroup3HorizontalAxis(const CDMVariable* pVar)
    {
        cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();
        MetGmTagsPtr tags = cdmVariableView.find(pVar)->pTags_;

        // x
        MGM_THROW_ON_ERROR(tags->gp3()->set_dx(tags->dimTag()->xTag()->dx()));
        MGM_THROW_ON_ERROR(tags->gp3()->set_nx(tags->dimTag()->xTag()->nx()));
        MGM_THROW_ON_ERROR(tags->gp3()->set_cx(tags->dimTag()->xTag()->cx()));
        // y
        MGM_THROW_ON_ERROR(tags->gp3()->set_dy(tags->dimTag()->yTag()->dy()));
        MGM_THROW_ON_ERROR(tags->gp3()->set_ny(tags->dimTag()->yTag()->ny()));
        MGM_THROW_ON_ERROR(tags->gp3()->set_cy(tags->dimTag()->yTag()->cy()));
    }

    void MetGmCDMWriterImpl::writeGroup3VerticalAxis(const CDMVariable* pVar)
    {
        cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();
        MetGmTagsPtr tags = cdmVariableView.find(pVar)->pTags_;

        if(tags->dimTag().get() && tags->dimTag()->zTag().get()) {
            MGM_THROW_ON_ERROR(tags->gp3()->set_nz(tags->dimTag()->zTag()->nz()));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pr(tags->dimTag()->zTag()->pr()));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pz(tags->dimTag()->zTag()->pz()));
        } else {
            /* no z profile for variable*/
            MGM_THROW_ON_ERROR(tags->gp3()->set_nz(1));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pr(0));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pz(1));
        }
    }

    void MetGmCDMWriterImpl::writeGroup3Data(const CDMVariable* pVar)
    {
        cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();

        writeGroup3TimeAxis(pVar);

        writeGroup3HorizontalAxis(pVar);

        writeGroup3VerticalAxis(pVar);

        cdmVariableView.find(pVar)->pTags_->gp3()->dump();

        MGM_THROW_ON_ERROR(mgm_write_group3(*metgmFileHandle_, *metgmHandle_, *(cdmVariableView.find(pVar)->pTags_->gp3())));
    }

    void MetGmCDMWriterImpl::writeGroup4Data(const CDMVariable* pVar)
    {
        cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();

        if(cdmVariableView.find(pVar)->pTags_->dimTag().get()
                && cdmVariableView.find(pVar)->pTags_->dimTag()->zTag().get()) {
            MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, cdmVariableView.find(pVar)->pTags_->dimTag()->zTag()->points().get()));
        } else {
            /* no z profile for variable */
            float f = 0;
            MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, &f));
        }

    }

    void MetGmCDMWriterImpl::writeGroup5Data(const CDMVariable* pVar)
    {
        cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();

        MGM_THROW_ON_ERROR(mgm_write_group5 (*metgmFileHandle_, *metgmHandle_, *(cdmVariableView.find(pVar)->pTags_->gp5())));
    }

    void MetGmCDMWriterImpl::init()
    {
        detectCDMVariables();

        std::map<short, const CDMVariable* >::const_iterator cit;

        for(cit = pid2CdmVariablesMMap_.begin(); cit != this->pid2CdmVariablesMMap_.end(); ++cit) {

            const short p_id = cit->first;
            const CDMVariable* varPtr = cit->second;
            std::string variableName = varPtr->getName();

            std::cerr
                    << __FUNCTION__ << ":"
                    << __LINE__     << ":"
                    << "p_id = "
                    << p_id
                    << " CDMVariable with name = "
                    << variableName
                    << std::endl;

            MetGmTagsPtr tags;
            boost::shared_ptr<MetGmGroup3Ptr> gp3 =
                    MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

            assert(gp3.get());

            gp3->set_p_id(p_id);

            cdmVariableIndex &cdmVariableView = cdmVariableProfileMap_.get<cdmvariable_index>();
            if(cdmVariableView.find(varPtr) != cdmVariableView.end())
                throw CDMException("hmmm... the variable should not be fount in variable profile map");

            if(kildeName2FillValueMap_.find(varPtr->getName()) != kildeName2FillValueMap_.end())
            {
                const float externalFillValue = kildeName2FillValueMap_[varPtr->getName()];
                tags = MetGmTags::createMetGmTags(cdmReader, varPtr, gp3, &externalFillValue);
            } else {
                tags = MetGmTags::createMetGmTags(cdmReader, varPtr, gp3, 0);
            }

            assert(tags.get());

            MetGmCDMVariableProfile profile(p_id, varPtr, tags);
            cdmVariableProfileMap_.insert(profile);
        }

        writeGroup0Data();
        writeGroup1Data();
        writeGroup2Data();

        writeHeader();

        profile_multi::const_iterator varIt;
        for(varIt = cdmVariableProfileMap_.begin(); varIt != cdmVariableProfileMap_.end(); ++varIt) {

            MetGmCDMVariableProfile profile = *varIt;
            writeGroup3Data(profile.variable());
            writeGroup4Data(profile.variable());
            writeGroup5Data(profile.variable());
        }
    }

    MetGmCDMWriterImpl::MetGmCDMWriterImpl
            (
                    boost::shared_ptr<CDMReader> cdmReader,
                    const std::string& outputFile,
                    const std::string& configFile
                    )
                        : CDMWriter(cdmReader, outputFile), configFileName_(configFile),
                          metgmVersion_(boost::shared_ptr<MetGmVersion>()),
                          metgmHandle_(boost::shared_ptr<MetGmHandlePtr>()),
                          metgmFileHandle_(boost::shared_ptr<MetGmFileHandlePtr>()),
                          metgmTimeTag_(boost::shared_ptr<MetGmTimeTag>())
    {
        std::auto_ptr<XMLDoc> xmlDoc;
        if (configFileName_ == std::string()) {
            xmlDoc = std::auto_ptr<XMLDoc>(0);
        } else {
            xmlDoc = std::auto_ptr<XMLDoc>(new XMLDoc(configFileName_));
        }


        metgmTimeTag_ = MetGmTimeTag::createMetGmTimeTag(cdmReader);
        metgmVersion_ = MetGmVersion::createMetGmVersion(xmlDoc);
        metgmFileHandle_ = MetGmFileHandlePtr::createMetGmFileHandlePtrForWriting(outputFile);
        metgmHandle_ = MetGmHandlePtr::createMetGmHandle();

        assert(xmlDoc.get() != 0);

//        mapMetgmPidToMetgmHDs(xmlDoc);
        mapKildeNamesToFillValues(xmlDoc);
        mapKildeVariablesToMetgmPids(xmlDoc);

        init();
    }

    MetGmCDMWriterImpl::~MetGmCDMWriterImpl() { }

} // end namespace


