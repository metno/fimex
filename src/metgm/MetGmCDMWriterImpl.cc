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
        if(*metgmVersion_ == MGM_Edition1) {
            MGM_THROW_ON_ERROR(mgm_set_number_of_params(*metgmHandle_, pid2CdmVariablesMMap_.size()))
        } else if(*metgmVersion_ == MGM_Edition2) {
            MGM_THROW_ON_ERROR(mgm_set_number_of_params(*metgmHandle_, pid2CdmVariablesMMap_.size()))

            std::set<short> p_id_set;

            std::multimap<short, const CDMVariable*>::const_iterator cmmapIt;
            for(cmmapIt = pid2CdmVariablesMMap_.begin(); cmmapIt != pid2CdmVariablesMMap_.end(); ++cmmapIt) {
                p_id_set.insert(cmmapIt->first);
            }

            const short ndp = p_id_set.size();

            MGM_THROW_ON_ERROR(mgm_set_number_of_dist_params(*metgmHandle_, ndp))

            std::set<short>::const_iterator cit;
            size_t index = 0;
            for(cit = p_id_set.begin(); cit != p_id_set.end(); ++cit) {

                ++index;

                const short p_id = *cit;

                size_t ndpr = pid2CdmVariablesMMap_.count(p_id);

                MGM_THROW_ON_ERROR(mgm_set_param_id(*metgmHandle_, index, p_id))
                MGM_THROW_ON_ERROR(mgm_set_ndpr(*metgmHandle_, index, ndpr))

                /**
                  * TODO: should HD be treated as CDMAttribute?
                  *
                  * the HD value should be highest for given parameter
                  */
                if(pid2hdmap_.find(p_id) != pid2hdmap_.end()) {
                    MGM_THROW_ON_ERROR(mgm_set_hd(*metgmHandle_, index, pid2hdmap_[p_id]))
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
        MGM_THROW_ON_ERROR(mgm_write_header(*metgmFileHandle_, *metgmHandle_))
    }

    void MetGmCDMWriterImpl::writeGroup3TimeAxis(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        MetGmTagsPtr tags;

        if(variable2TagsMap_.find(pVar) != variable2TagsMap_.end()) {
            tags = variable2TagsMap_[pVar];
        } else {
            if(kildeName2FillValueMap_.find(pVar->getName()) != kildeName2FillValueMap_.end())
            {
                const float externalFillValue = kildeName2FillValueMap_[pVar->getName()];
                tags = MetGmTags::createMetGmTags(cdmReader, pVar, gp3, &externalFillValue);
            } else {
                tags = MetGmTags::createMetGmTags(cdmReader, pVar, gp3, 0);
            }

            variable2TagsMap_.insert(std::make_pair<const CDMVariable*, MetGmTagsPtr>(pVar, tags));
        }

        if(tags->dimTag()->tTag().get()) {
            MGM_THROW_ON_ERROR(gp3->set_nt(tags->dimTag()->tTag()->nT()))
            MGM_THROW_ON_ERROR(gp3->set_dt(tags->dimTag()->tTag()->dT()))
        } else {
            MGM_THROW_ON_ERROR(gp3->set_nt(1))
            MGM_THROW_ON_ERROR(gp3->set_dt(metgmTimeTag_->dT() * metgmTimeTag_->nT()))
        }
    }

    void MetGmCDMWriterImpl::writeGroup3HorizontalAxis(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        MetGmTagsPtr tags = variable2TagsMap_[pVar];

        // x
        MGM_THROW_ON_ERROR(gp3->set_dx(tags->dimTag()->xTag()->dx()));
        MGM_THROW_ON_ERROR(gp3->set_nx(tags->dimTag()->xTag()->nx()));
        MGM_THROW_ON_ERROR(gp3->set_cx(tags->dimTag()->xTag()->cx()));
        // y
        MGM_THROW_ON_ERROR(gp3->set_dy(tags->dimTag()->yTag()->dy()));
        MGM_THROW_ON_ERROR(gp3->set_ny(tags->dimTag()->yTag()->ny()));
        MGM_THROW_ON_ERROR(gp3->set_cy(tags->dimTag()->yTag()->cy()));
    }

    void MetGmCDMWriterImpl::writeGroup3VerticalAxis(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        MetGmTagsPtr tags = variable2TagsMap_[pVar];

        if(tags->dimTag().get() && tags->dimTag()->zTag().get()) {
            MGM_THROW_ON_ERROR(gp3->set_nz(tags->dimTag()->zTag()->nz()));
            MGM_THROW_ON_ERROR(gp3->set_pr(tags->dimTag()->zTag()->pr()));
            MGM_THROW_ON_ERROR(gp3->set_pz(tags->dimTag()->zTag()->pz()));
        } else {
            /* no z profile for variable*/
            MGM_THROW_ON_ERROR(gp3->set_nz(1));
            MGM_THROW_ON_ERROR(gp3->set_pr(0));
            MGM_THROW_ON_ERROR(gp3->set_pz(1));
        }
    }

    void MetGmCDMWriterImpl::writeGroup3Data(boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        writeGroup3TimeAxis(gp3, pVar);

        writeGroup3HorizontalAxis(gp3, pVar);

        writeGroup3VerticalAxis(gp3, pVar);

        gp3->dump();

        MGM_THROW_ON_ERROR(mgm_write_group3(*metgmFileHandle_, *metgmHandle_, *gp3));
    }

    void MetGmCDMWriterImpl::writeGroup4Data(const boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        MetGmTagsPtr tags = variable2TagsMap_[pVar];

        if(tags->dimTag().get() && tags->dimTag()->zTag().get()) {
            MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, tags->dimTag()->zTag()->points().get()));
        } else {
            /* no z profile for variable */
            float f = 0;
            MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, &f));
        }

    }

    void MetGmCDMWriterImpl::writeGroup5Data(const boost::shared_ptr<MetGmGroup3Ptr> gp3, const CDMVariable* pVar)
    {
        MetGmTagsPtr tags = variable2TagsMap_[pVar];
        MGM_THROW_ON_ERROR(mgm_write_group5 (*metgmFileHandle_, *metgmHandle_, *(tags->gp5())));
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

            boost::shared_ptr<MetGmGroup3Ptr> gp3 = MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

            assert(gp3.get());

            gp3->set_p_id(p_id);

            writeGroup3Data(gp3, varPtr);
            writeGroup4Data(gp3, varPtr);
            writeGroup5Data(gp3, varPtr);
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


