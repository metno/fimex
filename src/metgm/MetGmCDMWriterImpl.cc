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
#include "fimex/Units.h"
#include "fimex/interpolation.h"

// private/implementation code
#include "../../include/metgm/MetGmTags.h"
#include "../../include/metgm/MetGmUtils.h"
#include "../../include/metgm/MetGmVersion.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmGroup1Ptr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"
#include "../../include/metgm/MetGmConfigurationMappings.h"

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

    typedef boost::shared_ptr<MetGmTags> MetGmTagsPtr;

    void MetGmCDMWriterImpl::configure(const std::auto_ptr<XMLDoc>& doc)
    {
        if(!doc.get())
            throw CDMException("Please supply xml config file the MetGmReader has to be informed how are pids mapped to actual CDM variables");

        const CDM& cdmRef = cdmReader->getCDM();

        // start metgm_parameter
        XPathObjPtr xpathObj = doc->getXPathObject("/metgm/metgm_parameter");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        size_t size = (nodes) ? nodes->nodeNr : 0;
        for (size_t i = 0; i < size; ++i) {

            xmlNodePtr node = nodes->nodeTab[i];

            std::string metgmName = getXmlProp(node, "name");
            if(metgmName.empty()) {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " parameter metgmName empty " << std::endl;
                continue;
            }

            XPathObjPtr xpathObj = doc->getXPathObject("/metgm/metgm_parameter[@name=\""+metgmName+"\"]/attribute[@name=\"metgm_p_id\"]");
            std::string str_p_id;
            short p_id = 0;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_p_id = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_p_id == std::string("")) {
                    std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " p_id not found -> " << metgmName << std::endl;
                    continue;
                }
                p_id = boost::lexical_cast<size_t>(str_p_id);
            } else {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " p_id not found -> " << metgmName << std::endl;
                continue;
            }

            xpathObj = doc->getXPathObject("/metgm/metgm_parameter[@name=\""+metgmName+"\"]/attribute[@name=\"_FillValue\"]");
            std::string str_FillValue;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_FillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            xpathObj = doc->getXPathObject("/metgm/metgm_parameter[@name=\""+metgmName+"\"]/attribute[@name=\"units\"]");
            std::string str_units;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_units = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            xpathObj = doc->getXPathObject("/metgm/metgm_parameter[@name=\""+metgmName+"\"]/attribute[@name=\"standard_name\"]");
            std::string str_standard_name;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_standard_name = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_standard_name.empty()) {
                    std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " standard_name not found -> " << metgmName << std::endl;
                    continue;
                }
                // find all variables with given standard_name value
                std::vector<std::string> varNames = cdmRef.findVariables("standard_name", str_standard_name);
                for(size_t index = 0; index < varNames.size(); ++index) {
                    std::string varName = varNames[index];
                    if(cdmRef.hasDimension(varName)) {
                        /* not interested in variables that are dimensions */
                        std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " [SKIPPING] there is dimension with name -> " << metgmName << std::endl;
                        continue;
                    }

                    const CDMVariable* pVar = &cdmRef.getVariable(varName);

                    if(!str_units.empty() && !cdmRef.getUnits(varName).empty()) {
                        /* check if dimensions convertible */
                        Units checker;
                        if(!checker.areConvertible(str_units, cdmRef.getUnits(varName))) {
                            std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " [SKIPPING] dimensions not convertible for -> " << metgmName << std::endl;
                            continue;
                        }
                    }

                    MetGmConfigurationMappings cfgEntry(p_id, pVar);
                    cfgEntry.kildeName_ = pVar->getName();
                    cfgEntry.units_ = str_units.empty() ? std::string() : str_units;

                    std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " found -> " << pVar->getName() << std::endl;

                    if(!str_FillValue.empty())
                        cfgEntry.setFillValue(boost::lexical_cast<float>(str_FillValue));

                    xmlConfiguration_.insert(cfgEntry);
                }

            } else {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " standard_name not found -> " << metgmName << std::endl;
                continue;
            }
        }
        // end metgm_parameter

//        XPathObjPtr
                xpathObj = doc->getXPathObject("/metgm/variable");
//        xmlNodeSetPtr
                nodes = xpathObj->nodesetval;
//        size_t
                size = (nodes) ? nodes->nodeNr : 0;
        for (size_t i = 0; i < size; ++i) {

            xmlNodePtr node = nodes->nodeTab[i];

            std::string kildeName = getXmlProp(node, "name");
            if(kildeName.empty()) {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " kildeName empty " << std::endl;
                continue;
            }

            if(!cdmRef.hasVariable(kildeName)) {
                std::cerr << __FILE__ " @ " << __FUNCTION__ << " @ " << __LINE__ << " : " << " not found in CDM model -> " << kildeName << std::endl;
                continue;
            }

            XPathObjPtr xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+kildeName+"\"]/attribute[@name=\"metgm_p_id\"]");
            std::string str_p_id;
            short p_id = 0;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_p_id = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_p_id == std::string("")) {
                    std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " p_id not found -> " << kildeName << std::endl;
                    continue;
                }
                p_id = boost::lexical_cast<size_t>(str_p_id);
            } else {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " p_id not found -> " << kildeName << std::endl;
                continue;
            }

            xpathObj = doc->getXPathObject("/metgm/metgm_parameter[@name=\""+kildeName+"\"]/attribute[@name=\"units\"]");
            std::string str_units;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_units = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            const CDMVariable* pVar = &cdmRef.getVariable(kildeName);

            if(!str_units.empty() && !cdmRef.getUnits(kildeName).empty()) {
                /* check if dimensions convertible */
                Units checker;
                if(!checker.areConvertible(str_units, cdmRef.getUnits(kildeName))) {
                    std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " [SKIPPING] dimensions not convertible for -> " << kildeName << std::endl;
                    continue;
                }
            }

            MetGmConfigurationMappings cfgEntry(p_id, pVar);
            cfgEntry.kildeName_ = kildeName;

            xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+kildeName+"\"]/attribute[@name=\"_FillValue\"]");
            std::string str_FillValue;
            if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_FillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_FillValue.empty()) {
                    /*do nothing*/;
                } else {
                    float fillValue = boost::lexical_cast<float>(str_FillValue);
                    cfgEntry.setFillValue(fillValue);
                }
            }

            xmlConfiguration_.insert(cfgEntry);
        }
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
        short total_number_of_parameters = cdmConfiguration_.size();
        std::cerr << __FUNCTION__ << "@" << __LINE__ << " total_number_of_parameters :" << total_number_of_parameters << std::endl;
        MGM_THROW_ON_ERROR(mgm_set_number_of_params(*metgmHandle_, cdmConfiguration_.size()))

        if(*metgmVersion_ == MGM_Edition2) {

            cdmPidView& pidView = cdmConfiguration_.get<cdm_pid_index>();
            const short ndp = pidView.size();

            MGM_THROW_ON_ERROR(mgm_set_number_of_dist_params(*metgmHandle_, ndp))

            size_t index = 0;
            for(cdmPidView::const_iterator cit = pidView.begin(); cit != pidView.end(); ++cit) {

                ++index;

                const MetGmCDMVariableProfile& profile = *cit;
                size_t ndpr = pidView.count(profile.p_id_);

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
        cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();
        MetGmTagsPtr tags = variableView.find(pVar)->pTags_;

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
        cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();
        MetGmTagsPtr tags = variableView.find(pVar)->pTags_;

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
        cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();
        MetGmTagsPtr tags = variableView.find(pVar)->pTags_;

        if(tags->dimTag().get() && tags->dimTag()->zTag().get()) {
            std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
                      << " pVar->Name " << pVar->getName()
                      << std::endl;
            MGM_THROW_ON_ERROR(tags->gp3()->set_nz(tags->dimTag()->zTag()->nz()));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pr(tags->dimTag()->zTag()->pr()));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pz(tags->dimTag()->zTag()->pz()));
        } else {
            /* no z profile for variable*/
            std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
                      << " pVar->Name " << pVar->getName()
                      << std::endl;
            MGM_THROW_ON_ERROR(tags->gp3()->set_nz(1));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pr(0));
            MGM_THROW_ON_ERROR(tags->gp3()->set_pz(1));
        }
    }

    void MetGmCDMWriterImpl::writeGroup3Data(const CDMVariable* pVar)
    {

        cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();

        writeGroup3TimeAxis(pVar);

        writeGroup3HorizontalAxis(pVar);

        writeGroup3VerticalAxis(pVar);

        std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
                  << " pVar->Name " << pVar->getName() << " dumping gp3"
                  << std::endl;
        variableView.find(pVar)->pTags_->gp3()->dump();

        MGM_THROW_ON_ERROR(mgm_write_group3(*metgmFileHandle_, *metgmHandle_, *(variableView.find(pVar)->pTags_->gp3())));
    }

    void MetGmCDMWriterImpl::writeGroup4Data(const CDMVariable* pVar)
    {
        cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();

        if(variableView.find(pVar)->pTags_->dimTag().get()
                && variableView.find(pVar)->pTags_->dimTag()->zTag().get()) {
            MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, variableView.find(pVar)->pTags_->dimTag()->zTag()->points().get()));
        } else {
            /* no z profile for variable */
            float f = 0;
            MGM_THROW_ON_ERROR(mgm_write_group4 (*metgmFileHandle_, *metgmHandle_, &f));
        }

    }

    void MetGmCDMWriterImpl::writeGroup5Data(const CDMVariable* pVar)
    {
        cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();

        MGM_THROW_ON_ERROR(mgm_write_group5 (*metgmFileHandle_, *metgmHandle_, *(variableView.find(pVar)->pTags_->gp5())));
    }

    void MetGmCDMWriterImpl::init()
    {
        xmlVariableView &variableView = xmlConfiguration_.get<xml_variable_index>();
        for(xmlVariableView::const_iterator vit = variableView.begin(); vit != variableView.end(); ++vit) {
            MetGmConfigurationMappings entry = *vit;

            std::cerr
                    << __FUNCTION__ << ":"
                    << __LINE__     << ":"
                    << "p_id = "
                    << entry.p_id_
                    << " CDMVariable with name = "
                    << entry.kildeName_
                    << std::endl;

            MetGmTagsPtr tags;
            boost::shared_ptr<MetGmGroup3Ptr> gp3 =
                    MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

            assert(gp3.get());

            gp3->set_p_id(entry.p_id_);

            cdmVariableView &variableView = cdmConfiguration_.get<cdm_variable_index>();
            if(variableView.find(entry.variable_) != variableView.end()) {
                throw CDMException("hmmm... the variable should not be fount in variable profile map");
            }

            if(entry.fillValue_.get())
            {
                tags = MetGmTags::createMetGmTags(cdmReader, entry.variable_, gp3, entry.fillValue_.get());
            } else {
                tags = MetGmTags::createMetGmTags(cdmReader, entry.variable_, gp3, 0);
            }

            assert(tags.get());

            MetGmCDMVariableProfile profile(entry.p_id_, entry.variable_, tags);
            cdmConfiguration_.insert(profile);
        }

        writeGroup0Data();
        writeGroup1Data();
        writeGroup2Data();

        writeHeader();

        cdm_configuration::const_iterator varIt;
        for(varIt = cdmConfiguration_.begin(); varIt != cdmConfiguration_.end(); ++varIt) {

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

        configure(xmlDoc);

        init();
    }

    MetGmCDMWriterImpl::~MetGmCDMWriterImpl() { }

} // end namespace