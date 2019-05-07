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

#include "MetGmCDMReaderImpl.h"

// private implementation details
//
#include "MetGmUtils.h"
#include "MetGmVersion.h"
#include "MetGmHandlePtr.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup2Ptr.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmGroup5Ptr.h"
#include "MetGmDimensionsTag.h"
#include "MetGmFileHandlePtr.h"

// METGM C lib
//
#include "metgm.h"

// fimex
//
#include "fimex/CDM.h"
#include "fimex/CDMAttribute.h"
#include "fimex/CDMDimension.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/XMLDoc.h"
#include "fimex/CDMException.h"

// boost
//
#include <boost/regex.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/scoped_array.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

// libxml2
//
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

//#define GXDEBUG 1

namespace MetNoFimex {

    MetGmCDMReaderImpl::MetGmCDMReaderImpl(const std::string& mgmsource, const XMLInput& configXML, const boost::shared_ptr<CDM>& cdm)
        : CDMReader(), sourceFileName_(mgmsource), configId_(configXML.id())
    {
        cdm_ = cdm; // as not accesible via initialzation list

        try {
            init(configXML);
        } catch (std::runtime_error& exp) {
            throw CDMException(std::string("MetGmCDMReaderImpl error: ") + exp.what());
        }
    }

    MetGmCDMReaderImpl::MetGmCDMReaderImpl(const boost::shared_ptr<CDM>& cdm)
        : CDMReader()
    {
        cdm_ = cdm; // as not accesible via initialzation list
    }

    MetGmCDMReaderImpl::~MetGmCDMReaderImpl() { }

    std::string MetGmCDMReaderImpl::spaceToUnderscore(const std::string& name)
    {
        return boost::algorithm::replace_all_copy(name, " ", "_");
    }

    void MetGmCDMReaderImpl::configure(const XMLDoc_p& doc)
    {
        if(!doc.get())
            throw CDMException("Please supply xml config file the MetGmReader has to be informed how are pids mapped to actual CDM variables");

        xmlXPathObject_p xpathObj = doc->getXPathObject("/metgm_config/reader/variable");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        size_t size = (nodes) ? nodes->nodeNr : 0;
        for (size_t i = 0; i < size; ++i) {

            xmlNodePtr node = nodes->nodeTab[i];

            std::string kildeName = getXmlProp(node, "name");
            if(kildeName.empty()) {
                continue;
            }

            xmlXPathObject_p xpathObj = doc->getXPathObject("/metgm_config/reader/variable[@name=\"" + kildeName + "\"]/attribute[@name=\"metgm_p_id\"]");
            std::string str_p_id;
            short p_id = 0;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_p_id = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_p_id == std::string("")) {
                    continue;
                }
                p_id = boost::lexical_cast<size_t>(str_p_id);
            } else {
                continue;
            }

            xpathObj = doc->getXPathObject("/metgm_config/reader/variable[@name=\""+kildeName+"\"]/attribute[@name=\"standard_name\"]");
            std::string str_standard_name;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_standard_name = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            if(str_standard_name.empty()) {
                continue;
            }

            xpathObj = doc->getXPathObject("/metgm_config/reader/variable[@name=\""+kildeName+"\"]/attribute[@name=\"units\"]");
            std::string str_units;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_units = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            xpathObj = doc->getXPathObject("/metgm_config/reader/variable[@name=\""+kildeName+"\"]/attribute[@name=\"add_offset\"]");
            std::string str_offset;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_offset = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            xpathObj = doc->getXPathObject("/metgm_config/reader/variable[@name=\""+kildeName+"\"]/attribute[@name=\"scale_factor\"]");
            std::string str_scale;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_scale = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            MetGmConfigurationMappings cfgEntry(p_id, spaceToUnderscore(kildeName));
            cfgEntry.standardName_ = spaceToUnderscore(str_standard_name);
            cfgEntry.units_        = str_units;
            cfgEntry.addOffset_    = str_offset;
            cfgEntry.scaleFactor_  = str_scale;

            xpathObj = doc->getXPathObject("/metgm_config/reader/variable[@name=\""+kildeName+"\"]/attribute[@name=\"_FillValue\"]");
            std::string str_FillValue;
            if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_FillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_FillValue.empty()) {
                    cfgEntry.fillValue_ = std::string("9999.0");
                } else {
                    cfgEntry.fillValue_ = str_FillValue;
                }
            }

            xmlConfiguration_.insert(cfgEntry);
        }
    }

    void MetGmCDMReaderImpl::init(const XMLInput& configXML)
    {
        XMLDoc_p xmlDoc = configXML.getXMLDoc();
        configure(xmlDoc);

        parseMgmFile(sourceFileName_);

        if(*pHandle_->version() == MGM_EditionNONE)
            throw CDMException(std::string("can't use MGM_EditionNONE as version"));

        addTimeDimension();
        addGlobalCDMAttributes();
        addHorizontalDimensions();
        addVerticalDimensions();
        addVariables();

//        sanityCheck();

    }

    void MetGmCDMReaderImpl::addGlobalCDMAttributes()
    {
        // ATM hardcoded values
        std::string hcConventions("CF-1.0");
        std::string hcInstitution("Forsvarets forskningsinstitutt, ffi.no");

        CDMAttribute cdmConventionsAttribute("Conventions", "string", hcConventions);
        CDMAttribute cdmInstitutionAttribute("institution", "string", hcInstitution);

        boost::posix_time::ptime now(boost::posix_time::second_clock::universal_time());

        std::string strHistory("");
        strHistory.append("analysis time: ").append(pGroup1_->analysisTimeAsIsoExtendedString());
        strHistory.append(" start time: ").append(pGroup1_->startTimeAsIsoExtendedString());
        strHistory.append(" created by Fimex on ");
        strHistory.append(boost::gregorian::to_iso_extended_string(now.date()));
        CDMAttribute cdmHistoryAttribute("history", "string", strHistory);

        CDMAttribute cdmSourceAttribute("source", "string", "unknown");
        if(*pHandle_->version() == MGM_Edition2) {
            cdmSourceAttribute = CDMAttribute("source", "string", std::string(pGroup1_->productNation()).append(" ").append(pGroup1_->modelType()));
        }
        CDMAttribute cdmTitleAttribute("title", "string", pHandle_->fileHandle()->fileName() + std::string(" ") + pHandle_->version()->getAsString());
        CDMAttribute cdmReferencesAttribute("references", "string", "unknown");

        CDMAttribute cdmMetgmAnalysisDateTimeAttribute("metgm_analysis_date_time", "string", pGroup1_->analysisTimeAsIsoString());
        CDMAttribute cdmMetgmStartDateTimeAttribute("metgm_start_date_time", "string", pGroup1_->startTimeAsIsoString());
        CDMAttribute cdmMetgmVersionAttribute("metgm_version", "string", spaceToUnderscore(pHandle_->version()->getAsString()));
        CDMAttribute cdmMetgmDataTypeAttribute("metgm_data_type", "string", pGroup1_->dataTypeAsString());
        CDMAttribute cdmMetgmFreeTextAttribute("metgm_free_text", "string", pGroup1_->freeText());

        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmConventionsAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmInstitutionAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmSourceAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmTitleAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmReferencesAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmHistoryAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmAnalysisDateTimeAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmStartDateTimeAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmVersionAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmDataTypeAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmFreeTextAttribute);

        /**
          * TODO: find global attributes that have
           *      "metgm" in name and create comment
          */
        std::stringstream metgm_comment;
        metgm_comment
                << "<meta_data>"
                << std::endl

                << "\t<attribute"
                << " name="  << '\"' << cdmMetgmAnalysisDateTimeAttribute.getName() << '\"'
                << " value=" << '\"' << cdmMetgmAnalysisDateTimeAttribute.getStringValue() << '\"'
                << " type=\"string\" />"
                << std::endl

                << "\t<attribute"
                << " name="  << '\"' << cdmMetgmStartDateTimeAttribute.getName() << '\"'
                << " value=" << '\"' << cdmMetgmStartDateTimeAttribute.getStringValue() << '\"'
                << " type=\"string\" />"
                << std::endl

                << "\t<attribute"
                << " name="  << '\"' << cdmMetgmVersionAttribute.getName() << '\"'
                << " value=" << '\"' << cdmMetgmVersionAttribute.getStringValue() << '\"'
                << " type=\"string\" />"
                << std::endl

                << "\t<attribute"
                << " name="  << '\"' << cdmMetgmDataTypeAttribute.getName() << '\"'
                << " value=" << '\"' << cdmMetgmDataTypeAttribute.getStringValue() << '\"'
                << " type=\"string\" />"
                << std::endl

                << "\t<attribute"
                << " name="  << '\"' << cdmMetgmFreeTextAttribute.getName() << '\"'
                << " value=" << '\"' << cdmMetgmFreeTextAttribute.getStringValue() << '\"'
                << " type=\"string\" />"
                << std::endl;

        if(*pHandle_->version() == MGM_Edition2) {
            CDMAttribute cdmMetgmProductionNationAttribute = CDMAttribute("metgm_production_nation", "string", pGroup1_->productNation());
            cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmProductionNationAttribute);
            CDMAttribute cdmMetgmModelTypeAttribute = CDMAttribute("metgm_model_type", "string", pGroup1_->modelType());
            cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmModelTypeAttribute);

            metgm_comment
                    << "\t<attribute"
                    << " name="  << '\"' << cdmMetgmProductionNationAttribute.getName() << '\"'
                    << " value=" << '\"' << cdmMetgmProductionNationAttribute.getStringValue() << '\"'
                    << " type=\"string\" />"
                    << std::endl

                    << "\t<attribute"
                    << " name="  << '\"' << cdmMetgmModelTypeAttribute.getName() << '\"'
                    << " value=" << '\"' << cdmMetgmModelTypeAttribute.getStringValue() << '\"'
                    << " type=\"string\" />"
                    << std::endl;
        }

        metgm_comment << "</meta_data>";

        CDMAttribute cdmCommentAttribute("comment", "string", metgm_comment.str());
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmCommentAttribute);
    }

    void MetGmCDMReaderImpl::addTimeDimension()
    {
        if(cdmConfiguration_.size() == 0)
            return;

        std::string hcTimeDimensionName = "time";
        std::string hcSymbolForTimeDimension = "T";
        std::string hcTimeDimensionUnits = "seconds since 1970-01-01 00:00:00 +00:00";

        cdmPidView& pidView = cdmConfiguration_.get<cdm_pid_index>();

        cdmPidView::iterator pIt = pidView.begin();
        for(; pIt != pidView.end(); ++pIt) if(pIt->p_id_ > 0) break;

        if(pIt == pidView.end())
            pIt = pidView.begin();

        long timeDimensionSize = pIt->pTags_->tTag()->nT();

        tDim_.setName(hcTimeDimensionName);
        tDim_.setLength(timeDimensionSize);
        tDim_.setUnlimited(true);

        std::vector<std::string> timeDimensionShape;
        timeDimensionShape.push_back(tDim_.getName());
        CDMDataType timeDimensionDataType = CDM_DOUBLE;
        CDMVariable timeVariable(hcTimeDimensionName, timeDimensionDataType, timeDimensionShape);

        std::vector<double> timeInUnitsVector = pIt->pTags_->tTag()->pointsAsDouble();
        DataPtr timeDimensionData = createData(timeDimensionDataType, timeInUnitsVector.begin(), timeInUnitsVector.end());
        timeVariable.setData(timeDimensionData);

        cdm_->addDimension(tDim_);
        cdm_->addVariable(timeVariable);

        // add attributes
        CDMAttribute timeUnitsAttribute("units", "string", hcTimeDimensionUnits);
        CDMAttribute timeLongNameAttribute("long_name", "string", hcTimeDimensionName);
        CDMAttribute timeStandardNameAttribute("standard_name", "string", hcTimeDimensionName);
        CDMAttribute timeAxisAttribute("axis", "string", hcSymbolForTimeDimension);
        cdm_->addAttribute(timeVariable.getName(), timeUnitsAttribute);
        cdm_->addAttribute(timeVariable.getName(), timeLongNameAttribute);
        cdm_->addAttribute(timeVariable.getName(), timeStandardNameAttribute);
        cdm_->addAttribute(timeVariable.getName(), timeAxisAttribute);

        // analysis time -> unique forecast reference time
        CDMVariable analysisTimeVar("analysis_time", timeDimensionDataType, std::vector<std::string>());
        DataPtr analysisTimeData = createData(timeDimensionDataType, 1);
        analysisTimeData->setValue(0, pGroup1_->analysisTime());
        analysisTimeVar.setData(analysisTimeData);
        cdm_->addVariable(analysisTimeVar);
        cdm_->addAttribute("analysis_time", CDMAttribute("units", hcTimeDimensionUnits));
        cdm_->addAttribute("analysis_time", CDMAttribute("standard_name", "forecast_reference_time"));
    }

    void MetGmCDMReaderImpl::addHorizontalDimensions()
    {
        if(cdmConfiguration_.size() == 0)
            return;

        cdmPidView& pidView = cdmConfiguration_.get<cdm_pid_index>();

        cdmPidView::iterator pIt = pidView.begin();
        for(; pIt != pidView.end(); ++pIt) if(pIt->pTags_->xTag().get() && pIt->pTags_->yTag().get()) break;

        if(pIt == pidView.end())
            throw CDMException("can't find X / Y axis");

        MetGmCDMVariableProfile profile = *pIt;

        // long and lat as dimensions on its own
        std::string xName = "longitude";
        CDMAttribute xDimLongNameAttribute = CDMAttribute("long_name", "string", "longitude");
        CDMAttribute xDimStandardNameAttribute = CDMAttribute("standard_name", "string", "longitude");
        CDMAttribute xDimUnitsAttribute = CDMAttribute("units", "string", "degree_east");
        xDim_ = CDMDimension(xName, profile.pTags_->xTag()->nx());
        std::vector<std::string> xDimShape;
        xDimShape.push_back(xDim_.getName());
        CDMVariable xVar(xName, CDM_DOUBLE, xDimShape);
        DataPtr xData = createData(CDM_DOUBLE,
                                                   profile.pTags_->xTag()->xPoints().begin(),
                                                   profile.pTags_->xTag()->xPoints().end());
        xVar.setData(xData);
        cdm_->addDimension(xDim_);
        cdm_->addVariable(xVar);
        cdm_->addAttribute(xName, xDimLongNameAttribute);
        cdm_->addAttribute(xName, xDimStandardNameAttribute);
        cdm_->addAttribute(xName, xDimUnitsAttribute);

        std::string yName = "latitude";
        CDMAttribute yDimLongNameAttribute("long_name", "string", "latitude");
        CDMAttribute yDimStandardNameAttribute("standard_name", "string", "latitude");
        CDMAttribute yDimUnitsAttribute("units", "string", "degree_north");
        yDim_ = CDMDimension(yName, profile.pTags_->yTag()->ny());
        std::vector<std::string> yDimShape;
        yDimShape.push_back(yDim_.getName());
        CDMVariable yVar(yName, CDM_DOUBLE, yDimShape);
        DataPtr yData = createData(CDM_DOUBLE,
                                                   profile.pTags_->yTag()->yPoints().begin(),
                                                   profile.pTags_->yTag()->yPoints().end());
        yVar.setData(yData);
        cdm_->addDimension(yDim_);
        cdm_->addVariable(yVar);
        cdm_->addAttribute(yName, yDimLongNameAttribute);
        cdm_->addAttribute(yName, yDimStandardNameAttribute);
        cdm_->addAttribute(yName, yDimUnitsAttribute);
    }

    void MetGmCDMReaderImpl::addVerticalDimensions()
    {
        /**
          * in artilery METGM we have 3 level types:
          * 1. pressure
          * 2. geopotential height - MSL reference
          * 3. geopotential height - GND reference
          */

        if(cdmConfiguration_.size() == 0) {
            return;
        }

        cdmPidView& pidView = cdmConfiguration_.get<cdm_pid_index>();

        for(cdmPidView::iterator pidIt = pidView.begin(); pidIt != pidView.end(); ++pidIt) {
            MetGmCDMVariableProfile profile = *pidIt;

            if(profile.p_id_ == 0
               || profile.pTags_->zTag().get() == 0
               || (profile.pTags_->hd() != MetGmHDTag::HD_3D
                   && profile.pTags_->hd() != MetGmHDTag::HD_3D_T))
            {
                continue;
            } else {
                std::string unitName;
                std::string longName;
                std::string standardName;
                if(profile.pTags_->pr() == 0) {
                    unitName = "m";
                    longName = spaceToUnderscore("height in meters above mean sea level");
                    standardName = spaceToUnderscore("height_above_reference_ellipsoid");
                } else if(profile.pTags_->pr() == 1) {
                    unitName = "m";
                    longName = spaceToUnderscore("height in meters above ground level");
                    standardName = spaceToUnderscore("height");
                } else if(profile.pTags_->pr() == 2) {
                    unitName = "hPa";
                    longName = spaceToUnderscore("heigt as pressure in hPa");
                    standardName = spaceToUnderscore("height");
                }

                // try finding if same dimension already exists
                const std::vector<CDMDimension>& dimensions = cdm_->getDimensions();
                for(size_t index = 0; index < dimensions.size(); ++index) {
                    CDMDimension dim = dimensions.at(index);
                    if(dim.getName().find(longName) == std::string::npos) {
                        continue;
                    } else {
                        const CDMVariable& var = cdm_->getVariable(dim.getName());
                        boost::shared_array<float> vertical_data = var.getData()->asFloat();
                        if(memcmp(vertical_data.get(), profile.pTags_->zTag()->points().get(), profile.pTags_->zTag()->nz() * sizeof(float)) == 0) {
                            profile.zDimensionName_ = dim.getName();
                            pidView.replace(pidIt, profile);
                            continue;
                        }
                    }
                }

                if(!profile.zDimensionName_.empty()) {
                    // z profile already existing
                    continue;
                }

                // to create unique fimex name for CDM modell
                longName.append("_pid_").append(boost::lexical_cast<std::string>(profile.p_id_));

                DataPtr data =
                        createData(CDM_FLOAT,
                                   profile.pTags_->zTag()->points().get(),
                                   profile.pTags_->zTag()->points().get() + profile.pTags_->zTag()->nz());

                CDMDimension levelDim = CDMDimension(longName, profile.pTags_->zTag()->nz());
                cdm_->addDimension(levelDim);

                std::vector<std::string> levelShape;
                levelShape.push_back(levelDim.getName());

                CDMVariable levelVar(levelDim.getName(), CDM_FLOAT, levelShape);
                cdm_->addVariable(levelVar);

                CDMAttribute levelStandardNameAttribute("standard_name", "string", standardName);
                cdm_->addAttribute(levelVar.getName(), levelStandardNameAttribute);

                CDMAttribute levelUnitsAttribute("units", "string", unitName);
                cdm_->addAttribute(levelVar.getName(), levelUnitsAttribute);

                CDMAttribute levelLongNameAttribute("long_name", "string", longName);
                cdm_->addAttribute(levelVar.getName(), levelLongNameAttribute);

                CDMAttribute levelAxisAttribute("axis", "string", "z");
                cdm_->addAttribute(levelVar.getName(), levelAxisAttribute);

                cdm_->getVariable(levelDim.getName()).setData(data);

                profile.zDimensionName_ = levelDim.getName();
                pidView.replace(pidIt, profile);
            }
        }

        return;
    }

    void MetGmCDMReaderImpl::addVariables()
    {
        cdmNameView& nameView = cdmConfiguration_.get<cdm_name_index>();
        for(cdmNameView::iterator nIt = nameView.begin(); nIt != nameView.end(); ++nIt)
        {
            MetGmCDMVariableProfile profile = *nIt;

            int p_id = profile.p_id_;

            std::string fillValue = profile.fillValue_.empty() ? std::string("9999.0") : profile.fillValue_;

            std::vector<CDMAttribute> attributes;

            CDMAttribute metgmPidAttribute("metgm_p_id", "short", boost::lexical_cast<std::string>(p_id));
            attributes.push_back(metgmPidAttribute);

            CDMAttribute cfNameAttribute("standard_name", "string", profile.standardName_);
            attributes.push_back(cfNameAttribute);

            CDMAttribute varUnitsAttribute("units", "string", profile.units_);
            attributes.push_back(varUnitsAttribute);

            CDMAttribute varFillValueAttribute("_FillValue", "float", fillValue);
            attributes.push_back(varFillValueAttribute);

            if(!profile.addOffset_.empty()) {
                CDMAttribute varAddOffsetAttribute("add_offset", "float", profile.addOffset_);
                attributes.push_back(varAddOffsetAttribute);
            }

            if(!profile.scaleFactor_.empty()) {
                CDMAttribute varScaleFactorAttribute("scale_factor", "float", profile.scaleFactor_);
                attributes.push_back(varScaleFactorAttribute);
            }

            std::vector<std::string> shape;

            shape.push_back(xDim_.getName());
            shape.push_back(yDim_.getName());

            if(p_id != 0) {

                if(!profile.zDimensionName_.empty() && cdm_->hasDimension(profile.zDimensionName_))
                    shape.push_back(profile.zDimensionName_);

                shape.push_back(tDim_.getName());
            }

            CDMVariable var(profile.cdmName_, CDM_FLOAT, shape);

            DataPtr data = createData(profile.pTags_->totalDataSize(), profile.pTags_->data());
            var.setData(data);

            cdm_->addVariable(var);

            for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(profile.cdmName_, *attrIt);
            }
        }
    }

    DataPtr MetGmCDMReaderImpl::getDataSlice(const std::string& varName, size_t unLimDimPos)
    {
//        MetGmProfilingTimerOnDestruction timer;
        CDMVariable& variable = cdm_->getVariable(varName);
        if(variable.hasData()) {
            return getDataSliceFromMemory(variable, unLimDimPos);
        } else {
            throw CDMException("variable.hasData() fails");
        }
    }

    DataPtr MetGmCDMReaderImpl::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
//        MetGmProfilingTimerOnDestruction timer;

        if(!cdm_->hasVariable(varName))
            return DataPtr();

        CDMVariable& variable = cdm_->getVariable(varName);

        /**
          * TODO: check if data exists in some cache
          */

        // field data can be x,y,level,time; x,y,level; x,y,time; x,y;
        const std::vector<std::string>& dims = variable.getShape();
        const CDMDimension* layerDim = 0;
        const CDMDimension* timeDimension = 0;
        const CDMDimension* xDimension = 0;
        const CDMDimension* yDimension = 0;

        size_t xy_size = 1;
        for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
            CDMDimension& dim = cdm_->getDimension(*it);
            if (dim.getName() != xDim_.getName() &&
                dim.getName() != yDim_.getName() &&
                !dim.isUnlimited())
            {
                    layerDim = &dim;
            }
            if(dim.isUnlimited()) {
                timeDimension = &dim;
            }
            if(dim.getName() == xDim_.getName()) {
                xDimension = &dim;
            }
            if(dim.getName() == yDim_.getName()) {
                yDimension = &dim;
            }
            if ( !dim.isUnlimited() && &dim != layerDim ) {
                xy_size *= dim.getLength();
            }
        }

        if ((!dims.empty()) && (layerDim != 0) && (timeDimension != 0)) { // 3D + T
            /**
              * implementing shortcut (can be done even easier  --- bulk read)
              * read directly whole data from file
              */

            if(!variable.hasData()) {
                // this will actually load all the data
                DataPtr data = getDataSlice(varName, 0);
            }

            assert(variable.hasData());

            DataPtr sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;

        } if ((!dims.empty()) && (layerDim != 0) && (timeDimension == 0)) { // 3D and const in time

            if(!variable.hasData()) {
                // this will actually load all the data
                DataPtr data = getDataSlice(varName, 0);
            }

            assert(variable.hasData());

            DataPtr sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;

        } else if(xDimension != 0 && yDimension != 0 && layerDim == 0 && timeDimension != 0) { // 2D + T

            throw CDMException("MetGmCDMReaderImpl getDatalSlice for 2D + T case not implemented");

        } else if(!dims.empty() && layerDim == 0 && timeDimension == 0) { // 2D - x , y

            if(!variable.hasData()) {
                DataPtr data = getDataSlice(varName, 0);
            }

            assert(variable.hasData());

            DataPtr sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;

        } else if(xDimension == 0 && yDimension == 0 && layerDim == 0 && timeDimension) {

            assert(variable.hasData());

            DataPtr sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;
        } else {
            return createData(variable.getDataType(), 0);
        }
    }

    void MetGmCDMReaderImpl::parseMgmFile(const std::string& mgmFileName)
    {
        pHandle_ = MetGmHandlePtr::createMetGmHandleForReading(mgmFileName);
        if(!(*pHandle_))
            throw CDMException(std::string("error opening metgm handle"));

        pGroup1_ = MetGmGroup1Ptr::createMetGmGroup1PtrForReading(pHandle_);

        pGroup2_ = MetGmGroup2Ptr::createMetGmGroup2PtrForReading(pHandle_);

        boost::shared_ptr<MetGmVerticalTag> prevZTag;

        for(int index = 0; index < pGroup2_->totalnp(); ++index) {

            boost::shared_ptr<MetGmTags> tags = MetGmTags::createMetGmTagsForReading(pGroup1_, pGroup2_, prevZTag);

            prevZTag = tags->zTag();

            xmlPidView &pidView = xmlConfiguration_.get<xml_pid_index>();

            std::string kildeName;
            std::string standardName;
            std::string addOffset;
            std::string scaleFactor;
            std::string strUnit(mgm_get_param_unit(tags->p_id(), *pHandle_));
            std::string fillValue;
            if(pidView.count(tags->p_id()) == 0) {

            } else if(pidView.count(tags->p_id()) == 1) {
                MetGmConfigurationMappings entry = *(pidView.find(tags->p_id()));
                kildeName = entry.cdmName_;
                standardName = entry.standardName_;
                fillValue = entry.fillValue_;
                addOffset = entry.addOffset_;
                scaleFactor = entry.scaleFactor_;
//                if(!entry.units_.empty())
//                    strUnit = entry.units_;
            } else {
                xmlPidView::iterator ic0, ic1;
                boost::tuples::tie(ic0,ic1) = pidView.equal_range(tags->p_id());
                for(; ic0 != ic1; ++ic0) {
                    if(!ic0->units_.empty() && ic0->units_ == strUnit) {
                        kildeName = ic0->cdmName_;
//                        strUnit = ic0->units_;
                        standardName = ic0->standardName_;
                        fillValue = ic0->fillValue_;
                        addOffset = ic0->addOffset_;
                        scaleFactor = ic0->scaleFactor_;
                        break;
                    }
                }
            }

            if(!kildeName.empty()) {

                std::string fixedKildeName(kildeName);

                switch(tags->pr()) {
                case 0:
                    if(!boost::algorithm::ends_with(kildeName, "MSL"))
                        fixedKildeName.append("_MSL");
                    break;
                case 1:
                    if(!boost::algorithm::ends_with(kildeName, "GND"))
                        fixedKildeName.append("_GND");
                    break;
                case 2:
                    if(!boost::algorithm::ends_with(kildeName, "Pa"))
                        fixedKildeName.append("_Pa");
                    break;
                }

                MetGmCDMVariableProfile profile(tags->p_id(), fixedKildeName, tags);
                profile.standardName_ = standardName;
                profile.units_ = strUnit;
                profile.fillValue_ = fillValue;
                profile.addOffset_ = addOffset;
                profile.scaleFactor_ = scaleFactor;
                cdmConfiguration_.insert(profile);

            }
        }
    }

    void MetGmCDMReaderImpl::sanityCheck()
    {
        // check for variables that are MSL dependant
        // but MSL itself has not been included
        std::vector<std::string> msl = cdm_->findVariables("standard_name", "altitude");
        cdmNameView& nameView = cdmConfiguration_.get<cdm_name_index>();
        if(msl.empty()) {
            const std::vector<CDMVariable>& varVec = cdm_->getVariables();
            for(size_t index = 0; index < varVec.size(); ++index) {
                if(varVec.at(index).getName().find("_MSL") != std::string::npos) {
                    cdmNameView::iterator nIt = nameView.find(varVec.at(index).getName());
                    if(nIt != nameView.end()) {
                        MGM_MESSAGE_POINT(std::string(" removing MSL dependent variable ").append(varVec.at(index).getName()).append(" because MSL(altitude) is not included by config file"))
                        nameView.erase(nIt);
                    }
                }
            }
            return;
        }
    }
}
