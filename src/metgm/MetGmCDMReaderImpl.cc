#include "../../include/metgm/MetGmCDMReaderImpl.h"

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

// private implementation details
#include "../../include/metgm/MetGmUtils.h"
#include "../../include/metgm/MetGmVersion.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"
#include "../../include/metgm/MetGmDimensionsTag.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"

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

    MetGmCDMReaderImpl::MetGmCDMReaderImpl(const std::string& metgmsource, const std::string& configfilename, const boost::shared_ptr<CDM>& cdm)
        : CDMReader(), configFileName_(configfilename)
    {
        cdm_ = cdm; // as not accesible via initialzation list

        try {
            metgmFileHandle_ = MetGmFileHandlePtr::createMetGmFileHandlePtrForReading((metgmsource));
            metgmHandle_ = MetGmHandlePtr::createMetGmHandle();
            init();
        } catch (std::runtime_error& exp) {
            throw CDMException(std::string("MetGmCDMReaderImpl error: ") + exp.what());
        }
    }

    MetGmCDMReaderImpl::~MetGmCDMReaderImpl() { }

    std::string MetGmCDMReaderImpl::dataTypeToString(short data_type)
    {
        switch(data_type) {
        case 0:
            return std::string("0"); //("0 - Climatological Data");
        case 1:
            return std::string("1"); //("1 - Numerical weather analysis");
        case 2:
            return std::string("2"); //("2 - Numerical weather prediction");
        case 3:
            return std::string("3"); //("3 - Observations");
        case 4:
            return std::string("4");  //("4 - Compound data");
        case 5:
            return std::string("5");  //("5 - REQGM");
        default:
            return std::string();
        };
        return std::string();
    }

    void MetGmCDMReaderImpl::readMetgmVersion()
    {
        metgmFileHandle_->reset();

        readMetgmHeader();

        metgmVersion_ = MetGmVersion::createMetGmVersion(mgm_get_version(*metgmHandle_));
  }

    std::string MetGmCDMReaderImpl::spaceToUnderscore(const std::string& name)
    {
        return boost::algorithm::replace_all_copy(name, " ", "_");
    }

    void MetGmCDMReaderImpl::configure(const std::auto_ptr<XMLDoc>& doc)
    {
        if(!doc.get())
            throw CDMException("Please supply xml config file the MetGmReader has to be informed how are pids mapped to actual CDM variables");

        XPathObjPtr xpathObj = doc->getXPathObject("/metgm/variable");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        size_t size = (nodes) ? nodes->nodeNr : 0;
        for (size_t i = 0; i < size; ++i) {

            xmlNodePtr node = nodes->nodeTab[i];

            std::string kildeName = getXmlProp(node, "name");
            if(kildeName.empty()) {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " kildeName empty " << std::endl;
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

            xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+kildeName+"\"]/attribute[@name=\"standard_name\"]");
            std::string str_standard_name;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_standard_name = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            if(str_standard_name.empty()) {
                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " standard name not found -> " << kildeName << std::endl;
                continue;
            } else {
//                std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " standard name for -> " << kildeName  << " is " << str_standard_name << std::endl;
            }

            xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+kildeName+"\"]/attribute[@name=\"units\"]");
            std::string str_units;
            if(xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_units = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
            }

            CDMVariable* pDummyVar = new CDMVariable(kildeName, CDM_FLOAT, std::vector<std::string>());
            MetGmConfigurationMappings cfgEntry(p_id, pDummyVar);
            cfgEntry.kildeName_ = spaceToUnderscore(kildeName);
            cfgEntry.standardName_ = spaceToUnderscore(str_standard_name);
            cfgEntry.units_ = str_units;

            xpathObj = doc->getXPathObject("/metgm/variable[@name=\""+kildeName+"\"]/attribute[@name=\"_FillValue\"]");
            std::string str_FillValue;
            if (xpathObj->nodesetval && xpathObj->nodesetval->nodeNr > 0) {
                str_FillValue = getXmlProp(xpathObj->nodesetval->nodeTab[0], "value");
                if(str_FillValue.empty()) {
                    cfgEntry.setFillValue(9999.0f);
                } else {
                    float fillValue = boost::lexical_cast<float>(str_FillValue);
                    cfgEntry.setFillValue(fillValue);
                }
            }

            xmlConfiguration_.insert(cfgEntry);
        }
    }

    void MetGmCDMReaderImpl::init() throw(CDMException)
    {
        if(!(*metgmFileHandle_))
            throw CDMException(std::string("error opening metgm file handle"));

        if(!(*metgmHandle_))
            throw CDMException(std::string("error opening metgm handle"));

        readMetgmVersion();

        if(*metgmVersion_ == MGM_EditionNONE)
            throw CDMException(std::string("can't use MGM_EditionNONE as version"));

        std::auto_ptr<XMLDoc> xmlDoc;
        if (configFileName_ == std::string()) {
            xmlDoc = std::auto_ptr<XMLDoc>(0);
        } else {
            xmlDoc = std::auto_ptr<XMLDoc>(new XMLDoc(configFileName_));
        }

        configure(xmlDoc);

        addLevelDimensions();

        CDMDimension timeDimension = addTimeDimension();

        addGlobalCDMAttributes();

        addProjection();

        addVariables(timeDimension);
    }

    void MetGmCDMReaderImpl::addGlobalCDMAttributes()
    {
        // ATM hardcoded values
        std::string hcConventions("CF-1.0");
        std::string hcInstitution("Forsvarets forskningsinstitutt, ffi.no");

        metgmFileHandle_->reset();
        readMetgmHeader();

        CDMAttribute cdmConventionsAttribute("Conventions", "string", hcConventions);
        CDMAttribute cdmInstitutionAttribute("institution", "string", hcInstitution);

        boost::posix_time::ptime now(boost::posix_time::second_clock::universal_time());

        time_t analysisT = mgm_get_analysis_date_time(*metgmHandle_);
        boost::posix_time::ptime analysisTime = boost::posix_time::from_time_t(analysisT);

        time_t startT = mgm_get_start_date_time(*metgmHandle_);
        boost::posix_time::ptime startTime = boost::posix_time::from_time_t(startT);

        std::string strHistory("");
        strHistory.append("analysis time: ").append(boost::posix_time::to_iso_extended_string(analysisTime));
        strHistory.append(" start time: ").append(boost::posix_time::to_iso_extended_string(startTime));
        strHistory.append(" created by Fimex on ");
        strHistory.append(boost::gregorian::to_iso_extended_string(now.date()));
        CDMAttribute cdmHistoryAttribute("history",
                                         "string",
                                         strHistory);

        CDMAttribute cdmSourceAttribute("source", "string", "unknown");
        if(*metgmVersion_ == MGM_Edition2) {
            cdmSourceAttribute = CDMAttribute("source", "string", std::string(mgm_get_production_nation(*metgmHandle_)).append(" ").append(mgm_get_model_type(*metgmHandle_)));
        }
        CDMAttribute cdmTitleAttribute("title", "string", metgmFileHandle_->fileName() + std::string(" ") + metgmVersion_->getAsString());
        CDMAttribute cdmReferencesAttribute("references", "string", "unknown");

        CDMAttribute cdmMetgmAnalysisDateTimeAttribute("metgm_analysis_date_time", "string", boost::posix_time::to_iso_string(analysisTime));
        CDMAttribute cdmMetgmStartDateTimeAttribute("metgm_start_date_time", "string", boost::posix_time::to_iso_string(startTime));
        CDMAttribute cdmMetgmVersionAttribute("metgm_version", "string", spaceToUnderscore(metgmVersion_->getAsString()));
        CDMAttribute cdmMetgmDataTypeAttribute("metgm_data_type", "string", dataTypeToString(mgm_get_data_type(*metgmHandle_)));
        CDMAttribute cdmMetgmFreeTextAttribute("metgm_free_text", "string", mgm_get_free_text(*metgmHandle_));

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
                << "<metgm_meta_data>"
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

        if(*metgmVersion_ == MGM_Edition2) {
            CDMAttribute cdmMetgmProductionNationAttribute = CDMAttribute("metgm_production_nation", "string", std::string(mgm_get_production_nation(*metgmHandle_)));
            cdm_->addAttribute(cdm_->globalAttributeNS(), cdmMetgmProductionNationAttribute);
            CDMAttribute cdmMetgmModelTypeAttribute = CDMAttribute("metgm_model_type", "string", std::string(mgm_get_model_type(*metgmHandle_)));
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

        metgm_comment << "</metgm_meta_data>";

        CDMAttribute cdmCommentAttribute("comment", "string", metgm_comment.str());
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmCommentAttribute);
    }

    CDMDimension MetGmCDMReaderImpl::addTimeDimension()
    {
        CDMDimension timeDimension;

        /**
          * we can have only one time axis
          * loop all 7 important params
          * find max time distance from start date
          * and minimal time step
          */

        std::string hcTimeDimensionName = "time";
        std::string hcSymbolForTimeDimension = "T";
        std::string hcTimeDimensionUnits = "seconds since 1970-01-01 00:00:00 +00:00";

        metgmFileHandle_->reset();
        readMetgmHeader();

        boost::shared_ptr<MetGmGroup3Ptr> pg3 = MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

        /**
          * in seconds since epoch
          */
        time_t startT = mgm_get_start_date_time(*metgmHandle_);

        boost::posix_time::ptime startDateTime = boost::posix_time::from_time_t(startT);
        boost::posix_time::ptime epochDateTime(boost::gregorian::date(1970,1,1));

        size_t  timePointNumber = 0;
        double  deltaTime = 0;
        double  maxTimeSpan = 0;

        size_t np = mgm_get_number_of_params(*metgmHandle_);

        for(size_t gp3Index = 0; gp3Index < np; ++gp3Index) {
            int error = mgm_read_next_group3(*metgmFileHandle_, *metgmHandle_, *pg3);
            short p_id = pg3->p_id();
            if(p_id == 0) {
                // special case on non-temporal values
                continue;
            }

            if(error == MGM_ERROR_GROUP3_NOT_FOUND) {
                std::cerr << __FUNCTION__ << __LINE__ << " for gp3Index " << gp3Index << " MGM_ERROR_GROUP3_NOT_FOUND " << std::endl;
                continue;
            } else if(error != MGM_OK) {
                assert(error);
            } else if(error == MGM_OK) {

                int numOfSteps = pg3->nt();
                double timeStep = pg3->dt();

                if(timeStep * numOfSteps > maxTimeSpan) {
                    maxTimeSpan = timeStep * numOfSteps;
                    timePointNumber = numOfSteps;
                    deltaTime = timeStep;
                }
            }
            mgm_skip_group4(*metgmFileHandle_, *metgmHandle_);
            mgm_skip_group5(*metgmFileHandle_, *metgmHandle_);
        }

        std::vector<double> timeInUnitsVector;

        boost::posix_time::time_duration diff = startDateTime - epochDateTime;
        double startSinceEpochInSeconds = diff.total_seconds();
        for(size_t index = 0; index < timePointNumber; ++index) {
            timeInUnitsVector.push_back(startSinceEpochInSeconds + index * deltaTime);
            timeVec_.push_back(boost::posix_time::from_time_t(timeInUnitsVector[index]));
        }


        long timeDimensionSize = timeVec_.size();
        timeDimension.setName(hcTimeDimensionName);
        timeDimension.setLength(timeDimensionSize);

        // ATM we always consider that the time is UNLIMITED dimension
        timeDimension.setUnlimited(true);
        cdm_->addDimension(timeDimension);
        std::vector<std::string> timeDimensionShape;
        timeDimensionShape.push_back(timeDimension.getName());
        CDMDataType timeDimensionDataType = CDM_DOUBLE;
        CDMVariable timeVariable(hcTimeDimensionName, timeDimensionDataType, timeDimensionShape);

        boost::shared_ptr<Data> timeDimensionData = createData(timeDimensionDataType, timeInUnitsVector.begin(), timeInUnitsVector.end());
        timeVariable.setData(timeDimensionData);
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

        // analysis time is unique forecast reference time
        time_t analysis_time_t = mgm_get_analysis_date_time(*metgmHandle_);
        std::string analysisTimeName = "analysis_time";
        std::string analysisTimeStandardName = "forecast_reference_time";
        std::vector<std::string> nullShape;
        CDMVariable analysisTimeVar(analysisTimeName, timeDimensionDataType, nullShape);
        boost::shared_ptr<Data> analysisTimeData = createData(timeDimensionDataType, 1);
        analysisTimeData->setValue(0, analysis_time_t);
        analysisTimeVar.setData(analysisTimeData);
        cdm_->addVariable(analysisTimeVar);
        cdm_->addAttribute(analysisTimeName, CDMAttribute("units", hcTimeDimensionUnits));
        cdm_->addAttribute(analysisTimeName, CDMAttribute("standard_name", analysisTimeStandardName));

        return timeDimension;
    }

    boost::tuple<std::string, std::string> MetGmCDMReaderImpl::addProjection()
    {
        std::string projectionName;
        std::string projectionCoordinates;
        std::string projStr = "+proj=latlong +datum=WGS84";
        std::string gridMappingType;

        metgmFileHandle_->reset();
        readMetgmHeader();

        /**
          * As x and y will be same for all except pid = 0
          * grab the group 3 values for first pid > 1
          */
        boost::shared_ptr<MetGmGroup3Ptr> pg3 = MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

        int nx = 0; // long
        int ny = 0; // lat
        double dx = 0;
        double dy = 0;
        double cx = 0; // center long
        double cy = 0; // center lat
        size_t pid = 1;
        for(; pid < 8; ++pid) {
            if(mgm_read_next_group3(*metgmFileHandle_, *metgmHandle_, *pg3) == 0) {
                nx = pg3->nx();
                ny = pg3->ny();
                dx = pg3->dx();
                dy = pg3->dy();
                cx = pg3->cx();
                cy = pg3->cy();
            } else {
                /**
                  * handle errors
                  */
            }
        }

        double longitudeSpanDegrees = dx * (nx - 1);
        double latitudeSpanDegrees = dy * (ny - 1);

        boost::regex projexpr( "[+]proj=([[:alnum:]_]+)[[:space:]]+" );
        boost::smatch projmatch;
        if (boost::regex_search(projStr, projmatch, projexpr)) {
            if(projmatch.size() > 1)
                gridMappingType = projmatch[1];
        } else {
            throw CDMException("creating grid mapping type failed");
        }

        if(gridMappingType.empty())
            throw CDMException("creating grid mapping type failed");

        projectionName = std::string("projection_" + gridMappingType);
        // projection-variable without datatype and dimension
//        CDMVariable projVar(projectionName, CDM_FLOAT, std::vector<std::string>());
//        cdm_->addVariable(projVar);

        boost::shared_ptr<Projection> projection = Projection::createByProj4(projStr);
        if(projection.get() == 0)
            throw CDMException("create by proj4 failed");

//        std::vector<CDMAttribute> projAttr = projection->getParameters();
//        for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
//            cdm_->addAttribute(projectionName, *attrIt);
//        }

        std::string projUnits = "degree";
        boost::smatch unitsmatch;
        boost::regex unitsexpr( "[+]units=([[:alnum:]]+)[[:space:]]+" );
        if (boost::regex_search(projStr, unitsmatch, unitsexpr)) {
            if(unitsmatch.size() > 1)
                projUnits = unitsmatch[1];
        }

        if(projection->isDegree()) { // check if projection is lot-lat
            // long and lat as dimensions on its own
            std::string xName = "longitude";
            CDMAttribute xDimLongNameAttribute = CDMAttribute("long_name", "string", "longitude");
            CDMAttribute xDimStandardNameAttribute = CDMAttribute("standard_name", "string", "longitude");
            CDMAttribute xDimUnitsAttribute = CDMAttribute("units", "string", "degree_east");

            xDim_ = CDMDimension(xName, nx);
            CDMDataType xDataType = string2datatype("float");
            std::vector<std::string> xDimShape;
            xDimShape.push_back(xDim_.getName());
            CDMVariable xVar(xName, xDataType, xDimShape);
            boost::shared_ptr<Data> xData = createData(CDM_FLOAT, nx);

            double startX = cx - longitudeSpanDegrees / 2.0;

            for (int i = 0; i < nx; i++) {
                double value = startX + i * dx;
                xData->setValue(i, value);
            }
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

            yDim_ = CDMDimension(yName, ny);
            CDMDataType yDataType = string2datatype("float");
            std::vector<std::string> yDimShape;
            yDimShape.push_back(yDim_.getName());
            CDMVariable yVar(yName, yDataType, yDimShape);
            boost::shared_ptr<Data> yData = createData(CDM_FLOAT, ny);

            double startY = cy - latitudeSpanDegrees / 2.0;

            for (int i = 0; i < ny; i++) {
                double value = startY + i * dy;
                yData->setValue(i, value);
            }
            yVar.setData(yData);
            cdm_->addDimension(yDim_);
            cdm_->addVariable(yVar);

            cdm_->addAttribute(yName, yDimLongNameAttribute);
            cdm_->addAttribute(yName, yDimStandardNameAttribute);
            cdm_->addAttribute(yName, yDimUnitsAttribute);
        } else {
            throw CDMException("projection units not degrees");
        }

        return boost::make_tuple(projectionName, projectionCoordinates);

    }

    void MetGmCDMReaderImpl::readMetgmHeader()
    {
        metgmHandle_->reset();
        MGM_THROW_ON_ERROR(mgm_read_header(*metgmFileHandle_, *metgmHandle_));
    }

    void MetGmCDMReaderImpl::addLevelDimensions()
    {
        /**
          * in artilery METGM we have basicaly 3 levels:
          * pressure or geopotential height - with MSL or GND reference
          */

        std::string hcLevelType = "float";

        metgmFileHandle_->reset();
        readMetgmHeader();

        boost::shared_ptr<MetGmGroup3Ptr> pg3 = MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);
        if(pg3 == 0)
            throw CDMException("mgm_new_group3() failed");
        /**
          * number of parameters
          */
        size_t np = mgm_get_number_of_params(*metgmHandle_);
        METGM_ZProfile prevZProfile;
        for(size_t gp3Index = 0; gp3Index < np; ++gp3Index) {

            int error = mgm_read_next_group3(*metgmFileHandle_, *metgmHandle_, *pg3);

            if(error == MGM_ERROR_GROUP3_NOT_FOUND) {
                continue;
            } else if(error != MGM_OK) {
                assert(error);
                return;
            }

            int p_id = pg3->p_id();

            if(p_id == 0 || p_id == 1) {
                /**
                  * when skipping read its groups 3,4,5
                  * to move the pointer properly forward
                  */

                continue;
            }


            int nz = pg3->nz();
            int pz = pg3->pz();
            int pr = pg3->pr();

            if(pz == 0) {
                if(!prevZProfile.isValid()) {
                    throw CDMException("addLevelDimension : pz==0 : prevZProfile not valid");
                } else {
                    prevZProfile.pid_ = p_id;
                    prXpidXname_.insert(prevZProfile);
                }
            } else if(pz == 1 || pz == 0) {

               float *pg4Data = new float[nz];
               error = mgm_read_group4(*metgmFileHandle_, *metgmHandle_, pg4Data);
               if(error != MGM_OK) {
                   std::cerr << "p_id:" << p_id << " error " << error << std::endl;
                   return;
               }

                    boost::shared_ptr<Data> data;
                    CDMDataType levelDataType = string2datatype(hcLevelType);

                    std::string unitName;
                    std::string longName;
                    std::string standardName;
                    if(pr == 0) {
                        unitName = "m";
                        longName = spaceToUnderscore("height in meters above mean sea level");
                        standardName = spaceToUnderscore("height_above_reference_ellipsoid");
                    } else if(pr == 1) {
                        unitName = "m";
                        longName = spaceToUnderscore("height in meters above ground level");
                        standardName = spaceToUnderscore("height");
                        /**
                          * maybe we have add values MSL (pid=0)
                          */
                    } else if(pr == 2) {
                        unitName = "hPa";
                        longName = spaceToUnderscore("pressure in hPa");
                        standardName = spaceToUnderscore("height");
                    }


                    /**
                      * try finding if there already exists
                      * some pid with same vertical profile
                      */

                    std::string exisitng_z_profile_name;
                    const std::vector<CDMDimension>& refDimVec = cdm_->getDimensions();

                    for(size_t dimIndex = 0; dimIndex < refDimVec.size(); ++dimIndex) {
                        CDMDimension existing_dim = refDimVec.at(dimIndex);
                        exisitng_z_profile_name = existing_dim.getName();
                        if(boost::algorithm::contains(exisitng_z_profile_name, longName)) {
                            CDMVariable z_exisitng_variable  = cdm_->getVariable(exisitng_z_profile_name);
                            /**
                              * take and compare data
                              */
                            int cmpResult = memcmp(pg4Data, z_exisitng_variable.getData()->getDataPtr(), nz * sizeof(float));
                            if( cmpResult == 0) {
                                break;
                            } else if(cmpResult > 0) {
                                exisitng_z_profile_name.clear();
                                continue;
                            } else {
                                exisitng_z_profile_name.clear();
                                continue;
                            }
                        } else {
                            exisitng_z_profile_name.clear();
                            continue;
                        }
                    }

                    if(exisitng_z_profile_name.empty()) {
                        data = createData(levelDataType, pg4Data, pg4Data + nz);

                        longName.append("_").append(boost::lexical_cast<std::string>(p_id));
                        CDMDimension levelDim = CDMDimension(longName, nz);
                        cdm_->addDimension(levelDim);

                        std::vector<std::string> levelShape;
                        /**
                          * shape might be dependable of x,y (if pr=1)
                          */
                        levelShape.push_back(levelDim.getName());
                        CDMVariable levelVar(levelDim.getName(), levelDataType, levelShape);
                        cdm_->addVariable(levelVar);

                        CDMAttribute levelStandardNameAttribute("standard_name", "string", standardName);
                        cdm_->addAttribute(levelVar.getName(), levelStandardNameAttribute);

                        CDMAttribute levelUnitsAttribute("units", "string", unitName);
                        cdm_->addAttribute(levelVar.getName(), levelUnitsAttribute);

                        CDMAttribute levelLongNameAttribute("long_name", "string", longName);
                        cdm_->addAttribute(levelVar.getName(), levelLongNameAttribute);

                        CDMAttribute levelAxisAttribute("axis", "string", "z");
                        cdm_->addAttribute(levelVar.getName(), levelAxisAttribute);

                        CDMAttribute levelPositiveAttribute("positive", "string", "up");
                        cdm_->addAttribute(levelVar.getName(), levelPositiveAttribute);

                        cdm_->getVariable(levelDim.getName()).setData(data);

                        METGM_ZProfile zProfile(levelDim.getName(), pr, p_id);

                        prXpidXname_.insert(zProfile);

                        prevZProfile = zProfile;

                    } else {
                        METGM_ZProfile zProfile(exisitng_z_profile_name, pr, p_id);

                        prXpidXname_.insert(zProfile);

                        prevZProfile = zProfile;
                    }

                    delete [] pg4Data;

                } else if(pz == 2) {
                    assert(0);
                } else {
                    assert(0);
                }

        }

        return;
    }

    void MetGmCDMReaderImpl::addVariables(const CDMDimension& timeDimension)
    {
        std::string hcDataType = "float";

        metgmFileHandle_->reset();
        readMetgmHeader();
        /**
          * number of parameters
          */
        size_t np = mgm_get_number_of_params(*metgmHandle_);

        std::cerr << __FUNCTION__ << __LINE__ << " np in mgm file " << np << std::endl;

        xmlPidView &pidView = xmlConfiguration_.get<xml_pid_index>();
        xmlPidView::iterator cit = pidView.begin();
        for(; cit != pidView.end(); ++cit) {
            std::cerr << __FUNCTION__ << " @ " << __LINE__
                      << " pid: " << cit->p_id_
                      << " name: " << cit->kildeName_
                      << " standard name: " << (cit->standardName_.empty() ? "NULL" : cit->standardName_)
                      << " units: " << (cit->units_.empty() ? "NULL" : cit->units_)
                      << std::endl;
        }


        for(size_t gp3Index  = 0; gp3Index < np; ++gp3Index)
        {
            boost::shared_ptr<MetGmGroup3Ptr> pg3 = MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

            int error = mgm_read_next_group3(*metgmFileHandle_, *metgmHandle_, *pg3);

            if(error == MGM_ERROR_GROUP3_NOT_FOUND) {
                std::cerr << __FUNCTION__ << __LINE__ << " for gp3Index " << gp3Index << " MGM_ERROR_GROUP3_NOT_FOUND " << std::endl;
                continue;
            } else if(error != MGM_OK) {
                assert(error);
            }

            int p_id = pg3->p_id();

            std::cerr << __FUNCTION__ << __LINE__ << " adding variable for pid " << p_id << std::endl;

            xmlPidView::iterator ic0, ic1;
            xmlPidView &pidView = xmlConfiguration_.get<xml_pid_index>();
            boost::tuples::tie(ic0,ic1) = pidView.equal_range(p_id);

            if(ic0 == pidView.end()) {
                std::cerr << __FUNCTION__ << __LINE__ << " pid NOT found in xmlConfiguration " << std::endl;
                continue;
            }

            std::string cfgName         = ic0->kildeName_;
            std::string cfgStandardName = ic0->standardName_;
            std::string cfgUnits        = ic0->units_;
            float       cfgFillValue    = ic0->fillValue_.get() ? *(ic0->fillValue_) : 9999.0f;

            std::string mgmUnits(mgm_get_param_unit(p_id, *metgmHandle_));

            if(p_id == 7) {
                bool bExactMatch = false;
                do {
                    std::cerr << __FUNCTION__ << __LINE__ << " checking units cfgName  " << ic0->kildeName_ << std::endl;
                    if(!ic0->units_.empty()) {
                        Units checker;
                        if(checker.areConvertible(mgmUnits, ic0->units_)) {
                            cfgName         = ic0->kildeName_;
                            cfgStandardName = ic0->standardName_;
                            cfgUnits        = ic0->units_;
                            cfgFillValue    = ic0->fillValue_.get() ? *(ic0->fillValue_) : 9999.0f;
                            bExactMatch = true;
                            break;
                        }
                    }
                    ++ic0;
                } while (ic0 != ic1);

                if(!bExactMatch)
                    continue;
            }

            if(cfgUnits.empty()) cfgUnits = mgmUnits;

            std::cerr << __FUNCTION__ << __LINE__ << " units from metgm  " << cfgUnits << std::endl;
            std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << " metno variable name found -> " << cfgName << std::endl;

            /**
              * we might have more pressure/geopotential
              * but with different reference level
              * we will include this in variable name
              */
            short pr = pg3->pr();
            switch(pr) {
            case 0:
                if(!boost::algorithm::ends_with(cfgName, "MSL"))
                    cfgName.append("_MSL");
                break;
            case 1:
                if(!boost::algorithm::ends_with(cfgName, "GND"))
                    cfgName.append("_GND");
                break;
            case 2:
                if(!boost::algorithm::ends_with(cfgName, "Pa"))
                    cfgName.append("_Pa");
                break;
            }

            std::vector<CDMAttribute> attributes;

            /**
              * we will add metgm p_id as variable's attribute
              * even though it is not supported by CF standard
              */
            CDMAttribute metgmPidAttribute("metgm_p_id", "short", boost::lexical_cast<std::string>(p_id));
            attributes.push_back(metgmPidAttribute);

            CDMAttribute cfNameAttribute("standard_name", "string", cfgStandardName);
            attributes.push_back(cfNameAttribute);

//            CDMAttribute gridMappingAttribute("grid_mapping", "string", projName);
//            attributes.push_back(gridMappingAttribute);

            CDMAttribute varUnitsAttribute("units", "string", cfgUnits);
            attributes.push_back(varUnitsAttribute);

            CDMAttribute varFillValueAttribute("_FillValue", "float", boost::lexical_cast<std::string>(cfgFillValue));
            attributes.push_back(varFillValueAttribute);

            // map shape, generate variable, set attributes/variable to CDM (fastest moving index (x) first, slowest (unlimited, time) last
            std::vector<std::string> shape;

            /**
              * TODO: maybe hard core this to long amd lat
              */
            shape.push_back(xDim_.getName());
            shape.push_back(yDim_.getName());

            /**
              * p_id = 0 is only (x, y) that is (lon, lat) dependent
              */
            if(p_id != 0) {
                metgm_profile_set::iterator it = prXpidXname_.find(boost::make_tuple(pr, p_id));
                if(it != prXpidXname_.end()) {
                    METGM_ZProfile zProfile = *it;
                    std::string levelName = zProfile.name_;

                    if(cdm_->hasDimension(levelName)) {
                        CDMDimension levelDimension = cdm_->getDimension(levelName);
                        shape.push_back(levelDimension.getName());
                    }
                }

                shape.push_back(timeDimension.getName());

//                if (!coordinates.empty()) {
//                    CDMAttribute coordinatesAttributes("coordinates", coordinates);
//                    attributes.push_back(coordinatesAttributes);
//                }
            }

            CDMDataType type = string2datatype(hcDataType);
            CDMVariable var(cfgName, type, shape);
            cdm_->addVariable(var);
            cdmvariable2mgm_group3map_.insert(std::make_pair<std::string, boost::shared_ptr<MetGmGroup3Ptr> >(cfgName, pg3));

            for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(cfgName, *attrIt);
            }
        }
    }

    boost::shared_ptr<Data> MetGmCDMReaderImpl::getDataSlice(const std::string& varName, size_t unLimDimPos)
    {
        short p_id = -1;
        CDMAttribute metgmPid;
        CDMVariable& variable = cdm_->getVariable(varName);
        if(cdmvariable2mgm_group3map_.find(varName) != cdmvariable2mgm_group3map_.end()) {
            if(cdm_->getAttribute(varName, "metgm_p_id", metgmPid)) {
                boost::shared_ptr<Data> ptrPid = metgmPid.getData();
                p_id = ptrPid->asConstShort()[0];
            }
        }

        /**
          * TODO: Fimex needs elaborate caching
          */
        if(variable.hasData()) {
            return getDataSliceFromMemory(variable, unLimDimPos);
        } else {
            if(p_id == -1)
                return MetNoFimex::createData(CDM_FLOAT, 0);
        }
        // only time can be unLimDim
        if (unLimDimPos > timeVec_.size()) {
            throw CDMException("requested time outside data-region");
        }

        boost::shared_ptr<MetGmGroup3Ptr> initialPg3 = cdmvariable2mgm_group3map_.find(varName)->second;

        metgmFileHandle_->reset();
        readMetgmHeader();

        // read group3 data until you match with initialPg3
        // in order to honor data reading sequence
        boost::shared_ptr<MetGmGroup3Ptr> fwdPg3 = MetGmGroup3Ptr::createMetGmGroup3Ptr(metgmHandle_);

        MGM_THROW_ON_ERROR(mgm_read_this_group3(*metgmFileHandle_, *metgmHandle_, p_id, *fwdPg3))

        std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
                  << " pVar->Name " << variable.getName() << " dumping gp3"
                  << std::endl;
        fwdPg3->dump();

        if(*initialPg3 != 0) {
            while(fwdPg3->neq(initialPg3)) {
                MGM_THROW_ON_ERROR(mgm_read_next_group3(*metgmFileHandle_, *metgmHandle_, *fwdPg3))
            }
        }

        if(fwdPg3 == 0 && p_id >=0)
            throw CDMException("fwdPg3 is null");

        // field data can be x,y,level,time; x,y,level; x,y,time; x,y;
        const CDMDimension* tDimension = 0;
        if(!cdm_->getTimeAxis(varName).empty()) {
            tDimension = &(cdm_->getDimension(cdm_->getTimeAxis(varName).c_str()));
            assert(tDimension->isUnlimited());
        }

        const CDMDimension* zDimension = 0;
        if(!cdm_->getVerticalAxis(varName).empty()) {
            zDimension = &(cdm_->getDimension(cdm_->getVerticalAxis(varName).c_str()));
        }

        const CDMDimension* xDimension = 0;
        if(!cdm_->getHorizontalXAxis(varName).empty()) {
            xDimension = &(cdm_->getDimension(cdm_->getHorizontalXAxis(varName).c_str()));
        }

        const CDMDimension* yDimension = 0;
        if(!cdm_->getHorizontalYAxis(varName).empty()) {
            yDimension = &(cdm_->getDimension(cdm_->getHorizontalYAxis(varName).c_str()));
        }

        size_t xy_size = 1;
        if(xDimension)
            xy_size *= xDimension->getLength();
        if(yDimension)
            xy_size *= yDimension->getLength();

        size_t sliceDataDimension = (zDimension != 0) ? zDimension->getLength() * xy_size : xy_size;
        /**
          * reading group5 data is done as whole
          * all time slices at once (hdp = 3D + T)
          */
        size_t totalDataDimension = (tDimension != 0) ? tDimension->getLength() * sliceDataDimension : sliceDataDimension;

        boost::shared_ptr<Data> data = createData(CDM_FLOAT, 0);

        MetGmHDTag::MetGmHD dimensionality = MetGmHDTag::HD_0D;
        if(xDimension != 0 && yDimension != 0 && zDimension != 0 && tDimension != 0) {
            dimensionality = MetGmHDTag::HD_3D_T;
        } else if(xDimension != 0 && yDimension != 0 && zDimension == 0 && tDimension != 0) {
            dimensionality = MetGmHDTag::HD_2D_T;
        } else if(xDimension == 0 && yDimension == 0 && zDimension == 0 && tDimension != 0) {
            dimensionality = MetGmHDTag::HD_0D_T;
        } else if(xDimension != 0 && yDimension != 0 && zDimension != 0 && tDimension == 0) {
            dimensionality = MetGmHDTag::HD_3D;
        } else if(xDimension != 0 && yDimension != 0 && zDimension == 0 && tDimension == 0) {
            dimensionality = MetGmHDTag::HD_2D;
        }

        switch(dimensionality) {

        case MetGmHDTag::HD_0D_T:
            {
                throw CDMException("time data should already be set");
                break;
            }


        case MetGmHDTag::HD_2D:
            {
                CDMAttribute metgmPid = cdm_->getAttribute(variable.getName(), "metgm_p_id");

                boost::shared_ptr<Data> ptrPid = metgmPid.getData();

                int p_id = ptrPid->asConstShort()[0];

                boost::scoped_array<float> pg5(new float[totalDataDimension]);

                short pz = fwdPg3->pz();
                if(pz > 0) {
                    MGM_THROW_ON_ERROR(mgm_skip_group4(*metgmFileHandle_, *metgmHandle_))
                }

                MGM_THROW_ON_ERROR(mgm_read_group5(*metgmFileHandle_, *metgmHandle_, pg5.get()))
                MGM_THROW_ON_ERROR(mgm_param_is_convertible(p_id, *metgmVersion_))

                data = MetNoFimex::createData(CDM_FLOAT, pg5.get(), pg5.get() + totalDataDimension);
                variable.setData(data);

                break;
            }

        case MetGmHDTag::HD_3D_T:
            {
                CDMAttribute metgmPid = cdm_->getAttribute(variable.getName(), "metgm_p_id");

                boost::shared_ptr<Data> ptrPid = metgmPid.getData();

                int p_id = ptrPid->asConstShort()[0];

                boost::scoped_array<float> pg5(new float[totalDataDimension]);
                boost::scoped_array<float> pg5T(new float[totalDataDimension]);

                short pz = fwdPg3->pz();

                if(pz > 0) {
                    MGM_THROW_ON_ERROR(mgm_skip_group4(*metgmFileHandle_, *metgmHandle_))
                }

                /**
                  * get the data for all time slices at once
                  * and then extract one we actually need
                  */
                MGM_THROW_ON_ERROR(mgm_read_group5(*metgmFileHandle_, *metgmHandle_, pg5.get()))
                MGM_THROW_ON_ERROR(mgm_param_is_convertible(p_id, *metgmVersion_))

                float* slice = pg5.get();
                float* sliceT = pg5T.get();
                const size_t maxXindex = xDim_.getLength();
                const size_t maxYindex = yDim_.getLength();
                const size_t maxZindex = zDimension->getLength();
                const size_t numOfSlices = tDimension->getLength();

                for(size_t sIndex = 0; sIndex < numOfSlices; ++sIndex) {

                    slice = pg5.get() + sIndex * sliceDataDimension;
                    sliceT = pg5T.get() + sIndex * sliceDataDimension;

                    for(size_t z_index = 0; z_index < maxZindex; ++z_index) {

                        for(size_t y_index = 0; y_index < maxYindex; ++y_index) {
                            for(size_t x_index = 0; x_index < maxXindex; ++x_index) {
                                sliceT[z_index * (maxYindex * maxXindex) + y_index * maxXindex + x_index] =
                                        slice[z_index + x_index * maxZindex + y_index * (maxZindex * maxXindex)];
                            } // x_index
                        } // y_index
                    } // z_index

                } // sliceIndex

                /**
                  * we will load all data
                  */
                data = MetNoFimex::createData(CDM_FLOAT, pg5T.get(), pg5T.get() + totalDataDimension);
                variable.setData(data);

                assert(variable.hasData());

                return getDataSliceFromMemory(variable, unLimDimPos);

                break;
            }
        case MetGmHDTag::HD_0D:
        case MetGmHDTag::HD_3D:
        case MetGmHDTag::HD_2D_T:
        default:
            {
                throw CDMException("MetGmCDMReaderImpl getDatalSlice for given dimensionality not implemented");
                break;
            }
        }

        return data;
    }

    boost::shared_ptr<Data> MetGmCDMReaderImpl::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
        if(!cdm_->hasVariable(varName))
            return boost::shared_ptr<Data>();

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
                boost::shared_ptr<Data> data = getDataSlice(varName, 0);
            }

            assert(variable.hasData());

            boost::shared_ptr<Data> sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;

        } if ((!dims.empty()) && (layerDim != 0) && (timeDimension == 0)) { // 3D and const in time

            if(!variable.hasData()) {
                // this will actually load all the data
                boost::shared_ptr<Data> data = getDataSlice(varName, 0);
            }

            assert(variable.hasData());

            boost::shared_ptr<Data> sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;

        } else if(xDimension != 0 && yDimension != 0 && layerDim == 0 && timeDimension != 0) { // 2D + T

            throw CDMException("MetGmCDMReaderImpl getDatalSlice for 2D + T case not implemented");

        } else if(!dims.empty() && layerDim == 0 && timeDimension == 0) { // 2D - x , y

            if(!variable.hasData()) {
                boost::shared_ptr<Data> data = getDataSlice(varName, 0);
            }

            assert(variable.hasData());

            boost::shared_ptr<Data> sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;

        } else if(xDimension == 0 && yDimension == 0 && layerDim == 0 && timeDimension) {

            assert(variable.hasData());

            boost::shared_ptr<Data> sliceData =
                    variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());

            return sliceData;
        } else {
            return createData(variable.getDataType(), 0);
        }
    }

}
