#include "fimex/WdbCDMReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMAttribute.h"
#include "fimex/Data.h"
#include "fimex/DataImpl.h"
#include "fimex/XMLDoc.h"

// gridexer
//
#include "gridexer/GxWdbExplorer.h"

// boost
//
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

// libxml2
//
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

namespace MetNoFimex {

    void GxWdbCDMReader::setDbHost(const std::string& dbHost)
    {
        wdbExplorer()->setDbHost(dbHost);
    }

    void GxWdbCDMReader::setDbName(const std::string& dbName)
    {
        wdbExplorer()->setDbName(dbName);
    }

    void GxWdbCDMReader::setDbUser(const std::string& dbUser)
    {
        wdbExplorer()->setDbUser(dbUser);
    }

    void GxWdbCDMReader::setDbPort(const unsigned int dbPort)
    {
        wdbExplorer()->setDbPort(dbPort);
    }

    std::string GxWdbCDMReader::dbHost()
    {
        return wdbExplorer()->dbHost();
    }

    std::string GxWdbCDMReader::dbHost() const
    {
        return dbHost();
    }

    std::string GxWdbCDMReader::dbName()
    {
        return wdbExplorer()->dbName();
    }

    std::string GxWdbCDMReader::dbName() const
    {
        return dbName();
    }

    std::string GxWdbCDMReader::dbUser()
    {
        return wdbExplorer()->dbUser();
    }

    std::string GxWdbCDMReader::dbUser() const
    {
        return dbUser();
    }

    std::string GxWdbCDMReader::wciUser()
    {
        return wdbExplorer()->wciUser();
    }

    std::string GxWdbCDMReader::wciUser() const
    {
        return wciUser();
    }

    std::string GxWdbCDMReader::connectString()
    {
        return wdbExplorer()->connectString();
    }

    std::string GxWdbCDMReader::connectString() const
    {
        return connectString();
    }

    unsigned int GxWdbCDMReader::dbPort()
    {
        return wdbExplorer()->dbPort();
    }

    unsigned int GxWdbCDMReader::dbPort() const
    {
        return dbPort();
    }

    std::string GxWdbCDMReader::getStandardNameForDimension(const std::string& name)
    {
        // ATM there is no support in Wdb to get
        // standard CF name for given dimension
        //
        return boost::algorithm::replace_all_copy(name, " ", "_");
    }

    void GxWdbCDMReader::setWdbToCFNamesMap(const boost::bimap<std::string, std::string>& map)
    {
        wdbtocfnamesmap_ = map;
    }

    void GxWdbCDMReader::addWdbNameToCFName(const std::string& wdbname, const std::string& cfname)
    {
        wdbtocfnamesmap_.insert(boost::bimap<std::string, std::string>::value_type(wdbname, cfname));
    }

    void GxWdbCDMReader::addWdbToCFNames(const boost::bimap<std::string, std::string>& map)
    {
        if(map.empty())
            return;

        boost::bimap<std::string, std::string>::left_const_iterator lci = map.left.begin();

        for(; lci != map.left.end(); ++lci) {
            addWdbNameToCFName(lci->first, lci->second);
        }
    }

    void GxWdbCDMReader::addWdbNameToFillValue(const std::string& wdbname, const double fillvalue)
    {
        wdbnametofillvaluemap_.insert(boost::bimap<std::string, double>::value_type(wdbname, fillvalue));
    }

    void GxWdbCDMReader::addWdbNameToFillValueMap(const boost::bimap<std::string, double>& map)
    {
        if(map.empty())
            return;

        boost::bimap<std::string, double>::left_const_iterator lci = map.left.begin();

        for(; lci != map.left.end(); ++lci) {
            addWdbNameToFillValue(lci->first, lci->second);
        }
    }

    GxWdbCDMReader::GxWdbCDMReader(const std::string& source, const std::string& configfilename)
        : source_(source), configFileName_(configfilename), wdbExplorer_(boost::shared_ptr<GxWdbExplorer>(reinterpret_cast<GxWdbExplorer*>(0)))
    {
        try {
            init();
        } catch (std::runtime_error& exp) {
            throw CDMException(std::string("WdbCDMReader error: ") + exp.what());
        }
    }

    bool GxWdbCDMReader::addDataProvider() {
        if(!providers_.empty())
            return true;

        std::string strlevelparameterconstraint;
        if(!levelparameters_.empty()) {
            // get the first one
            GxLevelParameterRow row = levelparameters_.at(0);
            strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
        }

        std::string strvalidtimeconstraint;
        if(!validtimes_.empty()) {
            // get the first one
            GxValidTimeRow firstRow = validtimes_.at(0);
            boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

            GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
            boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

            strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
        }

        std::vector<std::string> vecvalueparameters;
        if(!valueparameters_.empty()) {
            for(unsigned int i = 0; i < valueparameters_.size(); ++i) {
                GxValueParameterRow row = valueparameters_.at(i);
                vecvalueparameters.push_back(row.name());
            }
        }

        std::string strplace;
        if(!places_.empty()) {
            GxPlaceRow row = places_.at(0);
            strplace = row.name();
        }

        // get from the database what you can
        wdbExplorer()->getDataProviders(strplace,
                                        std::string(),
                                        strvalidtimeconstraint,
                                        vecvalueparameters,
                                        strlevelparameterconstraint,
                                        std::vector<std::string>(),
                                        providers_);

        if(providers_.size() >= 1)
            std::cerr << "found more tham one provider - taking first one!" << std::endl;
        else if(providers_.size() == 0)
            assert(0);

        return true;
    }

    bool GxWdbCDMReader::addPlace() {
        if(!places_.empty())
            return true;

        std::vector<std::string> vecdataproviders;
        if(!providers_.empty()) {
            for(unsigned int i = 0; i < providers_.size(); ++i) {
                GxDataProviderRow row = providers_.at(i);
                vecdataproviders.push_back(row.name());
            }
        }

        std::string strlevelparameterconstraint;
        if(!levelparameters_.empty()) {
            // get the first one
            GxLevelParameterRow row = levelparameters_.at(0);
            strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
        }

        std::string strvalidtimeconstraint;
        if(!validtimes_.empty()) {
            // get the first one
            GxValidTimeRow firstRow = validtimes_.at(0);
            boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

            GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
            boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

            strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
        }

        std::vector<std::string> vecvalueparameters;
        if(!valueparameters_.empty()) {
            for(unsigned int i = 0; i < valueparameters_.size(); ++i) {
                GxValueParameterRow row = valueparameters_.at(i);
                vecvalueparameters.push_back(row.name());
            }
        }

        // get from the database what you can
        wdbExplorer()->getPlaces(vecdataproviders,
                                 std::string(),
                                 strvalidtimeconstraint,
                                 vecvalueparameters,
                                 strlevelparameterconstraint,
                                 std::vector<std::string>(),
                                 places_);

        return true;
    }

    void GxWdbCDMReader::addGlobalCDMAttributes()
    {
        // ATM hardcoded values
        std::string hcConventions("CF-1.0");
        std::string hcInstitution("Norwegian Meteorological Institute, met.no");

        CDMAttribute cdmConventionsAttribute("Conventions", "string", hcConventions);
        CDMAttribute cdmInstitutionAttribute("institution", "string", hcInstitution);

        // ATM we should have/get the data provider
        if(providers_.empty())
            addDataProvider();

        std::string strdataprovidername = providers_.at(0).name();

        boost::posix_time::ptime now(boost::posix_time::second_clock::universal_time());
        CDMAttribute cdmHistoryAttribute("history", "string",
                                         boost::gregorian::to_iso_extended_string(now.date()).append(" creation by fimex from ").append(connectString()).append(" , dataprovider ").append(strdataprovidername));



        CDMAttribute cdmSourceAttribute("source", "string", strdataprovidername);

        // how to extract the title ???
        CDMAttribute cdmTitleAttribute("title", "string", "none");

        // not so important --- maybe should even remove these
        CDMAttribute cdmReferencesAttribute("references", "string", "unknown");
        CDMAttribute cdmCommentAttribute("comment", "string", "none");

        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmConventionsAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmInstitutionAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmSourceAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmTitleAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmReferencesAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmCommentAttribute);
        cdm_->addAttribute(cdm_->globalAttributeNS(), cdmHistoryAttribute);
    }

    CDMDimension GxWdbCDMReader::addTimeDimension()
    {
        CDMDimension timeDimension;
        // let's deal with time axis: we will
        // look at all validfrom -validto pairs
        // will take them and find the minmal
        // interval there
        std::string hcTimeDimensionName = "time";
        std::string hcSymbolForTimeDimension = "T";
        std::string hcTimeDimensionType = "float";

        std::vector<std::string> vecdataproviders;
        if(!providers_.empty()) {
            for(unsigned int i = 0; i < providers_.size(); ++i) {
                GxDataProviderRow row = providers_.at(i);
                vecdataproviders.push_back(row.name());
            }
        }

        std::string strlevelparameterconstraint;
        if(!levelparameters_.empty()) {
            // get the first one
            GxLevelParameterRow row = levelparameters_.at(0);
            strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
        }

        std::string strplace;
        if(!places_.empty()) {
            GxPlaceRow row = places_.at(0);
            strplace = row.name();
        }


        std::vector<std::string> vecvalueparameters;
        if(!valueparameters_.empty()) {
            for(unsigned int i = 0; i < valueparameters_.size(); ++i) {
                GxValueParameterRow row = valueparameters_.at(i);
                vecvalueparameters.push_back(row.name());
            }
        }

        if(validtimes_.empty()) {
//            std::cerr << __FUNCTION__ << " validtimes.size() " << validtimes_.size() << std::endl;
            wdbExplorer()->getValidTimes(vecdataproviders,
                                         strplace,
                                         std::string(),
                                         vecvalueparameters,
                                         strlevelparameterconstraint,
                                         std::vector<std::string>(),
                                         validtimes_);
        } else {
            std::cerr << __FUNCTION__ << " validtimes.size() " << validtimes_.size() << std::endl;
        }

        std::string timeDimensionUnits = "seconds";
        int timeScaleFactor = 1;

        // find miniumum validtime.. from<->to distance
        long long validtimeminimumdelta;
        if(validtimes_.at(0).to().sinceEpochInSeconds() != validtimes_.at(0).from().sinceEpochInSeconds())
            validtimeminimumdelta = std::abs(validtimes_.at(0).to().sinceEpochInSeconds() - validtimes_.at(0).from().sinceEpochInSeconds());
        else
            validtimeminimumdelta = std::abs(validtimes_.at(0).to().sinceEpochInSeconds() - validtimes_.at(1).to().sinceEpochInSeconds());

        for(unsigned int index = 1; index < validtimes_.size(); ++index) {
            if(abs(validtimes_.at(index).to().sinceEpochInSeconds() - validtimes_.at(index).from().sinceEpochInSeconds()))
                validtimeminimumdelta = std::abs(validtimes_.at(index).to().sinceEpochInSeconds() - validtimes_.at(index).from().sinceEpochInSeconds());
        }

        if(validtimeminimumdelta >= 24 * 60 * 60 && validtimeminimumdelta % (24 * 60 * 60) == 0) {
            timeDimensionUnits = std::string("days");
            timeScaleFactor = 24 * 60 * 60;
        } else if(validtimeminimumdelta >= 60 * 60 && validtimeminimumdelta % (60 * 60) == 0) {
            timeDimensionUnits = std::string("hours");
            timeScaleFactor = 60 * 60;
        } else if(validtimeminimumdelta % (60) == 0) {
            timeDimensionUnits = std::string("minutes");
            timeScaleFactor = 60;
        }

        // watch for the space
        timeDimensionUnits.append(" since 1970-01-01 00:00:00 +00:00");

        std::vector<double> timeInUnitsVector;
        if(!validtimes_.empty()) {
             timeInUnitsVector.resize(validtimes_.size());
             timeVec.resize(validtimes_.size());

             for(uint index = 0; index < validtimes_.size(); ++index) {
                 std::time_t validtimefrom = validtimes_[index].from().sinceEpochInSeconds();
                 std::time_t validtimeto = validtimes_[index].to().sinceEpochInSeconds();
                 timeInUnitsVector[index] = validtimes_[index].from().sinceEpochInSeconds() / timeScaleFactor;
                 std::pair<boost::posix_time::ptime, boost::posix_time::ptime> validtime_from_to;
                 validtime_from_to.first = boost::posix_time::from_time_t(validtimefrom);
                 validtime_from_to.second = boost::posix_time::from_time_t(validtimeto);
                 timeVec[index] = validtime_from_to;
             }
        }

        long timeDimensionSize = timeVec.size();
        timeDimension.setName(hcTimeDimensionName);
        timeDimension.setLength(timeDimensionSize);

        // ATM we always consider that the time is UNLIMITED dimension
        timeDimension.setUnlimited(true);
        cdm_->addDimension(timeDimension);
        std::vector<std::string> timeDimensionShape;
        timeDimensionShape.push_back(timeDimension.getName());
        CDMDataType timeDimensionDataType = string2datatype(hcTimeDimensionType);
        CDMVariable timeVariable(hcTimeDimensionName, timeDimensionDataType, timeDimensionShape);

        boost::shared_ptr<Data> timeDimensionData = createData(timeDimensionDataType, timeInUnitsVector.begin(), timeInUnitsVector.end());
        timeVariable.setData(timeDimensionData);
        cdm_->addVariable(timeVariable);

        // add attributes
        CDMAttribute timeUnitsAttribute("units", "string", timeDimensionUnits);
        CDMAttribute timeLongNameAttribute("long_name", "string", hcTimeDimensionName);
        CDMAttribute timeStandardNameAttribute("standard_name", "string", hcTimeDimensionName);
        CDMAttribute timeAxisAttribute("axis", "string", hcSymbolForTimeDimension);
        cdm_->addAttribute(timeVariable.getName(), timeUnitsAttribute);
        cdm_->addAttribute(timeVariable.getName(), timeLongNameAttribute);
        cdm_->addAttribute(timeVariable.getName(), timeStandardNameAttribute);
        cdm_->addAttribute(timeVariable.getName(), timeAxisAttribute);

        return timeDimension;
    }

    boost::tuple<std::string, std::string> GxWdbCDMReader::addProjection(const std::string& strplace)
    {
        if(strplace.empty())
            assert(0);

        std::string projectionName;
        std::string projectionCoordinates;

        std::vector<GxPlaceRegularGridRow> rows;
        wdbExplorer()->getGridDescription(strplace, rows);
        GxPlaceRegularGridRow row = rows.at(0);
        std::string projStr = row.projectionDefinition();
        std::string gridMappingType;

        boost::regex projexpr( "[+]proj=([[:alnum:]_]+)[[:space:]]+" );
        boost::smatch projmatch;
        if (boost::regex_search(projStr, projmatch, projexpr)) {
            if(projmatch.size() > 1)
                gridMappingType = projmatch[1];
        } else {
          std::cerr << "Oops - not found?\n";
        }
        assert(!gridMappingType.empty());

        projectionName = std::string("projection_" + gridMappingType);

        // projection-variable without datatype and dimension
        CDMVariable projVar(projectionName, CDM_FLOAT, std::vector<std::string>());
        cdm_->addVariable(projVar);
        boost::shared_ptr<Projection> projection = Projection::createByProj4(projStr);
        assert(projection.get());
        std::vector<CDMAttribute> projAttr = projection->getParameters();
        for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
            cdm_->addAttribute(projectionName, *attrIt);
        }

        std::string projUnits = "degree";
        boost::smatch unitsmatch;
        boost::regex unitsexpr( "[+]units=([[:alnum:]]+)[[:space:]]+" );
        if (boost::regex_search(projStr, unitsmatch, unitsexpr)) {
            if(unitsmatch.size() > 1)
                projUnits = unitsmatch[1];
        } else {
          std::cerr << "Oops - not found?\n";
        }

        // TODO:
        // must cover third possibility
        // lat-long rotated
        if(projection->isDegree()) { // check if projection is lot-lat
            // long and lat as dimensions on its own
            std::string xName("longitude");
            xDim = CDMDimension(xName, row.numberX());
            CDMDataType xDataType = string2datatype("float");
            std::vector<std::string> xDimShape;
            xDimShape.push_back(xDim.getName());
            CDMVariable xVar(xName, xDataType, xDimShape);
            boost::shared_ptr<Data> xData = createData(CDM_FLOAT, row.numberX());
            for (int i = 0; i < row.numberX(); i++) {
                float value = row.startX() + i * row.incrementX();
                xData->setValue(i, value);
            }
            xVar.setData(xData);
            cdm_->addDimension(xDim);
            cdm_->addVariable(xVar);
            CDMAttribute xDimLongNameAttribute("long_name", "string", "longitude");
            CDMAttribute xDimStandardNameAttribute("standard_name", "string", "longitude");
            CDMAttribute xDimUnitsAttribute("units", "string", "degree_east");
            cdm_->addAttribute(xName, xDimLongNameAttribute);
            cdm_->addAttribute(xName, xDimStandardNameAttribute);
            cdm_->addAttribute(xName, xDimUnitsAttribute);

            std::string yName("latitude");
            yDim = CDMDimension(yName, row.numberY());
            CDMDataType yDataType = string2datatype("float");
            std::vector<std::string> yDimShape;
            yDimShape.push_back(yDim.getName());
            CDMVariable yVar(yName, yDataType, yDimShape);
            boost::shared_ptr<Data> yData = createData(CDM_FLOAT, row.numberY());
            for (int i = 0; i < row.numberY(); i++) {
                float value = row.startY() + i * row.incrementY();
                yData->setValue(i, value);
            }
            yVar.setData(yData);
            cdm_->addDimension(yDim);
            cdm_->addVariable(yVar);
            CDMAttribute yDimLongNameAttribute("long_name", "string", "latitude");
            CDMAttribute yDimStandardNameAttribute("standard_name", "string", "latitude");
            CDMAttribute yDimUnitsAttribute("units", "string", "degree_north");
            cdm_->addAttribute(yName, yDimLongNameAttribute);
            cdm_->addAttribute(yName, yDimStandardNameAttribute);
            cdm_->addAttribute(yName, yDimUnitsAttribute);
        } else {
            // ATM we'll assume that it is metric
            // long and lat as function of x, y
            std::string xName("x");
            xDim = CDMDimension(xName, row.numberX());
            CDMDataType xDataType = string2datatype("float");
            std::vector<std::string> xDimShape;
            xDimShape.push_back(xDim.getName());
            CDMVariable xVar(xName, xDataType, xDimShape);
            boost::shared_ptr<Data> xData = createData(CDM_FLOAT, row.numberX());
            for (int i = 0; i < row.numberX(); i++) {
                int value = row.startX() + i * row.incrementX();
                xData->setValue(i, value);
            }
            xVar.setData(xData);
            cdm_->addDimension(xDim);
            cdm_->addVariable(xVar);
            CDMAttribute xDimLongNameAttribute("long_name", "string", "x-coordinate in Cartesian system");
            CDMAttribute xDimStandardNameAttribute("standard_name", "string", "projection_x_coordinate");
            CDMAttribute xDimUnitsAttribute("units", "string", projUnits);
            cdm_->addAttribute(xName, xDimLongNameAttribute);
            cdm_->addAttribute(xName, xDimStandardNameAttribute);
            cdm_->addAttribute(xName, xDimUnitsAttribute);

            std::string yName("y");
            yDim = CDMDimension(yName, row.numberY());
            CDMDataType yDataType = string2datatype("float");
            std::vector<std::string> yDimShape;
            yDimShape.push_back(yDim.getName());
            CDMVariable yVar(yName, yDataType, yDimShape);
            boost::shared_ptr<Data> yData = createData(CDM_FLOAT, row.numberY());
            for (int i = 0; i < row.numberY(); i++) {
                int value = row.startY() + i * row.incrementY();
                yData->setValue(i, value);
            }
            yVar.setData(yData);
            cdm_->addDimension(yDim);
            cdm_->addVariable(yVar);
            CDMAttribute yDimLongNameAttribute("long_name", "string", "y-coordinate in Cartesian system");
            CDMAttribute yDimStandardNameAttribute("standard_name", "string", "projection_y_coordinate");
            CDMAttribute yDimUnitsAttribute("units", "string", projUnits);
            cdm_->addAttribute(yName, yDimLongNameAttribute);
            cdm_->addAttribute(yName, yDimStandardNameAttribute);
            cdm_->addAttribute(yName, yDimUnitsAttribute);
        }

        std::string longitudeName("longitude");
        std::string latitudeName("latitude");

        // add projection axes 'coordinates = "longitude latitude";
        if (xDim.getName() != longitudeName && yDim.getName() != latitudeName) {
            projectionCoordinates = longitudeName + " " + latitudeName;
            cdm_->generateProjectionCoordinates(projectionName, xDim.getName(), yDim.getName(), longitudeName, latitudeName);
        }

        return boost::make_tuple(projectionName, projectionCoordinates);

    }

    std::map<short, CDMDimension> GxWdbCDMReader::addLevelDimensions()
    {
        std::map<short, CDMDimension> levelDims;

        std::vector<std::string> vecdataproviders;
        if(!providers_.empty()) {
            for(unsigned int i = 0; i < providers_.size(); ++i) {
                GxDataProviderRow row = providers_.at(i);
                vecdataproviders.push_back(row.name());
            }
        }

        std::string strplace;
        if(!places_.empty()) {
            GxPlaceRow row = places_.at(0);
            strplace = row.name();
        }

        if(levelparameters_.empty()) {
            std::string strvalidtimeconstraint;
            if(!validtimes_.empty()) {
                // get the first one
                GxValidTimeRow firstRow = validtimes_.at(0);
                boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

                GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
                boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

                strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
            }

            std::vector<std::string> vecvalueparameters;
            if(!valueparameters_.empty()) {
                for(unsigned int i = 0; i < valueparameters_.size(); ++i) {
                    GxValueParameterRow row = valueparameters_.at(i);
                    vecvalueparameters.push_back(row.name());
                }
            }

             wdbExplorer()->getLevelParameters(vecdataproviders,
                                               strplace,
                                               std::string(),
                                               strvalidtimeconstraint,
                                               vecvalueparameters,
                                               std::vector<std::string>(),
                                               levelparameters_);
        }

        std::string hcLevelType = "float";

        for(unsigned int index = 0; index < levelparameters_.size(); ++index) {
            GxLevelParameterRow row = levelparameters_.at(index);

            // get level size from database
            std::vector<std::pair<double, double> > levelvaluepairs;
            wdbExplorer()->getLevelParameterFromToPairs(vecdataproviders.at(0), strplace, row.name(), levelvaluepairs);
            long levelSize = levelvaluepairs.size();

            CDMDataType levelDataType = string2datatype(hcLevelType);
            // we have to get proper standard name
            // in order to enable CDM model to find
            // our dimension/variable
            // search first into internal wdbtocf name map
            // if that fails try some deafault approach
            // but write that this happened in log
            std::string levelCFName;
            boost::bimap<std::string, std::string>::left_iterator left_iter = wdbtocfnamesmap_.left.find(row.name());
            if(left_iter != wdbtocfnamesmap_.left.end())
                levelCFName = left_iter->second;
            else
                levelCFName = getStandardNameForDimension(row.name());

            CDMDimension levelDim(levelCFName, levelSize);
            levelDims.insert(std::pair<short, CDMDimension>(index, levelDim));
            cdm_->addDimension(levelDim);

            std::vector<std::string> levelShape;
            levelShape.push_back(levelDim.getName());
            CDMVariable levelVar(levelCFName, levelDataType, levelShape);
            cdm_->addVariable(levelVar);

            // add attributes
            CDMAttribute levelUnitsAttribute("units", "string", row.unitName());
            cdm_->addAttribute(levelVar.getName(), levelUnitsAttribute);
            CDMAttribute levelLongNameAttribute("long_name", "string", row.name());
            cdm_->addAttribute(levelVar.getName(), levelLongNameAttribute);
            CDMAttribute levelStandardNameAttribute("standard_name", "string", levelCFName);
            cdm_->addAttribute(levelVar.getName(), levelStandardNameAttribute);

//            CDMAttribute levelPositiveAttribute("positive", "string", "up");
//            cdm_->addAttribute(levelVar.getName(), levelPositiveAttribute);

            std::vector<float> lv;
            for(unsigned int index = 0; index < levelvaluepairs.size(); ++index)
                lv.push_back(levelvaluepairs.at(index).first);

            boost::shared_ptr<Data> data;
            if (lv.size() > 0) {
                data = createData(levelDataType, lv.begin(), lv.end());
            }
            cdm_->getVariable(levelDim.getName()).setData(data);

            levelNamesToPairsMap[levelCFName] = levelvaluepairs;
        }

        return levelDims;
    }

    void GxWdbCDMReader::init() throw(CDMException)
    {
        assert(wdbExplorer().get() == 0);

        setWdbExplorer(boost::shared_ptr<GxWdbExplorer>(new GxWdbExplorer()));

        // use XML config file information
        XMLDoc doc(configFileName_);

        // try parsing wdb conneciton data
        {
            xmlNodePtr wdbConnectionNode;
            {
                XPathObjPtr xpathWdbConnectionObj = doc.getXPathObject("/wdb_fimex_config/wdb_parameters/wdb_connection");
                xmlNodeSetPtr wdbconnectionnodes = xpathWdbConnectionObj->nodesetval;
                int wdbconnectionssize = (wdbconnectionnodes) ? wdbconnectionnodes->nodeNr : 0;

                assert(wdbconnectionssize <= 1);

                if(wdbconnectionssize == 1) {
                    wdbConnectionNode = wdbconnectionnodes->nodeTab[0];
                    assert(wdbConnectionNode->type == XML_ELEMENT_NODE);
                    // fetch attributes for this node
                    //
                    xmlNodePtr child = wdbConnectionNode->children;
                    while(child != 0) {
                        if ((child->type == XML_ELEMENT_NODE) &&
                            (std::string("attribute") == std::string(reinterpret_cast<const char *>(child->name)))) {
                                std::string name = getXmlProp(child, "name");
                                std::string value = getXmlProp(child, "value");
                                if(name == std::string("dbhost")) {
                                    setDbHost(value);
                                    std::cerr << "wdb dbhost: " << dbHost() << std::endl;
                                } else if(name == std::string("dbname")) {
                                    setDbName(value);
                                    std::cerr << "wdb dbname: " << dbName() << std::endl;
                                } else if(name == std::string("dbuser")) {
                                    setDbUser(value);
                                    std::cerr << "wdb dbuser: " << dbUser() << std::endl;
                                } else if(name == std::string("dbport")) {
                                    setDbPort(value.empty() ? std::numeric_limits<unsigned int>::quiet_NaN() : boost::lexical_cast<unsigned int>(value));
                                    std::cerr << "wdb dbport: " << dbPort() << std::endl;
                                }
                        }
                        child = child->next;
                    }
                }
            }
        }

        {
            std::string valueParameterNonStandardName;
            std::string valueParameterStandardCFName;
            double valueParameterFillValue = std::numeric_limits<float>::quiet_NaN();
            xmlNodePtr valueParameterNode;
            {
                XPathObjPtr xpathValueParameterObj = doc.getXPathObject("/wdb_fimex_config/wdb_parameters/value_parameter");
                xmlNodeSetPtr nodes = xpathValueParameterObj->nodesetval;
                int size = (nodes) ? nodes->nodeNr : 0;
                for(int node_index = 0; node_index < size; ++node_index) {
                     valueParameterNode = nodes->nodeTab[node_index];
                     assert(valueParameterNode->type == XML_ELEMENT_NODE);
                     valueParameterNonStandardName = getXmlProp(valueParameterNode, "name");
                     // fetch attributes for this node
                     //
                     xmlNodePtr child = valueParameterNode->children;
                     while(child != 0) {
                         if ((child->type == XML_ELEMENT_NODE) &&
                             (std::string("attribute") == std::string(reinterpret_cast<const char *>(child->name)))) {
                                 std::string name = getXmlProp(child, "name");
                                 std::string value = getXmlProp(child, "value");
                                 if(name == std::string("standard_cf_name")) {
                                     valueParameterStandardCFName = value;
                                     addWdbNameToCFName(valueParameterNonStandardName, valueParameterStandardCFName);
                                     std::cerr << "name: " << valueParameterNonStandardName << " cf name: " << valueParameterStandardCFName << std::endl;
                                 } else if(name == std::string("_FillValue")) {
                                     valueParameterFillValue = boost::lexical_cast<float>(value);
                                     addWdbNameToFillValue(valueParameterNonStandardName, valueParameterFillValue);
                                     std::cerr << "name: " << valueParameterNonStandardName << " fill value: " << valueParameterFillValue << std::endl;
                                 }
                         }
                         child = child->next;
                     }
                }
            }
        }

        {
            std::string levelParameterNonStandardName;
            std::string levelParameterStandardCFName;
            xmlNodePtr levelParameterNode;
            {
                XPathObjPtr xpathLevelParameterObj = doc.getXPathObject("/wdb_fimex_config/wdb_parameters/level_parameter");
                xmlNodeSetPtr levelnodes = xpathLevelParameterObj->nodesetval;
                int levelnodessize = (levelnodes) ? levelnodes->nodeNr : 0;
                for(int node_index = 0; node_index < levelnodessize; ++node_index) {
                     levelParameterNode = levelnodes->nodeTab[node_index];
                     assert(levelParameterNode->type == XML_ELEMENT_NODE);
                     levelParameterNonStandardName = getXmlProp(levelParameterNode, "name");
                     // fetch attributes for this node
                     //
                     xmlNodePtr child = levelParameterNode->children;
                     while(child != 0) {
                         if ((child->type == XML_ELEMENT_NODE) &&
                             (std::string("attribute") == std::string(reinterpret_cast<const char *>(child->name)))) {
                                 std::string name = getXmlProp(child, "name");
                                 std::string value = getXmlProp(child, "value");
                                 if(name == std::string("standard_cf_name")) {
                                     levelParameterStandardCFName = value;
                                     addWdbNameToCFName(levelParameterNonStandardName, levelParameterStandardCFName);
                                     std::cerr << "name: " << levelParameterNonStandardName << " cf name: " << levelParameterStandardCFName << std::endl;
                                 }
                         }
                         child = child->next;
                     }
                }
            }
        }

        {
            std::string providerName;
            xmlNodePtr providerNode;
            {
                XPathObjPtr xpathProviderParameterObj = doc.getXPathObject("/wdb_fimex_config/wdb_parameters/data_providers/provider");
                xmlNodeSetPtr providernodes = xpathProviderParameterObj->nodesetval;
                int providernodessize = (providernodes) ? providernodes->nodeNr : 0;
                for(int provider_index = 0; provider_index < providernodessize; ++provider_index) {
                     providerNode = providernodes->nodeTab[provider_index];
                     assert(providerNode->type == XML_ELEMENT_NODE);
                     providerName = getXmlProp(providerNode, "name");
                     GxDataProviderRow row;
                     row.setName(providerName);
                     row.setNumberOfTuples(std::numeric_limits<float>::quiet_NaN());
                     providers_.push_back(row);
                     std::cerr << "xml config adding provider: " << row.name() << std::endl;
                }
            }
        }

        {
            std::string placeName;
            xmlNodePtr placeNode;
            {
                XPathObjPtr xpathPlaceParameterObj = doc.getXPathObject("/wdb_fimex_config/wdb_parameters/grid_places/place");
                xmlNodeSetPtr placenodes = xpathPlaceParameterObj->nodesetval;
                int placenodessize = (placenodes) ? placenodes->nodeNr : 0;
                for(int place_index = 0; place_index < placenodessize; ++place_index) {
                     placeNode = placenodes->nodeTab[place_index];
                     assert(placeNode->type == XML_ELEMENT_NODE);
                     placeName = getXmlProp(placeNode, "name");
                     GxPlaceRow row;
                     row.setName(placeName);
                     row.setNumberOfTuples(std::numeric_limits<float>::quiet_NaN());
                     places_.push_back(row);
                     std::cerr << "xml config adding place: " << row.name() << std::endl;
                }
            }
        }

        if(!source_.empty()) {
            // the source that has format
// dbHost=<string>;dbName=<string>;dbPort=<string>;dbUser=<string>;wciUser=<string>;provider=<string>;place=<string>
            std::vector<std::string> splitvector;
            boost::algorithm::split(splitvector, source_, boost::algorithm::is_any_of(";"));
            assert(splitvector.size() != 0);
            std::map<std::string, std::string> splitmap;
            for(unsigned int i = 0; i < splitvector.size(); ++i) {
                std::vector<std::string> subsplit;
                boost::algorithm::split(subsplit, splitvector.at(i), boost::algorithm::is_any_of("="));
                if(subsplit.size() != 2)
                    continue;
                splitmap[subsplit.at(0)] = subsplit.at(1);
            }
            setDbHost(splitmap["dbHost"]);
            setDbName(splitmap["dbName"]);
            setDbUser(splitmap["dbUser"]);
//            setWciUser(splitmap["wciName"]);
            if(!splitmap["dbPort"].empty())
                setDbPort(boost::lexical_cast<unsigned int>(splitmap["dbPort"]));
            else
                setDbPort(5432);
            if(!splitmap["provider"].empty()) {
                GxDataProviderRow row;
                row.setName(splitmap["provider"]);
                row.setNumberOfTuples(std::numeric_limits<int>::infinity());
                providers_.push_back(row);
            }
            if(!splitmap["place"].empty()) {
                GxPlaceRow row;
                row.setName(splitmap["place"]);
                row.setNumberOfTuples(std::numeric_limits<int>::infinity());
                places_.push_back(row);
            }
        }

        // lets use data --- cmd parameters have precedance
        //
        assert(wdbExplorer().get());
        wdbExplorer()->init();

        addDataProvider();

        addPlace();

        // levels
        std::map<short, CDMDimension> levelDims = addLevelDimensions();

        addGlobalCDMAttributes();

        // projection of the array (currently only one allowed)
        // get projection and coordinates
        boost::tuple<std::string, std::string> projectionTuple = addProjection(places_.at(0).name());
        std::string projectionName = projectionTuple.get<0>();
        std::string projectionCoordinates = projectionTuple.get<1>();

        // time
        CDMDimension timeDim = addTimeDimension();

        addVariables(projectionName, projectionCoordinates, timeDim, levelDims);
    }

    void GxWdbCDMReader::addVariables(const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const std::map<short, CDMDimension>& levelDims)
    {
        // ATM there is not way of determining _FillValue
        // from wdb, so we have to hard code some
        std::string hcDataType = "float";

        std::vector<std::string> vecdataproviders;
        if(!providers_.empty()) {
            for(unsigned int i = 0; i < providers_.size(); ++i) {
                GxDataProviderRow row = providers_.at(i);
                vecdataproviders.push_back(row.name());
            }
        }

        std::string strplace;
        if(!places_.empty()) {
            GxPlaceRow row = places_.at(0);
            strplace = row.name();
        }


        if(valueparameters_.empty()) {
            std::vector<std::string> veclevelpatametersconstraints;

            for(unsigned int index = 0; index < levelparameters_.size(); ++index) {
                std::string strlevelparameterconstraint;
                GxLevelParameterRow row = levelparameters_.at(index);
                strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
                veclevelpatametersconstraints.push_back(strlevelparameterconstraint);
            }
            if(veclevelpatametersconstraints.empty())
                veclevelpatametersconstraints.push_back(std::string());

            std::string strvalidtimeconstraint;
            if(!validtimes_.empty()) {
                // get the first one
                GxValidTimeRow firstRow = validtimes_.at(0);
                boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

                GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
                boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

                strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
            }

            for(unsigned int index = 0; index < veclevelpatametersconstraints.size(); ++index) {
                std::vector<GxValueParameterRow> tmp;
                wdbExplorer()->getValueParameters(vecdataproviders,
                                                  strplace,
                                                  std::string(),
                                                  strvalidtimeconstraint,
                                                  veclevelpatametersconstraints.at(index),
                                                  std::vector<std::string>(),
                                                  tmp);
                valueparameters_.insert(valueparameters_.end(), tmp.begin(), tmp.end());
            }

        }


        for(unsigned int index = 0; index < valueparameters_.size(); ++index)
        {
            GxValueParameterRow variable = valueparameters_.at(index);

//            std::string variableCFName;
//            boost::bimap<std::string, std::string>::left_iterator left_iter =
//                    wdbtocfnamesmap_.left.find(variable.valueparametername_);
//            if(left_iter != wdbtocfnamesmap_.left.end())
//                variableCFName = left_iter->second;
//            else
//                variableCFName = getStandardNameForDimension(variable.valueparametername_);

            std::string variableWdbName = variable.name();
            std::cerr << "adding valueparameter [Wdb name]: " << variableWdbName << std::endl;

            double variableFillValue;
            boost::bimap<std::string, double>::left_iterator left_iter =
                    wdbnametofillvaluemap_.left.find(variableWdbName);
            if(left_iter != wdbnametofillvaluemap_.left.end())
                variableFillValue = left_iter->second;
            else
                variableFillValue = std::numeric_limits<double>::quiet_NaN();


            std::vector<GxLevelParameterRow> levelParameters;
            wdbExplorer()->getLevelParametersForValueParameter(vecdataproviders.at(0), strplace, variableWdbName, levelParameters);
            assert(levelParameters.size() == 1);
            GxLevelParameterRow level = levelParameters.at(0);

            // spaces replaced by _
            boost::algorithm::replace_all(variableWdbName, " ", "_");

            // add the projection
            std::vector<CDMAttribute> attributes;
            CDMAttribute gridMappingAttribute("grid_mapping", projName);
            attributes.push_back(gridMappingAttribute);

            CDMAttribute varUnitsAttribute("units", "string", variable.name());
            attributes.push_back(varUnitsAttribute);

            CDMAttribute varFillValueAttribute("_FillValue", "float", boost::lexical_cast<std::string>(variableFillValue));
            attributes.push_back(varFillValueAttribute);

            if (!coordinates.empty()) {
                CDMAttribute coordinatesAttributes("coordinates", coordinates);
                attributes.push_back(coordinatesAttributes);
            }

            // map shape, generate variable, set attributes/variable to CDM (fastest moving index (x) first, slowest (unlimited, time) last
            std::vector<std::string> shape;
            shape.push_back(xDim.getName());
            shape.push_back(yDim.getName());

            for(unsigned int index = 0; index < levelDims.size(); ++index) {
                std::string levelCFName = level.cfName();
                if(levelCFName.empty()) {
                    boost::bimap<std::string, std::string>::left_iterator left_iter =
                            wdbtocfnamesmap_.left.find(level.name());
                    if(left_iter != wdbtocfnamesmap_.left.end())
                        levelCFName = left_iter->second;
                    else
                        levelCFName = getStandardNameForDimension(level.name());
                }
                if(levelDims.find(index)->second.getName() == levelCFName) {
                    shape.push_back(levelDims.find(index)->second.getName());
                    std::cerr << "adding level parameter: " << levelCFName << " to valueparameter: " << variableWdbName << std::endl;
                }
            }

            shape.push_back(timeDim.getName());

            CDMDataType type = string2datatype(hcDataType);
            CDMVariable var(variableWdbName, type, shape);
            cdm_->addVariable(var);

            for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(variableWdbName, *attrIt);
            }
        }
    }

    boost::shared_ptr<Data> GxWdbCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException) {
//        ++totalGetDataSliceCount_;
//        getDataSliceTime_.restart();
        // unLimDimPos is days since "bla bla"
        const CDMVariable& variable = cdm_->getVariable(varName);
        if (variable.hasData()) {
            return getDataSliceFromMemory(variable, unLimDimPos);
        }
        // only time can be unLimDim
        if (unLimDimPos > timeVec.size()) {
            throw CDMException("requested time outside data-region");
        }

        boost::posix_time::ptime validTimeFrom = timeVec.at(unLimDimPos).first;
        boost::posix_time::ptime validTimeTo = timeVec.at(unLimDimPos).second;

        std::cerr << "\nFROM: " << to_iso_string(validTimeFrom).c_str() << std::endl;
        std::cerr << "TO: " << to_iso_string(validTimeTo).c_str() << std::endl;

//        boost::gregorian::days oneDay(1);
//        boost::posix_time::ptime validTimeTo = validTimeFrom + oneDay;
//        qDebug() << to_iso_string(validTimeTo).c_str();

        // field data can be x,y,level,time; x,y,level; x,y,time; x,y;
        const std::vector<std::string>& dims = variable.getShape();
        const CDMDimension* layerDim = 0;
        size_t xy_size = 1;
        for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
            CDMDimension& dim = cdm_->getDimension(*it);
            if (dim.getName() != xDim.getName() &&
                dim.getName() != yDim.getName() &&
                !dim.isUnlimited())
            {
                layerDim = &dim;
            }
            if (! dim.isUnlimited()) {
                xy_size *= dim.getLength();
            }
        }

        boost::shared_ptr<Data> data = createData(variable.getDataType(), xy_size);

        // test for availability of the current time in the variable (getSlice will get data for every time)
        bool contains = true;
        // check in snoeskred if there is at all data for given number of dasy since "bla bla"

        // see how to handle timeless data
        // data like f1(x, y), f2(x,y,level) etc

        // try getting the gid
        if (!contains) {
            // return empty dataset
            return createData(variable.getDataType(), 0);
        }

        // select all available layers
        // althought we have only one
        if ((layerDim != 0) && (layerDim->getLength() > 0)) {
            for (size_t i = 0; i < layerDim->getLength(); ++i) {
//                layerVals.push_back(levelVecMap[layerDim->getName()][i]);
            }
        }

        size_t dataCurrentPos = 0;
        std::vector<short> gridData;
        gridData.reserve(xDim.getLength()*yDim.getLength());
//        for all levels
//        {
            boost::shared_ptr<Data> levelData; // sql call for reading the data for given variable
            // prepare in data
            // and get the gid
            std::vector<std::string> dataproviders;
            dataproviders.push_back(providers_.at(0).name());
//            std::string referencetime();
//            std::string levelparameter();
            std::vector<std::string> dataversion;
            dataversion.push_back("-1");
            std::vector<std::string> valueparameters;
            valueparameters.push_back(varName);
            std::string validtime = "inside " + to_iso_string(validTimeFrom) + "+00" + " TO " + to_iso_string(validTimeTo) + "+00";

            std::vector<GxGidRow> gids;
            wdbExplorer()->getGids(dataproviders,
                                   places_.at(0).name(),
                                   std::string(),
                                   validtime,
                                   valueparameters,
                                   std::string(),
                                   dataversion,
                                   gids);

            if(!gids.empty()) {
                // get the data itself
//                qDebug() << "getting data for GID = " << gids.at(0).value_ << " of type " << gids.at(0).valuetype_.c_str();
                std::stringstream strgid;
                strgid << gids.at(0).value();
                wdbExplorer()->getGridDataAsFimexData(strgid.str(), gids.at(0).valueType(), levelData);
                if(levelData != 0) {
                    data->setValues(dataCurrentPos, *levelData, 0, levelData->size());
                    dataCurrentPos += levelData->size();
                }
//                std::vector<float> floatData = wdbexplorer_.getGridData<float>(strgid.str());
//                if(!floatData.empty()) {
//                    for(int i = 0; i < floatData.size(); i++)
//                        std::cerr << floatData.at(i) << "    ";
//                    data->setValues(dataCurrentPos, *levelData, 0, levelData->size());
//                    dataCurrentPos += levelData->size();
//                }

            }


//        }
        //        }
//        totalGetDataSliceTime_ += getDataSliceTime_.elapsed();
//        boost::shared_ptr<Data> data;
        return data;
    }
}
