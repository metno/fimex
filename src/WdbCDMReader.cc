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

//#define GXDEBUG 1

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

    std::string GxWdbCDMReader::dbHost() const
    {
        return wdbExplorer()->dbHost();
    }

    std::string GxWdbCDMReader::dbName() const
    {
        return wdbExplorer()->dbName();
    }

    std::string GxWdbCDMReader::dbUser() const
    {
        return wdbExplorer()->dbUser();
    }

    std::string GxWdbCDMReader::wciUser() const
    {
        return wdbExplorer()->wciUser();
    }

    std::string GxWdbCDMReader::connectString() const
    {
        return wdbExplorer()->connectString();
    }

    unsigned int GxWdbCDMReader::dbPort() const
    {
        return wdbExplorer()->dbPort();
    }


    std::string GxWdbCDMReader::getStandardNameForDimension(const std::string& name)
    {
        // ATM there is no support in Wdb to get
        // standard CF name for given dimension
        //
        return boost::algorithm::replace_all_copy(name, " ", "_");
    }

//    void GxWdbCDMReader::setWdbToCFNamesMap(const boost::bimap<std::string, std::string>& map)
//    {
//        wdbtocfnamesmap_ = map;
//    }

    void GxWdbCDMReader::addWdbNameToCFName(const std::string& wdbname, const std::string& cfname)
    {
        wdb2cfnamesmap_[wdbname] = cfname;
        cf2wdbnamesmap_[cfname]  = wdbname;
    }

//    void GxWdbCDMReader::addWdbToCFNames(const boost::bimap<std::string, std::string>& map)
//    {
//        if(map.empty())
//            return;

//        boost::bimap<std::string, std::string>::left_const_iterator lci = map.left.begin();

//        for(; lci != map.left.end(); ++lci) {
//            addWdbNameToCFName(lci->first, lci->second);
//        }
//    }

    void GxWdbCDMReader::addWdbNameToFillValue(const std::string& wdbname, const double fillvalue)
    {
        wdbname2fillvaluemap_[wdbname] = fillvalue;
    }

//    void GxWdbCDMReader::addWdbNameToFillValueMap(const boost::bimap<std::string, double>& map)
//    {
//        if(map.empty())
//            return;

//        boost::bimap<std::string, double>::left_const_iterator lci = map.left.begin();

//        for(; lci != map.left.end(); ++lci) {
//            addWdbNameToFillValue(lci->first, lci->second);
//        }
//    }

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
//        if(!levelparameters_.empty()) {
//            // get the first one
//            GxLevelParameterRow row = levelparameters_.at(0);
//            strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
//        }

        std::string strvalidtimeconstraint;
//        if(!validtimes_.empty()) {
//            // get the first one
//            GxValidTimeRow firstRow = validtimes_.at(0);
//            boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

//            GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
//            boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

//            strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
//        }

        std::vector<std::string> vecvalueparameters;
//        if(!valueparameters_.empty()) {
//            for(unsigned int i = 0; i < valueparameters_.size(); ++i) {
//                GxValueParameterRow row = valueparameters_.at(i);
//                vecvalueparameters.push_back(row.name());
//            }
//        }

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
//        if(!providers_.empty()) {
//            for(unsigned int i = 0; i < providers_.size(); ++i) {
//                GxDataProviderRow row = providers_.at(i);
//                vecdataproviders.push_back(row.name());
//            }
//        }

        std::string strlevelparameterconstraint;
//        if(!levelparameters_.empty()) {
//            // get the first one
//            GxLevelParameterRow row = levelparameters_.at(0);
//            strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
//        }

        std::string strvalidtimeconstraint;
//        if(!validtimes_.empty()) {
//            // get the first one
//            GxValidTimeRow firstRow = validtimes_.at(0);
//            boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

//            GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
//            boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

//            strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
//        }

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

        // AAM we don't need to include level constraints
        //
        std::string strlevelparameterconstraint;
//        if(!levelparameters_.empty()) {
//            // get the first one
//            GxLevelParameterRow row = levelparameters_.at(0);
//            strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
//        }

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

        // take into accoint the reference time
        std::string strreferencetime;
        if(!referencetimes_.empty()) {
            // take the last (most fresh) value
            //
            GxReferenceTimeRow refTimeRow = referencetimes_.at(referencetimes_.size() -1);
            boost::posix_time::ptime refTime = boost::posix_time::from_time_t(refTimeRow.sinceEpochInSeconds());
            strreferencetime = "exact " + to_iso_string(refTime) + "+00";
#ifdef GXDEBUG
            std::cerr << __FUNCTION__ << " =========================== REFERENCE TIME " << strreferencetime << std::endl;
#endif
        }

        if(validtimes_.empty()) {
            wdbExplorer()->getValidTimes(vecdataproviders,
                                         strplace,
                                         strreferencetime,
                                         vecvalueparameters,
                                         strlevelparameterconstraint,
                                         std::vector<std::string>(),
                                         validtimes_);
#ifdef GXDEBUG
            std::cerr << __FUNCTION__ << " ==================================== validtimes.size() " << validtimes_.size() << std::endl;
#endif
        } else {
#ifdef GXDEBUG
            std::cerr << __FUNCTION__ << " validtimes.size() " << validtimes_.size() << std::endl;
#endif
        }

        std::string timeDimensionUnits = "seconds";
        int timeScaleFactor = 1;

        // ATM treat everytihing as seconds since epoch
        // and later make some policies that user can
        // choose from
        //
        // find miniumum validtime.. from<->to distance
//        long long validtimeminimumdelta;
//        if(validtimes_.at(0).to().sinceEpochInSeconds() != validtimes_.at(0).from().sinceEpochInSeconds())
//            validtimeminimumdelta = std::abs(validtimes_.at(0).to().sinceEpochInSeconds() - validtimes_.at(0).from().sinceEpochInSeconds());
//        else
//            validtimeminimumdelta = std::abs(validtimes_.at(0).to().sinceEpochInSeconds() - validtimes_.at(1).to().sinceEpochInSeconds());

//        for(unsigned int index = 1; index < validtimes_.size(); ++index) {
//            if(abs(validtimes_.at(index).to().sinceEpochInSeconds() - validtimes_.at(index).from().sinceEpochInSeconds()))
//                validtimeminimumdelta = std::abs(validtimes_.at(index).to().sinceEpochInSeconds() - validtimes_.at(index).from().sinceEpochInSeconds());
//        }

//        if(validtimeminimumdelta >= 24 * 60 * 60 && validtimeminimumdelta % (24 * 60 * 60) == 0) {
//            timeDimensionUnits = std::string("days");
//            timeScaleFactor = 24 * 60 * 60;
//        } else if(validtimeminimumdelta >= 60 * 60 && validtimeminimumdelta % (60 * 60) == 0) {
//            timeDimensionUnits = std::string("hours");
//            timeScaleFactor = 60 * 60;
//        } else if(validtimeminimumdelta % (60) == 0) {
//            timeDimensionUnits = std::string("minutes");
//            timeScaleFactor = 60;
//        }

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

    void GxWdbCDMReader::addReferenceTimeVariable()
    {
#ifdef GXDEBUG
        std::cerr << __FUNCTION__ << "@" << __LINE__ << " : REFERENCE TIME VARIABLE" << std::endl;
#endif
//        CDMDimension referenceTimeDimension;

        std::string hcReferenceTimeDimensionName = "forecast_reference_time";
        std::string hcReferenceTimeDimensionStandardName = "forecast_reference_time";
        std::string hcReferenceTimeDimensionType = "float";

        std::vector<std::string> vecdataproviders;
        if(!providers_.empty()) {
            for(unsigned int i = 0; i < providers_.size(); ++i) {
                GxDataProviderRow row = providers_.at(i);
                vecdataproviders.push_back(row.name());
            }
        }

        // ATM we don't need to include level constraints
        //
        std::string strlevelparameterconstraint;

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

        if(referencetimes_.empty()) {
            std::vector<GxReferenceTimeRow> tmpRefTimes;
            wdbExplorer()->
                    getReferenceTimes
                    (
                            vecdataproviders,
                            strplace,
                            std::string(),
                            vecvalueparameters,
                            strlevelparameterconstraint,
                            std::vector<std::string>(),
                            tmpRefTimes
                    );
            // only use the latest reference time (unless initialized with all)
            referencetimes_.push_back(tmpRefTimes.at(tmpRefTimes.size()-1));

#ifdef GXDEBUG
            std::cerr << __FUNCTION__ << " referencetimes.size() " << referencetimes_.size() << std::endl;
#endif
            // we need to take the latest
            // (most fresh) reference value
            GxReferenceTimeRow row = referencetimes_.at(referencetimes_.size() - 1);
            referencetimes_.clear();
            referencetimes_.push_back(row);
//            std::cerr << __FUNCTION__
//                      << " referencetimes.size() cut to "
//                      << referencetimes_.size()
//                      << std::endl;
        } else {
#ifdef GXDEBUG
            std::cerr << __FUNCTION__ << " REFERENCE TIME not empty referencetimes.size() " << referencetimes_.size() << std::endl;
#endif
        }


        std::string referenceTimeDimensionUnits = "seconds";
        int referenceTimeScaleFactor = 1;

        // ATM treat everytihing as seconds since epoch
        // and later make some policies that user can
        // choose from
        //

        // watch for the space
        referenceTimeDimensionUnits.append(" since 1970-01-01 00:00:00 +00:00");

        std::vector<double> referenceTimeInUnitsVector;

        referenceTimeInUnitsVector.resize(referencetimes_.size());
        referenceTimeVec.resize(referencetimes_.size());

        for(uint index = 0; index < referencetimes_.size(); ++index) {
            referenceTimeInUnitsVector[index] = referencetimes_[index].sinceEpochInSeconds() / referenceTimeScaleFactor;
            referenceTimeVec[index] = boost::posix_time::from_time_t(referencetimes_[index].sinceEpochInSeconds());
        }

//        referenceTimeDimension.setName("forecast_reference_time");
//        referenceTimeDimension.setLength(1);
//        cdm_->addDimension(referenceTimeDimension);

        std::vector<std::string> referenceTimeDimensionShape;
        referenceTimeDimensionShape.push_back("forecast_reference_time");
        CDMDataType referenceTimeDimensionDataType = string2datatype(hcReferenceTimeDimensionType);
        CDMVariable referenceTimeVariable(hcReferenceTimeDimensionName, referenceTimeDimensionDataType, std::vector<std::string>());

        boost::shared_ptr<Data> referenceTimeDimensionData = createData(referenceTimeDimensionDataType, referenceTimeInUnitsVector.begin(), referenceTimeInUnitsVector.end());
        referenceTimeVariable.setData(referenceTimeDimensionData);
        cdm_->addVariable(referenceTimeVariable);

        // add attributes
        CDMAttribute referenceTimeUnitsAttribute("units", "string", referenceTimeDimensionUnits);
        CDMAttribute referenceTimeLongNameAttribute("long_name", "string", hcReferenceTimeDimensionName);
        CDMAttribute referenceTimeStandardNameAttribute("standard_name", "string", hcReferenceTimeDimensionStandardName);
        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeUnitsAttribute);
        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeLongNameAttribute);
        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeStandardNameAttribute);

        return;
    }

    CDMDimension GxWdbCDMReader::addReferenceTimeDimension()
    {
#ifdef GXDEBUG
        std::cerr << __FUNCTION__ << "@" << __LINE__ << " : ================ REFERENCE" << std::endl;
#endif

        CDMDimension referenceTimeDimension;
        // let's deal with time axis: we will
        // look at all validfrom -validto pairs
        // will take them and find the minmal
        // interval there
        std::string hcReferenceTimeDimensionName = "forecast_reference_time";
        std::string hcReferenceTimeDimensionStandardName = "forecast_reference_time";
        std::string hcSymbolForReferenceTimeDimension = "T";
        std::string hcReferenceTimeDimensionType = "float";

        std::vector<std::string> vecdataproviders;
        if(!providers_.empty()) {
            for(unsigned int i = 0; i < providers_.size(); ++i) {
                GxDataProviderRow row = providers_.at(i);
                vecdataproviders.push_back(row.name());
            }
        }

        // ATM we don't need to include level constraints
        //
        std::string strlevelparameterconstraint;

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

        if(referencetimes_.empty()) {

            wdbExplorer()->
                    getReferenceTimes
                    (
                            vecdataproviders,
                            strplace,
                            std::string(),
                            vecvalueparameters,
                            strlevelparameterconstraint,
                            std::vector<std::string>(),
                            referencetimes_
                    );
//#ifdef GXDEBUG
//            std::cerr << __FUNCTION__ << " referencetimes.size() " << referencetimes_.size() << std::endl;
//#endif
            GxReferenceTimeRow row = referencetimes_.at(referencetimes_.size() - 1);
            referencetimes_.clear();
            referencetimes_.push_back(row);
            std::cerr << __FUNCTION__
                      << " referencetimes.size() cut to "
                      << referencetimes_.size()
                      << std::endl;
        } else {
//#ifdef GXDEBUG
            std::cerr << __FUNCTION__ << " REFERENCE TIME not empty referencetimes.size() " << referencetimes_.size() << std::endl;
//#endif
        }


//        std::string referenceTimeDimensionUnits = "seconds";
        int referenceTimeScaleFactor = 1;

        // ATM treat everytihing as seconds since epoch
        // and later make some policies that user can
        // choose from
        //

        // watch for the space
//        referenceTimeDimensionUnits.append(" since 1970-01-01 00:00:00 +00:00");

        std::vector<double> referenceTimeInUnitsVector;

        referenceTimeInUnitsVector.resize(referencetimes_.size());
        referenceTimeVec.resize(referencetimes_.size());

        for(uint index = 0; index < referencetimes_.size(); ++index) {
            referenceTimeInUnitsVector[index] = referencetimes_[index].sinceEpochInSeconds() / referenceTimeScaleFactor;
            referenceTimeVec[index] = boost::posix_time::from_time_t(referencetimes_[index].sinceEpochInSeconds());
        }

        long referenceTimeDimensionSize = referenceTimeVec.size();
        referenceTimeDimension.setName(hcReferenceTimeDimensionName);
        referenceTimeDimension.setLength(referenceTimeDimensionSize);

        referenceTimeDimension.setUnlimited(false);
        cdm_->addDimension(referenceTimeDimension);
        std::vector<std::string> referenceTimeDimensionShape;
        referenceTimeDimensionShape.push_back(referenceTimeDimension.getName());
        CDMDataType referenceTimeDimensionDataType = string2datatype(hcReferenceTimeDimensionType);
        CDMVariable referenceTimeVariable(hcReferenceTimeDimensionName, referenceTimeDimensionDataType, referenceTimeDimensionShape);

        boost::shared_ptr<Data> referenceTimeDimensionData = createData(referenceTimeDimensionDataType, referenceTimeInUnitsVector.begin(), referenceTimeInUnitsVector.end());
        referenceTimeVariable.setData(referenceTimeDimensionData);
        cdm_->addVariable(referenceTimeVariable);

        // add attributes
//        CDMAttribute referenceTimeUnitsAttribute("units", "string", referenceTimeDimensionUnits);
        CDMAttribute referenceTimeLongNameAttribute("long_name", "string", hcReferenceTimeDimensionName);
        CDMAttribute referenceTimeStandardNameAttribute("standard_name", "string", hcReferenceTimeDimensionStandardName);
//        CDMAttribute referenceTimeAxisAttribute("axis", "string", hcSymbolForReferenceTimeDimension);
//        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeUnitsAttribute);
        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeLongNameAttribute);
        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeStandardNameAttribute);
//        cdm_->addAttribute(referenceTimeVariable.getName(), referenceTimeAxisAttribute);

        return referenceTimeDimension;
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
#ifdef GXDEBUG
          std::cerr << __FUNCTION__  << ":" << __LINE__  << " Oops - not found?\n";
#endif
        }
        assert(!gridMappingType.empty());

        projectionName = std::string("projection_" + gridMappingType);
#ifdef GXDEBUG
        std::cerr << __FUNCTION__  << ":" << __LINE__ << " projectionName = " << projectionName << std::endl;
#endif
        // projection-variable without datatype and dimension
        CDMVariable projVar(projectionName, CDM_FLOAT, std::vector<std::string>());
        cdm_->addVariable(projVar);
#ifdef GXDEBUG
        std::cerr << __FUNCTION__  << ":" << __LINE__  << " projString = " << projStr << std::endl;
#endif
        boost::shared_ptr<Projection> projection = Projection::createByProj4(projStr);
        assert(projection.get());
#ifdef GXDEBUG
        std::cerr << __FUNCTION__  << ":" << __LINE__  << " proj4String = " << projection->getProj4String() << std::endl;
#endif
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
#ifdef GXDEBUG
          std::cerr << __FUNCTION__  << ":" << __LINE__  << " Oops - not found?\n";
#endif
        }

        // TODO:
        // must cover third possibility
        // lat-long rotated
        if(projection->isDegree()) { // check if projection is lot-lat
#ifdef GXDEBUG
            std::cerr << __FUNCTION__  << ":" << __LINE__  << " isDegree() = TRUE" << std::endl;
#endif
            // long and lat as dimensions on its own
            std::string xName;
            CDMAttribute xDimLongNameAttribute;
            CDMAttribute xDimStandardNameAttribute;
            CDMAttribute xDimUnitsAttribute;
            if(projection->getName() == "rotated_latitude_longitude") {
//                std::cerr << __FUNCTION__  << ":" << __LINE__  << " projectionName = rotated_latitude_longitude" << std::endl;
                xName = "x";
                xDimLongNameAttribute = CDMAttribute("long_name", "string", "x-coordinate in Cartesian system");
                xDimStandardNameAttribute = CDMAttribute("standard_name", "string", "grid_longitude");
                xDimUnitsAttribute = CDMAttribute("units", "string", "degree_east");
            } else {
                xName = "longitude";
                xDimLongNameAttribute = CDMAttribute("long_name", "string", "longitude");
                xDimStandardNameAttribute = CDMAttribute("standard_name", "string", "longitude");
                xDimUnitsAttribute = CDMAttribute("units", "string", "degree_east");
            }

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
            cdm_->addAttribute(xName, xDimLongNameAttribute);
            cdm_->addAttribute(xName, xDimStandardNameAttribute);
            cdm_->addAttribute(xName, xDimUnitsAttribute);

            std::string yName;
            CDMAttribute yDimLongNameAttribute;
            CDMAttribute yDimStandardNameAttribute;
            CDMAttribute yDimUnitsAttribute;
            if(projection->getName() == "rotated_latitude_longitude") {
//                std::cerr << __FUNCTION__  << ":" << __LINE__  << " isDegree() = rotated_latitude_longitude" << std::endl;
                yName = "y";
                yDimLongNameAttribute = CDMAttribute("long_name", "string", "y-coordinate in Cartesian system");
                yDimStandardNameAttribute = CDMAttribute("standard_name", "string", "grid_latitude");
                yDimUnitsAttribute = CDMAttribute("units", "string", "degree_north");
            } else {
                yName = "latitude";
                yDimLongNameAttribute = CDMAttribute("long_name", "string", "latitude");
                yDimStandardNameAttribute = CDMAttribute("standard_name", "string", "latitude");
                yDimUnitsAttribute = CDMAttribute("units", "string", "degree_north");
            }

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

            cdm_->addAttribute(yName, yDimLongNameAttribute);
            cdm_->addAttribute(yName, yDimStandardNameAttribute);
            cdm_->addAttribute(yName, yDimUnitsAttribute);
        } else {

//            std::cerr << __FUNCTION__  << ":" << __LINE__  << " isDegree() = FALSE" << std::endl;

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
//            if(!validtimes_.empty()) {
//                // get the first one
//                GxValidTimeRow firstRow = validtimes_.at(0);
//                boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

//                GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
//                boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

//                strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
//            }

            std::vector<std::string> vecvalueparameters;
//            vecvalueparameters.push_back(std::string());
//            if(!valueparameters_.empty()) {
//                for(unsigned int i = 0; i < valueparameters_.size(); ++i) {
//                    GxValueParameterRow row = valueparameters_.at(i);
//                    vecvalueparameters.push_back(row.name());
//                }
//            }

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
//          for(unsigned int index = levelparameters_.size() - 1; index >= 0; --index) {
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
            std::map<std::string, std::string>::const_iterator wdbname_iter = wdb2cfnamesmap_.find(row.name());
            if(wdbname_iter != wdb2cfnamesmap_.end())
                levelCFName = wdbname_iter->second;
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
            CDMAttribute levelAxisAttribute("axis", "string", "z");
            cdm_->addAttribute(levelVar.getName(), levelAxisAttribute);
#ifdef GXDEBUG
            std::cout << __FUNCTION__ << __LINE__ << " level long     name: " << row.name() << std::endl;
            std::cout << __FUNCTION__ << __LINE__ << " level standard name: " << levelCFName << std::endl;
            std::cout << __FUNCTION__ << __LINE__ << " level unit     name: " << row.unitName() << std::endl;
            std::cout << __FUNCTION__ << __LINE__ << " level axis     name: " << "z" << std::endl;
            std::cout << __FUNCTION__ << __LINE__ << " level data     size: " << levelSize << std::endl;
#endif
//            CDMAttribute levelPositiveAttribute("positive", "string", "up");
//            cdm_->addAttribute(levelVar.getName(), levelPositiveAttribute);

            std::vector<float> lv;
            for(unsigned int index = 0; index < levelvaluepairs.size(); ++index) {
                lv.push_back(levelvaluepairs.at(index).first);
#ifdef GXDEBUG
                std::cout << __FUNCTION__ << __LINE__ << " level : " << levelvaluepairs.at(index).first << std::endl;
#endif
            }

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
//        assert(wdbExplorer().get() == 0);

        setWdbExplorer(boost::shared_ptr<GxWdbExplorer>(new GxWdbExplorer()));

        if(!configFileName_.empty()) {
            // use XML config file information
            //
            XMLDoc doc(configFileName_);
            // try parsing wdb conneciton data
            //
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
#ifdef GXDEBUG
                                        std::cerr << "wdb dbhost: " << dbHost() << std::endl;
#endif
                                    } else if(name == std::string("dbname")) {
                                        setDbName(value);
#ifdef GXDEBUG
                                        std::cerr << "wdb dbname: " << dbName() << std::endl;
#endif
                                    } else if(name == std::string("dbuser")) {
                                        setDbUser(value);
#ifdef GXDEBUG
                                        std::cerr << "wdb dbuser: " << dbUser() << std::endl;
#endif
                                    } else if(name == std::string("dbport")) {
                                        setDbPort(value.empty() ? std::numeric_limits<unsigned int>::quiet_NaN() : boost::lexical_cast<unsigned int>(value));
#ifdef GXDEBUG
                                        std::cerr << "wdb dbport: " << dbPort() << std::endl;
#endif
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
#ifdef GXDEBUG
                                         std::cerr << __FUNCTION__ << "@" << __LINE__ << " : "
                                                   << "name: " << valueParameterNonStandardName << " cf name: " << valueParameterStandardCFName << std::endl;
#endif
                                         addWdbNameToCFName(valueParameterNonStandardName, valueParameterStandardCFName);
                                     } else if(name == std::string("_FillValue")) {
                                         valueParameterFillValue = boost::lexical_cast<float>(value);
#ifdef GXDEBUG
                                         std::cerr << __FUNCTION__ << "@" << __LINE__ << " : " << std::cerr
                                                   << "name: " << valueParameterNonStandardName << " fill value: " << valueParameterFillValue << std::endl;
#endif
                                         addWdbNameToFillValue(valueParameterNonStandardName, valueParameterFillValue);
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
#ifdef GXDEBUG
                                         std::cerr << __FUNCTION__ << "@" << __LINE__ << " : "
                                                   << std::cerr << "name: " << levelParameterNonStandardName << " cf name: " << levelParameterStandardCFName << std::endl;
#endif
                                         addWdbNameToCFName(levelParameterNonStandardName, levelParameterStandardCFName);
                                     }
                             }
                             child = child->next;
                         }
                    }
                }
            }

            {
                std::string refTimeValue;
                xmlNodePtr refTimeNode;
                {
                    XPathObjPtr xpathRefTimeObj = doc.getXPathObject("/wdb_fimex_config/wdb_parameters/reference_times/time");
                    xmlNodeSetPtr nodes = xpathRefTimeObj->nodesetval;
                    size_t size = (nodes) ? nodes->nodeNr : 0;
                    for(size_t index = 0; index < size; ++index) {
                         refTimeNode = nodes->nodeTab[index];
                         assert(refTimeNode->type == XML_ELEMENT_NODE);
                         refTimeValue = getXmlProp(refTimeNode, "value");
                         boost::posix_time::ptime referenceTime(boost::posix_time::from_iso_string(refTimeValue));
                         GxReferenceTimeRow row;
                         boost::posix_time::ptime epoch(boost::gregorian::date(1970,1,1));
                         boost::posix_time::time_duration::sec_type x = (referenceTime - epoch).total_seconds();
                         row.setSinceEpochInSeconds(x);
                         referencetimes_.push_back(row);
//#ifdef GXDEBUG
                         std::cerr << "xml config adding reference tine: " << boost::posix_time::to_iso_string(referenceTime)
                                   << " that is seconds since epoch " << row.sinceEpochInSeconds() << std::endl;
//#endif
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
#ifdef GXDEBUG
                         std::cerr << "xml config adding provider: " << row.name() << std::endl;
#endif
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
#ifdef GXDEBUG
                         std::cerr << "xml config adding place: " << row.name() << std::endl;
#endif
                    }
                }
            }
        }

        if(!source_.empty()) {
            // the source that has format
            // refTime is  given as iso string "20110210T000000"
// dbHost=<string>;dbName=<string>;dbPort=<string>;dbUser=<string>;wciUser=<string>;provider=<string>;place=<string>;refTime=<string>
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
            if(!splitmap["refTime"].empty()) {
                // override reference time that
                // may have come via config file
                //
                GxReferenceTimeRow row;
                referencetimes_.clear();
                boost::posix_time::ptime referenceTime(boost::posix_time::from_iso_string(splitmap["refTime"]));
                boost::posix_time::ptime epoch(boost::gregorian::date(1970,1,1));
                boost::posix_time::time_duration::sec_type x = (referenceTime - epoch).total_seconds();
                row.setSinceEpochInSeconds(x);
                row.setNumberOfTuples(std::numeric_limits<int>::infinity());
                referencetimes_.push_back(row);
#ifdef GXDEBUG
                std::cerr << "OVERRIDING reference time via cmd line: " << boost::posix_time::to_iso_string(referenceTime)
                          << " that is seconds since epoch " << row.sinceEpochInSeconds() << std::endl;
#endif
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
#ifdef GXDEBUG
        std::cerr << "NUMBER of LEVELS ADDED : " << levelDims.size() << std::endl;
#endif
        addGlobalCDMAttributes();

        // projection of the array (currently only one allowed)
        // get projection and coordinates
        boost::tuple<std::string, std::string> projectionTuple = addProjection(places_.at(0).name());
        std::string projectionName = projectionTuple.get<0>();
        std::string projectionCoordinates = projectionTuple.get<1>();

        // time
//        CDMDimension refTimeDim; = addReferenceTimeDimension();
        addReferenceTimeVariable();
        CDMDimension timeDim = addTimeDimension();

        addVariables(projectionName, projectionCoordinates, timeDim,/* refTimeDim,*/ levelDims);
    }

    void GxWdbCDMReader::addVariables(const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, /*const CDMDimension& referenceTimeDim,*/ const std::map<short, CDMDimension>& levelDims)
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
            // ATM we want all parameters
            // no matter level constraints
            //
            std::vector<std::string> veclevelparametersconstraints;
            veclevelparametersconstraints.push_back(std::string());

//            for(unsigned int index = 0; index < levelparameters_.size(); ++index) {
//                std::string strlevelparameterconstraint;
//                GxLevelParameterRow row = levelparameters_.at(index);
//                strlevelparameterconstraint.append("inside").append(" ").append(boost::lexical_cast<std::string>(row.from())).append(" TO ").append(boost::lexical_cast<std::string>(row.to())).append(" ").append(row.name());
//                veclevelparametersconstraints.push_back(strlevelparameterconstraint);
//            }

//            if(veclevelparametersconstraints.empty())
//                veclevelparametersconstraints.push_back(std::string());

            std::string strvalidtimeconstraint;
            // ATM we want all parameters
            // no matter time validity
            //
//            if(!validtimes_.empty()) {
//                // get the first one
//                GxValidTimeRow firstRow = validtimes_.at(0);
//                boost::posix_time::ptime fromdt = boost::posix_time::from_time_t(firstRow.from().sinceEpochInSeconds());

//                GxValidTimeRow lastRow = validtimes_.at(validtimes_.size() - 1);
//                boost::posix_time::ptime todt = boost::posix_time::from_time_t(lastRow.to().sinceEpochInSeconds());

//                strvalidtimeconstraint.append("inside").append(" ").append(boost::posix_time::to_iso_extended_string(fromdt)).append("+00").append(" TO ").append(boost::posix_time::to_iso_extended_string(todt)).append("+00");
//            }

            for(unsigned int index = 0; index < veclevelparametersconstraints.size(); ++index) {
#ifdef GXDEBUG
                std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << " finding value parameters with level contraints : " << veclevelparametersconstraints.at(index) << std::endl;
#endif
                std::vector<GxValueParameterRow> tmp;
                wdbExplorer()->getValueParameters(vecdataproviders,
                                                  strplace,
                                                  std::string(),
                                                  strvalidtimeconstraint,
                                                  veclevelparametersconstraints.at(index),
                                                  std::vector<std::string>(),
                                                  tmp);
#ifdef GXDEBUG
                std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << " found  : " << tmp.size() << std::endl;
#endif
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
#ifdef GXDEBUG
            std::cerr << "adding valueparameter [Wdb name]: " << variableWdbName << std::endl;
#endif
            double variableFillValue;
            std::map<std::string, double>::const_iterator wdbname_iter = wdbname2fillvaluemap_.find(variableWdbName);
            if(wdbname_iter != wdbname2fillvaluemap_.end())
                variableFillValue = wdbname_iter->second;
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
                    std::map<std::string, std::string>::const_iterator
                            wdbname_iter = wdb2cfnamesmap_.find(level.name());
                    if(wdbname_iter != wdb2cfnamesmap_.end())
                        levelCFName = wdbname_iter->second;
                    else
                        levelCFName = getStandardNameForDimension(level.name());
                }
                if(levelDims.find(index)->second.getName() == levelCFName) {
                    shape.push_back(levelDims.find(index)->second.getName());
//                    std::cout << "adding level parameter: " << levelCFName << " to valueparameter: " << variableWdbName << std::endl;
                }
            }

//            shape.push_back(referenceTimeDim.getName());
            shape.push_back(timeDim.getName());

            CDMDataType type = string2datatype(hcDataType);
            CDMVariable var(variableWdbName, type, shape);
            cdm_->addVariable(var);

            for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(variableWdbName, *attrIt);
            }
        }
    }

    boost::shared_ptr<Data> GxWdbCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
    {
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
#ifdef GXDEBUG
        std::cerr << "\nVARIABLE: " << varName << std::endl;
        std::cerr << "POSITION ON UNLIMITED TIME AXIS: " << unLimDimPos << std::endl;
        std::cerr << "FROM: " << to_iso_string(validTimeFrom).c_str() << std::endl;
        std::cerr << "TO: " << to_iso_string(validTimeTo).c_str() << std::endl;
#endif
        {
//            std::cerr << "===== DUMPING UNLIMITED TIME AXIS =====" << std::endl;
//            for(unsigned int position = 0; position < timeVec.size(); ++position) {
//                std::cerr
//                        << "[" << position << "] "
//                        << "FROM: "
//                        << to_iso_string(timeVec.at(position).first) << " --- "
//                        << "TO: "
//                        << to_iso_string(timeVec.at(position).second) << std::endl;
//            }
        }
        // field data can be x,y,level,time; x,y,level; x,y,time; x,y;
        const std::vector<std::string>& dims = variable.getShape();
        const CDMDimension* layerDim = 0;
        const CDMDimension* referenceTimeDim = 0;
        size_t xy_size = 1;
        for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
            CDMDimension& dim = cdm_->getDimension(*it);
            if (dim.getName() != xDim.getName() &&
                dim.getName() != yDim.getName() &&
                !dim.isUnlimited())
            {
                if(dim.getName() == std::string("forecast_reference_time"))
                    referenceTimeDim = &dim;
                else
                    layerDim = &dim;
            }
            if ( !dim.isUnlimited() && &dim != referenceTimeDim && &dim != layerDim ) {
                xy_size *= dim.getLength();
#ifdef GXDEBUG
                std::cout << "xy_size       " << xy_size << std::endl;
#endif
            }
        }

#ifdef GXDEBUG
        std::cout << "xy_size       " << xy_size << std::endl;
#endif
        // we have to extract all possible
        std::vector<std::string> vecLevels;
        if ((layerDim != 0) && (layerDim->getLength() > 0)) {
            std::string strLevel;
            std::string levelName = layerDim->getName();
#ifdef GXDEBUG
            std::cout << "finding levels for level dimension name: " << levelName << std::endl;
#endif
            std::map<std::string, std::string>::const_iterator cit = cf2wdbnamesmap_.find(levelName);
            std::string wdbLevelName = levelName;
            if(cit != cf2wdbnamesmap_.end()) {
                wdbLevelName = GxWdbCDMReader::getStandardNameForDimension(cit->second);
#ifdef GXDEBUG
                std::cout << "===================== and the WDB name is : " << wdbLevelName << std::endl;
#endif
            }

            std::vector<std::pair<double, double> > levelPairs = levelNamesToPairsMap[getStandardNameForDimension(levelName)];
            for(size_t levelIndex = 0; levelIndex < levelPairs.size(); ++levelIndex) {
                strLevel =
                        std::string("exact")
                        + std::string(" ")
                        + boost::lexical_cast<std::string>(levelPairs.at(levelIndex).first)
                        + std::string(" ")
                        + wdbLevelName;
                vecLevels.push_back(strLevel);
#ifdef GXDEBUG
                std::cout << "level # " << levelIndex << " is " << strLevel << std::endl;
#endif
            }
        }

            boost::shared_ptr<Data> levelData; // sql call for reading the data for given variable
            // prepare in data
            // and get the gid
            std::vector<std::string> dataproviders;
            dataproviders.push_back(providers_.at(0).name());
            std::vector<std::string> dataversion;
            dataversion.push_back("-1");
            std::vector<std::string> valueparameters;
            valueparameters.push_back(varName);

            std::string referenceTime;

            if( referenceTimeDim || referenceTimeVec.size() > 0 ) {
                // by default use the latest reference time
                // identified as the last index in vector
                boost::posix_time::ptime refTime = referenceTimeVec.at(referenceTimeVec.size() - 1);
                referenceTime = "exact " + to_iso_string(refTime) + "+00";
#ifdef GXDEBUG
                std::cerr << "NEWEST REFERENCE TIME: " << referenceTime << std::endl;
#endif
            }

            std::string validtimeExactFromPoint = "exact " + to_iso_string(validTimeFrom) + "+00";
            std::string validtimeExactToPoint = "exact " + to_iso_string(validTimeTo) + "+00";
            std::string validtimeFromToInterval = "inside " + to_iso_string(validTimeFrom) + "+00" + " TO " + to_iso_string(validTimeTo) + "+00";

            std::vector<GxGidRow> gids;

            for(size_t levelIndex = 0; levelIndex < vecLevels.size(); ++levelIndex) {
#ifdef GXDEBUG
                std::cout << "LEVEL: " << vecLevels.at(levelIndex) << std::endl;
#endif
                std::vector<GxGidRow> tmpGids;
                // try exact from point
                //
                wdbExplorer()->getGids(dataproviders,
                                       places_.at(0).name(),
                                       referenceTime,
                                       validtimeExactFromPoint,
                                       valueparameters,
                                       vecLevels.at(levelIndex),
                                       dataversion,
                                       tmpGids);
                if(tmpGids.empty()) {
                    // try whole interval
                    //
                    wdbExplorer()->getGids(dataproviders,
                                           places_.at(0).name(),
                                           referenceTime,
                                           validtimeFromToInterval,
                                           valueparameters,
                                           vecLevels.at(levelIndex),
                                           dataversion,
                                           tmpGids);
                    if(tmpGids.empty()) {
                        // try exact to point
                        //
                        wdbExplorer()->getGids(dataproviders,
                                               places_.at(0).name(),
                                               referenceTime,
                                               validtimeExactToPoint,
                                               valueparameters,
                                               vecLevels.at(levelIndex),
                                               dataversion,
                                               tmpGids);
                    }
                }

                if(!tmpGids.empty()) {
                    gids.push_back(tmpGids.at(0));
#ifdef GXDEBUG
                    std::cout << "============================== GID = " << tmpGids.at(0).value() << std::endl;
#endif
                }
            }

#ifdef GXDEBUG
            std::cout << "============================== GIDS SIZE = " << gids.size() << std::endl;
#endif
            size_t dataCurrentPos = 0;

            // TODO: optimize based on the actual dimension sizes....
            //

            unsigned int totalDataDimension = xy_size;
            totalDataDimension *= (layerDim != 0) ? layerDim->getLength() : 1;
            totalDataDimension *= (referenceTimeDim != 0) ? referenceTimeDim->getLength() : 1;
            boost::shared_ptr<Data> data = createData(variable.getDataType(), totalDataDimension);


            if(!gids.empty()) {
#ifdef GXDEBUG
                std::cout << "============ READING GRID DATA AS FIMEX DATA: " << std::endl;
#endif
                for(size_t gidIndex = 0; gidIndex < gids.size(); ++gidIndex) {
                    // get the data itself
#ifdef GXDEBUG
                    std::cerr << "getting data for GID = " << gids.at(gidIndex).value() << " of type " << gids.at(gidIndex).valueType() << std::endl;
#endif
                    std::stringstream strgid;
                    strgid << gids.at(gidIndex).value();

                    wdbExplorer()->getGridDataAsFimexData(strgid.str(), gids.at(gidIndex).valueType(), levelData);
                    if(levelData != 0) {
                        data->setValues(dataCurrentPos, *levelData, 0, levelData->size());
                        dataCurrentPos += levelData->size();
                    }

    //                std::cout << "============ READING GRID DATA AS FLOAT: " << std::endl;

    //                GxGridDataRow dataAsFloat;
    //                wdbExplorer()->getGridData(strgid.str(), dataAsFloat);
    //                std::ostringstream ost;
    //                for(unsigned int position = 0; position < dataAsFloat.data()->size(); position++) {
    //                    ost << dataAsFloat.data()->at(position) << "  ";
    //                    if((position / 80) == 0)
    //                        ost << std::endl;
    //                }
    //                std::cout << "============ DATA : " << std::endl << ost.str() << std::endl;
                }
#ifdef GXDEBUG
                std::cout << "============ ROW DATA SIZE: " << data->size() << std::endl;
#endif

            } else {
#ifdef GXDEBUG
                std::cout << "============ NO GIDS -> NO GRID DATA FOUND: " << std::endl;
#endif
                return createData(variable.getDataType(), 0);
            }


        return data;
    }

//    boost::shared_ptr<Data> GxWdbCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb) throw(CDMException)
//    {
//        std::vector<std::string> dimensionNames = sb.getDimensionNames();
//        std::vector<size_t> dimensionSizes = sb.getDimensionSizes();
//        std::vector<size_t> dimensionMaxSizes = sb.getMaxDimensionSizes();
//        std::vector<size_t> dimensionStartPositions = sb.getDimensionStartPositions();
//#ifdef GXDEBUG
//        for(size_t position = 0; position < dimensionNames.size(); ++position) {
//                    std::cout
//                         << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
//                         << "   dimension name            "  << dimensionNames.at(position) << std::endl
//                         << " \tdimension sizes           "  << dimensionSizes.at(position) << std::endl
//                         << " \tdimension max sizes       "  << dimensionMaxSizes.at(position) << std::endl
//                         << " \tdimension start positions "  << dimensionStartPositions.at(position)
//                         << std::endl;

//        }
//#endif
//        const CDMVariable& variable = cdm_->getVariable(varName);

//        // TODO: check if data exists in some cache
//        //

//        // find time axis -- validtime in our case
//#ifdef GXDEBUG
//        std::cerr << "\nVARIABLE: " << varName << std::endl;
//#endif
//        // field data can be x,y,level,time; x,y,level; x,y,time; x,y;
//        // -- reference time plays important role
//        //
//        const std::vector<std::string>& dims = variable.getShape();
//        const CDMDimension* timeDim = 0; // UNLIMITED DIMENSION
//        const CDMDimension* layerDim = 0;
//        const CDMDimension* referenceTimeDim = 0;
//        size_t xy_size = 1;
//        for (std::vector<std::string>::const_iterator it = dims.begin(); it != dims.end(); ++it) {
//            CDMDimension& dim = cdm_->getDimension(*it);
//            if (dim.getName() != xDim.getName() &&
//                dim.getName() != yDim.getName() &&
//                !dim.isUnlimited())
//            {
//                if(dim.getName() == std::string("forecast_reference_time")) // better if we have it
//                    referenceTimeDim = &dim;
//                else
//                    layerDim = &dim;
//            } else if(dim.isUnlimited()) {
//                // in our case thi is valid time axis
//                timeDim = &dim;
//            }
//            if ( !dim.isUnlimited() && &dim != referenceTimeDim && &dim != layerDim ) {
//                xy_size *= dim.getLength();
//#ifdef GXDEBUG
//                std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
//                          << "\t xy_size = " << xy_size
//                          << std::endl;
//#endif
//            }
//        }

//        // data we need to define for GIDs query
//        //
//        std::string strLevel;
//        size_t levelFrom = 1;
//        size_t levelTo = 1;
//        std::string referenceTime;
//        std::string validtimeExactFromPoint;
//        std::string validtimeExactToPoint;
//        std::string validtimeFromToInterval;
//        for(size_t position = 0; position < dimensionNames.size(); ++position) {
//            std::string currentDimName = dimensionNames.at(position);
//            if(timeDim !=0 && currentDimName == timeDim->getName()){
//                size_t timeIndex = dimensionStartPositions.at(position);

//                // check for out of bounds situation
//                //
//                if(timeIndex > timeDim->getLength())
//                    throw CDMException("requested time outside data-region");

//                boost::posix_time::ptime validTimeFrom = timeVec.at(timeIndex).first;
//                boost::posix_time::ptime validTimeTo = timeVec.at(timeIndex).second;

//                validtimeExactFromPoint = "exact " + to_iso_string(validTimeFrom) + "+00";
//                validtimeExactToPoint = "exact " + to_iso_string(validTimeTo) + "+00";
//                validtimeFromToInterval = "inside " + to_iso_string(validTimeFrom) + "+00" + " TO " + to_iso_string(validTimeTo) + "+00";
//#ifdef GXDEBUG
//                std::cout << "POSITION ON UNLIMITED TIME AXIS: " << timeIndex << std::endl;
//                std::cout << "FROM: " << validtimeExactFromPoint << std::endl;
//                std::cout << "TO: " << validtimeExactToPoint << std::endl;
//#endif
//            } else if(referenceTimeDim != 0 && currentDimName == referenceTimeDim->getName()) {
//                size_t referenceTimeIndex = dimensionStartPositions.at(position);

//                // check for out of bounds situation
//                //
//                if(referenceTimeIndex > referenceTimeDim->getLength())
//                    throw CDMException("requested reference_time outside data-region");

//                boost::posix_time::ptime refTime = referenceTimeVec.at(referenceTimeIndex);
//                referenceTime = "exact " + to_iso_string(refTime) + "+00";
//#ifdef GXDEBUG
//                std::cout << "reference time: " << referenceTime << std::endl;
//#endif
//            } else if(layerDim != 0 && currentDimName == layerDim->getName()) {
//                std::string levelName = layerDim->getName();
////#ifdef GXDEBUG
//                std::cout << "finding levels for level dimension with cf name: " << levelName << std::endl;
//                std::map<std::string, std::string>::const_iterator cit = cf2wdbnamesmap_.find(levelName);
//                std::string wdbLevelName = levelName;
//                if(cit != cf2wdbnamesmap_.end()) {
//                    wdbLevelName = GxWdbCDMReader::getStandardNameForDimension(cit->second);
//                    std::cout << "===================== and the WDB name is : " << wdbLevelName << std::endl;
//                }
////#endif
//                std::vector<std::pair<double, double> > levelPairs = levelNamesToPairsMap[getStandardNameForDimension(levelName)];
//                levelFrom = dimensionStartPositions.at(position);
//                levelTo = levelFrom + dimensionSizes.at(position) - 1;
//                // check for out of bounds situation
//                //
//                if(levelFrom > layerDim->getLength() || levelTo > layerDim->getLength() )
//                    throw CDMException("requested level outside data-region");

//                if(levelFrom == levelTo)
//                    strLevel =
//                        std::string("exact")
//                        + std::string(" ")
//                        + boost::lexical_cast<std::string>(levelPairs.at(levelFrom).first)
//                        + std::string(" ")
//                        + wdbLevelName;
//                else
//                    strLevel =
//                        std::string("inside")
////                        std::string("above")
//                        + std::string(" ")
//                        + boost::lexical_cast<std::string>(levelPairs.at(levelFrom).first)
//                        + std::string(" to ")
//                        + boost::lexical_cast<std::string>(levelPairs.at(levelTo).first)
//                        + std::string(" ")
//                        + wdbLevelName;
////#ifdef GXDEBUG
//                std::cerr << "level: " << strLevel << std::endl;
////#endif
//            }
//        }

//        // handle exteranly added referenceTime
//        if(referenceTime.empty() && referenceTimeVec.size() > 0) {
//            // take latest reference time identified
//            // by the last element in vector
//            boost::posix_time::ptime refTime = referenceTimeVec.at(referenceTimeVec.size() - 1);
//            referenceTime = "exact " + to_iso_string(refTime) + "+00";
//#ifdef GXDEBUG
//            std::cerr << "REFERENCE TIME: " << referenceTime << std::endl;
//#endif
//        }

//        boost::shared_ptr<Data> data = createData(variable.getDataType(), xy_size * (levelTo - levelFrom + 1));

//        size_t dataCurrentPos = 0;
//        std::vector<short> gridData;
//        gridData.reserve(xDim.getLength()*yDim.getLength());

//        // in ordinary getDataSlice we would have to run
//        // through lpossible level and get them all at once
//        //
//        boost::shared_ptr<Data> selectedLayerData; // sql call for reading the data for given variable
//        std::vector<std::string> dataproviders;
//        dataproviders.push_back(providers_.at(0).name());
//        std::vector<std::string> dataversion;
//        dataversion.push_back("-1");
//        std::vector<std::string> valueparameters;
//        valueparameters.push_back(varName);

//        std::vector<GxGidRow> gids;

//        // try exact from
//        //
//        wdbExplorer()->getGids(dataproviders,
//                               places_.at(0).name(),
//                               referenceTime,
//                               validtimeExactFromPoint,
//                               valueparameters,
//                               strLevel,
//                               dataversion,
//                               gids);
//        if(gids.empty()) {
//            // try whole interval
//            //
//            wdbExplorer()->getGids(dataproviders,
//                                   places_.at(0).name(),
//                                   referenceTime,
//                                   validtimeFromToInterval,
//                                   valueparameters,
//                                   strLevel,
//                                   dataversion,
//                                   gids);
//            if(gids.empty()) {
//                // try exact to point
//                //
//                wdbExplorer()->getGids(dataproviders,
//                                       places_.at(0).name(),
//                                       referenceTime,
//                                       validtimeExactToPoint,
//                                       valueparameters,
//                                       strLevel,
//                                       dataversion,
//                                       gids);
//            }
//        }


//        if(!gids.empty()) {
//            // get the data itself
//#ifdef GXDEBUG
//            std::cerr << "getting data for GID = " << gids.at(0).value() << " of type " << gids.at(0).valueType() << std::endl;
//#endif
//            std::stringstream strgid;
//            strgid << gids.at(0).value();
//#ifdef GXDEBUG
//            std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
//                      << "\t============ READING GRID DATA AS FIMEX DATA: " << std::endl;
//#endif
//            wdbExplorer()->getGridDataAsFimexData(strgid.str(), gids.at(0).valueType(), selectedLayerData);
//#ifdef GXDEBUG
//            std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
//                      << "\t============ SELECTED LAYER DATA size: " << selectedLayerData->size() << std::endl;
//#endif
//            if(selectedLayerData != 0) {
//                data->setValues(dataCurrentPos, *selectedLayerData, 0, selectedLayerData->size());
//                dataCurrentPos += selectedLayerData->size();
//            }
//#ifdef GXDEBUG
//            std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
//                      << "\t============ ROW DATA SIZE: " << data->size() << std::endl;

//            //                std::cout << "============ READING GRID DATA AS FLOAT: " << std::endl;

//            //                GxGridDataRow dataAsFloat;
//            //                wdbExplorer()->getGridData(strgid.str(), dataAsFloat);
//            //                std::ostringstream ost;
//            //                for(unsigned int position = 0; position < dataAsFloat.data()->size(); position++) {
//            //                    ost << dataAsFloat.data()->at(position) << "  ";
//            //                    if((position / 80) == 0)
//            //                        ost << std::endl;
//            //                }
//            //                std::cout << "============ DATA : " << std::endl << ost.str() << std::endl;
//#endif
//        } else {
//#ifdef GXDEBUG
//            std::cout << __FUNCTION__ << "@" << __LINE__ << " : " << std::endl
//                      << "\t============ NO GIDS -> NO GRID DATA FOUND: " << std::endl;
//#endif
//            return createData(variable.getDataType(), 0);
//        }

//        return data;
//    }

}
