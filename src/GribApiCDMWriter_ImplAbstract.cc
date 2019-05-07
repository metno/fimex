/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "GribApiCDMWriter_ImplAbstract.h"

#include "fimex/CDM.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/GribUtils.h"
#include "fimex/TimeUnit.h"
#include "fimex/Units.h"
#include "fimex/Utils.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include <boost/date_time/posix_time/posix_time.hpp>

#include <grib_api.h>

#include <libxml/tree.h>
#include <libxml/xpath.h>

#include <algorithm>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>

namespace MetNoFimex
{

/** helper classes Scale and UnScale to transform double vectors */
class Scale : public std::unary_function<std::string, bool>
{
    const double scale_factor;
    const double add_offset;
public:
    Scale(double scale_factor, double add_offset) : scale_factor(scale_factor), add_offset(add_offset) {}
    virtual ~Scale() {}
    virtual double operator() (double x) const {return x*scale_factor + add_offset;}
};

class UnScale : public Scale
{
public:
    UnScale(double scale_factor, double add_offset) : Scale(1/scale_factor, -1 * add_offset/scale_factor) {}
    virtual ~UnScale() {}
};

GribApiCDMWriter_ImplAbstract::GribApiCDMWriter_ImplAbstract(int gribVersion, CDMReader_p cdmReader, const std::string& outputFile, const std::string& configFile)
: CDMWriter(cdmReader, outputFile), gribVersion(gribVersion), configFile(configFile), xmlConfig(new XMLDoc(configFile))
{
    logger = getLogger("fimex.GribApi_CDMWriter");

    {
        std::string templXPath("/cdm_gribwriter_config/template_file");
        XPathObjPtr xPObj = xmlConfig->getXPathObject(templXPath);
        xmlNodeSetPtr nodes = xPObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size == 1) {
            const std::string gribTemplate = getXmlProp(nodes->nodeTab[0], "name");
            int error;
            FILE* fh = std::fopen(gribTemplate.c_str(), "r");
            if (!fh)
                throw CDMException("unable to open grib template file '" + gribTemplate + "', error is:" + std::strerror(errno));
            gribHandle = boost::shared_ptr<grib_handle>(grib_handle_new_from_file(0, fh, &error), grib_handle_delete);
            if (!gribHandle)
                throw CDMException("unable to create grib handle for template '" + gribTemplate + "'");
        } else {
            const std::string gribTemplate("GRIB" + type2string(gribVersion));
            gribHandle = boost::shared_ptr<grib_handle>(grib_handle_new_from_samples(0, gribTemplate.c_str()), grib_handle_delete);
            if (!gribHandle)
                throw CDMException("unable to create grib handle from samples '" + gribTemplate + "'");
        }
    }
    // check the file
    {
        std::ios_base::openmode mode = std::ios::binary|std::ios::out;
        std::string templXPath("/cdm_gribwriter_config/output_file[@type]");
        XPathObjPtr xPObj = xmlConfig->getXPathObject(templXPath);
        xmlNodeSetPtr nodes = xPObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        if (size > 0) {
            std::string openMode = getXmlProp(nodes->nodeTab[0], "type");
            if (openMode == "append") {
                // append or create
                gribFile.open(outputFile.c_str(), mode | std::ios::app);
                if (gribFile.is_open()) {
                    LOG4FIMEX(logger, Logger::DEBUG, "opened '"<<outputFile<<"' in append-mode");
                } else {
                    gribFile.open(outputFile.c_str(), mode);
                    LOG4FIMEX(logger, Logger::DEBUG, "created '"<<outputFile<<"' in append-mode");
                }
            } else {
                gribFile.open(outputFile.c_str(), mode);
                LOG4FIMEX(logger, Logger::DEBUG, "opened '"<<outputFile<<"' in overwrite-mode");
            }
        } else {
            gribFile.open(outputFile.c_str(), mode);
            LOG4FIMEX(logger, Logger::DEBUG, "opened '"<<outputFile<<"' in overwrite-mode");
        }
        if (!gribFile.is_open())
            throw CDMException("Cannot write grib-file: "+outputFile);
    }
}

GribApiCDMWriter_ImplAbstract::~GribApiCDMWriter_ImplAbstract()
{
}

void GribApiCDMWriter_ImplAbstract::run()
{
    using namespace std;
    using namespace boost::posix_time;
    LOG4FIMEX(logger, Logger::DEBUG, "GribApiCDMWriter_ImplAbstract::run()  " );

    // default to grid_second_order
//    string pType("grid_second_order");
//    try {
//        size_t pTypeSize = pType.size();
//        GRIB_CHECK(grib_set_string(gribHandle.get(), "packingType", pType.c_str(), &pTypeSize), "setting endStep");
//    } catch (...) {
//        LOG4FIMEX(logger, Logger::WARN, "unable to set packingType to " << pType );
//    }
    setGlobalAttributes();

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmReader);
    const CDM& cdm = cdmReader->getCDM();
    const CDM::VarVec& vars = cdm.getVariables();
    set<string> usedVariables;
    for (vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt = coordSys.begin();
            varSysIt != coordSys.end();
            ++varSysIt) {

        if ((*varSysIt)->isSimpleSpatialGridded()) {
            vector<string> csVars;
            // iterator over all variables
            for (CDM::VarVec::const_iterator vi = vars.begin(); vi != vars.end(); ++vi) {
                if (CompleteCoordinateSystemForComparator(vi->getName())(*varSysIt)) {
                    std::string usedXPath;
                    if (hasNodePtr(vi->getName(), usedXPath)) {
                        csVars.push_back(vi->getName());
                    } else {
                        LOG4FIMEX(logger, Logger::WARN, "cannot write variable " << vi->getName() << ": not declared in config-file " << configFile);
                    }
                }
            }
            if (csVars.empty()) continue;
            usedVariables.insert(csVars.begin(), csVars.end());
            try {
                // TODO: this would be nicer with varSysIt->getProjection() instead of csVars[0]
                setProjection(csVars.at(0));
            } catch (CDMException& e) {
                LOG4FIMEX(logger, Logger::WARN, "cannot write variable " << join(csVars.begin(), csVars.end(), ", ") << " due to projection problems: " << e.what());
                continue;
            }

            CoordinateSystem::ConstAxisPtr xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
            CoordinateSystem::ConstAxisPtr yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
            CoordinateSystem::ConstAxisPtr zAxis = (*varSysIt)->getGeoZAxis(); // Z
            CoordinateSystem::ConstAxisPtr tAxis = (*varSysIt)->getTimeAxis(); // time
            string stepUnit = "s";

            CoordinateSystemSliceBuilder sb(cdm, *varSysIt);
            sb.setAll(xAxis->getName());
            sb.setAll(yAxis->getName());
            // handling of time
            vector<ptime> refTimes;
            vector<FimexTime> vTimes;
            if (tAxis.get() != 0) {
                // time-Axis, eventually multi-dimensional, i.e. forecast_reference_time
                if ((*varSysIt)->hasAxisType(CoordinateAxis::ReferenceTime)) {
                    CoordinateSystem::ConstAxisPtr rtAxis = (*varSysIt)->findAxisOfType(CoordinateAxis::ReferenceTime);
                    DataPtr refTimesD = cdmReader->getScaledDataInUnit(rtAxis->getName(),"seconds since 1970-01-01 00:00:00");
                    boost::shared_array<unsigned long long> refs = refTimesD->asUInt64();
                    /* do something with the refTimes and select the wanted Position */
                    size_t refTimePos = 0; /* or whatever you select between 0 (default) and refTimes->size()-1 */
                    sb.setReferenceTimePos(refTimePos);
                    TimeUnit tu("seconds since 1970-01-01 00:00:00");
                    transform(&refs[0],
                            &refs[0] + refTimesD->size(),
                            back_inserter(refTimes),
                            bind1st(mem_fun_ref(&TimeUnit::unitTime2posixTime), tu));
                } else {
                    try {
                        refTimes.push_back(getUniqueForecastReferenceTime(cdmReader));
                    } catch (CDMException& ex) {
                        refTimes.push_back(not_a_date_time);
                    }
                }
                stringstream ss;
                if (refTimes.at(0) == not_a_date_time) {
                    ss << "seconds since 1970-01-01 00:00:00";
                } else {
                    time_facet* facet (new time_facet("%Y-%m-%d %H:%M:%S"));
                    ss.imbue(locale(ss.getloc(), facet));
                    ss << "seconds since " << refTimes[0];
                }
                DataPtr times = cdmReader->getScaledDataSliceInUnit(tAxis->getName(), ss.str(), sb.getTimeVariableSliceBuilder());
                boost::shared_array<long long> timesA = times->asInt64();
                TimeUnit tu(ss.str());
                transform(&timesA[0],
                        &timesA[0] + times->size(),
                        back_inserter(vTimes),
                        bind1st(mem_fun_ref(&TimeUnit::unitTime2fimexTime), tu));
                if (times->size() > 1) {
                    stepUnit = gribSeconds2stepUnits(timesA[1] - timesA[0]);
                }
                /* select the first startTime and the size for the time-dimension */
                sb.setTimeStartAndSize(0, 1); // default is all of ReferenceTimePos
            } else {
                // no times found
                try {
                    ptime refTime = getUniqueForecastReferenceTime(cdmReader);
                    refTimes.push_back(refTime);
                    vTimes.push_back(FimexTime(refTime.date().year(), refTime.date().month(), refTime.date().day(), refTime.time_of_day().hours(), refTime.time_of_day().minutes(), refTime.time_of_day().seconds()));
                } catch (CDMException& ex) {
                    refTimes.push_back(not_a_date_time);
                    vTimes.push_back(FimexTime(FimexTime::min_date_time));
                }
            }

            // fetch the levels from first variable
            std::vector<double> levels = getLevels(csVars.at(0));
            if (zAxis.get() != 0) {
                sb.setStartAndSize(zAxis->getName(), 0, 1);
            }

            // TODO: handle ensembles
            // reducing all unset dimensions to the first slice
            vector<string> dims = sb.getUnsetDimensionNames();
            for (vector<string>::iterator dim = dims.begin(); dim != dims.end(); ++dim) {
                LOG4FIMEX(logger, Logger::WARN, "unknown dimension in grib-writer: '" << *dim << "' using first slice" )
                sb.setStartAndSize(*dim, 0, 1);
            }

            // loops over ref-times, times, variables and levels
            map<string, string> variableWarnings;
            size_t rtPos = 0;
            for (vector<ptime>::iterator rTime = refTimes.begin(); rTime != refTimes.end(); ++rTime, ++rtPos) {
                if (refTimes.size() > 1) {
                    vTimes.clear(); // will be filled anew
                    sb.setReferenceTimePos(rtPos);
                    stringstream ss;
                    time_facet* facet (new time_facet("%Y-%m-%d %H:%M:%S"));
                    ss.imbue(locale(ss.getloc(), facet));
                    ss << "seconds since " << *rTime;
                    DataPtr times = cdmReader->getScaledDataSliceInUnit(tAxis->getName(), ss.str(), sb.getTimeVariableSliceBuilder());
                    boost::shared_array<long long> timesA = times->asInt64();
                    TimeUnit tu(ss.str());
                    transform(&timesA[0],
                            &timesA[0] + times->size(),
                            back_inserter(vTimes),
                            bind1st(mem_fun_ref(&TimeUnit::unitTime2fimexTime), tu));
                    if (times->size() > 1) {
                        stepUnit = gribSeconds2stepUnits(timesA[1] - timesA[0]);
                    }
                }
                size_t vtPos = 0;
                for (vector<FimexTime>::iterator vTime = vTimes.begin(); vTime != vTimes.end(); ++vTime, ++vtPos) {
                    if (vTimes.size() > 1) {
                        sb.setTimeStartAndSize(vtPos, 1);
                    }
                    for (vector<string>::iterator var = csVars.begin(); var != csVars.end(); ++var) {
                        setTime(*var, *rTime, *vTime, stepUnit);
                        for (size_t levelPos = 0; levelPos < levels.size(); ++levelPos) {
                            if (zAxis.get() != 0) {
                                sb.setStartAndSize(zAxis, levelPos, 1);
                            }
                            double levelVal = levels.at(levelPos);
                            try {
                                // level and var are dependent due to splitting possibilities
                                setLevel(*var, levelVal);
                                setParameter(*var, levelVal);
                                DataPtr data = cdmReader->getDataSlice(*var, sb);
                                if (data->size() != 0) {
                                    boost::shared_array<double> da = data->asDouble();
                                    size_t countMissing = count(&da[0], &da[0] + data->size(), cdm.getFillValue(*var));
                                    if (countMissing < data->size()) {
                                        data = handleTypeScaleAndMissingData(*var, levelVal, data);
                                        setData(data);
                                        writeGribHandleToFile();
                                    } else {
                                        LOG4FIMEX(logger, Logger::DEBUG, "all vals invalid, dropping " << *var << " level " << levelVal << " time " << *vTime);
                                    }
                                }
                            } catch (CDMException& ex) {
                                variableWarnings[*var] = ex.what();
                            }
                        }
                    }
                }
            }
            for (map<string, string>::iterator w = variableWarnings.begin(); w != variableWarnings.end(); ++w) {
                LOG4FIMEX(logger, Logger::WARN, "unable to write parameter "<< w->first << ": " << w->second);
            }
        }
    }
    for (CDM::VarVec::const_iterator vi = vars.begin(); vi != vars.end(); ++vi) {
        if (usedVariables.find(vi->getName()) == usedVariables.end()) {
            LOG4FIMEX(logger, Logger::DEBUG, "variable not written to grib: " << vi->getName());
        }
    }
}

void GribApiCDMWriter_ImplAbstract::setGlobalAttributes()
{
    std::string globalAttrXPath("/cdm_gribwriter_config/global_attributes/");
    std::string gattr = "g" + type2string(gribVersion) + "attribute";
    std::string attr[] = {"attribute", gattr};
    for (int i = 0; i < 2; i++) {
        std::string attrXPath = globalAttrXPath + attr[i];
        setNodesAttributes(attrXPath);
    }
}

void GribApiCDMWriter_ImplAbstract::setNodesAttributes(std::string attName, void* node)
{
    XPathObjPtr xPObj;
    if (node == 0) {
        xPObj = xmlConfig->getXPathObject(attName);
    } else {
        xPObj = xmlConfig->getXPathObject(attName, reinterpret_cast<xmlNodePtr>(node));
    }
    xmlNodeSetPtr nodes = xPObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int j = 0; j < size; j++) {
        xmlNodePtr node = nodes->nodeTab[j];
        std::string name = getXmlProp(node, "name");
        std::string value = getXmlProp(node, "value");
        std::string type = getXmlProp(node, "type");
        LOG4FIMEX(logger, Logger::DEBUG, "setting attribute: " << name << "(" << value << "," << type << ")");
        if (type == "long") {
            GRIB_CHECK(grib_set_long(gribHandle.get(), name.c_str(), string2type<long>(value)), "setting grib-attr");
        } else if (type == "double") {
            GRIB_CHECK(grib_set_double(gribHandle.get(), name.c_str(), string2type<double>(value)), "setting grib-attr");
        } else if (type == "string") {
            size_t msgSize = value.size();
            GRIB_CHECK(grib_set_string(gribHandle.get(), name.c_str(), value.c_str(), &msgSize), "setting grib-attr");
        } else {
            throw CDMException("unknown type for grib attributes: " + type );
        }
    }
}

void GribApiCDMWriter_ImplAbstract::setData(const DataPtr& data)
{
    GRIB_CHECK(grib_set_double_array(gribHandle.get(), "values", data->asDouble().get(), data->size()), "setting values");
}

void GribApiCDMWriter_ImplAbstract::setTime(const std::string& varName, const boost::posix_time::ptime& rtime, const FimexTime& vTime, const std::string& stepUnits)
{
    LOG4FIMEX(logger, Logger::DEBUG, "setTime(" << varName << ", " << rtime << ", " << vTime << ")" );
    long date, time, startStep, endStep;
    if (rtime == boost::date_time::not_a_date_time) {
        // no reference-time, using time itself as reference-time
        date = vTime.getYear() * 10000 + vTime.getMonth() * 100 + vTime.getMDay();
        time = vTime.getHour() * 100 + vTime.getMinute();
        startStep = 0;
        endStep = 0;
    } else {
        date = rtime.date().year() * 10000 + rtime.date().month() * 100 + rtime.date().day();
        time = rtime.time_of_day().hours() * 100 + rtime.time_of_day().minutes();
        long steps =  static_cast<long>(.5 + ((vTime.asPosixTime() - rtime).total_seconds() / gribStepUnits2seconds(stepUnits)));
        startStep = steps;
        // TODO: step length should be determined by bounds
        endStep = steps;
    }
    if (vTime == FimexTime(FimexTime::min_date_time)) {
        date = 10101; // seems to be the minimum date for grib 0001-01-01
        time = 0;
    }
    GRIB_CHECK(grib_set_long(gribHandle.get(), "dataDate", date), "setting dataDate");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "dataTime", time), "setting dataTime");
    size_t sSize = stepUnits.size();
    GRIB_CHECK(grib_set_string(gribHandle.get(), "stepUnits", stepUnits.c_str(), &sSize), "setting stepUnits");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "startStep", startStep), "setting startStep");
    // TODO: step length should be determined by bounds
    GRIB_CHECK(grib_set_long(gribHandle.get(), "endStep", endStep), "setting endStep");
}

std::vector<double> GribApiCDMWriter_ImplAbstract::getLevels(const std::string& varName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getLevels(" << varName << ")" );
    Units units;
    std::vector<double> levelData;
    // TODO: check what to do with hybrid levels
    const CDM& cdm = cdmReader->getCDM();
    std::string verticalAxis = cdm.getVerticalAxis(varName);
    std::string verticalAxisXPath("/cdm_gribwriter_config/axes/vertical_axis");
    std::string unit;
    if (verticalAxis != ""){
        DataPtr myLevelData = cdmReader->getData(verticalAxis);
        const boost::shared_array<double> levelDataArray = myLevelData->asDouble();
        levelData= std::vector<double>(&levelDataArray[0], &levelDataArray[myLevelData->size()]);
        CDMAttribute attr;
        if (cdm.getAttribute(verticalAxis, "standard_name", attr)) {
            verticalAxisXPath += "[@standard_name=\""+ attr.getData()->asString() + "\"]";
        } else if (cdmReader->getCDM().getAttribute(verticalAxis, "units", attr)) {
            // units compatible to Pa or m
            std::string unit = attr.getStringValue();
            if (units.areConvertible(unit, "m")) {
                verticalAxisXPath += "[@unitCompatibleTo=\"m\"]";
            } else if (units.areConvertible(unit, "Pa")) {
                verticalAxisXPath += "[@unitCompatibleTo=\"Pa\"]";
            } else {
                throw CDMException("units of vertical axis " + verticalAxis + " should be compatible with m or Pa but are: " + unit);
            }
        } else {
            throw CDMException("couldn't find standard_name or units for vertical Axis " + verticalAxis + ". Is this CF compatible?");
        }
    } else {
        // cdmGribWriterConfig should contain something like standard_name=""
        verticalAxisXPath += "[@standard_name=\"\"]";
    }
    // scale the original levels according to the cdm
    double scale_factor = cdm.getScaleFactor(verticalAxis);
    double add_offset = cdm.getAddOffset(verticalAxis);
    std::transform(levelData.begin(), levelData.end(), levelData.begin(), Scale(scale_factor, add_offset));


    // scale the levels according to grib
    verticalAxisXPath += "/grib" + type2string(gribVersion);
    XPathObjPtr verticalXPObj = xmlConfig->getXPathObject(verticalAxisXPath);
    xmlNodeSetPtr nodes = verticalXPObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size == 1) {
        xmlNodePtr node = nodes->nodeTab[0];
        if (levelData.size() == 0) {
            // add default value from config
            std::string value = getXmlProp(node, "value");
            if (value != "") {
                levelData.push_back(string2type<double>(value));
            } else {
                LOG4FIMEX(logger, Logger::ERROR, "no level data available");
                throw CDMException("no level-data available");
            }
        }
        // scale the levels from cf-units to grib-untis
        std::string gribUnits = getXmlProp(node, "units");
        if (gribUnits != "") {
            CDMAttribute attr;
            if (cdm.getAttribute(verticalAxis, "units", attr)) {
                double slope;
                double offset;
                units.convert(attr.getStringValue(), gribUnits, slope, offset);
                std::transform(levelData.begin(), levelData.end(), levelData.begin(), Scale(slope, offset));
            }
        }

        // unscale to be able to put the data into values suitable to grib
        std::string gribScaleFactorStr = getXmlProp(node, "scale_factor");
        std::string gribAddOffsetStr = getXmlProp(node, "add_offset");
        scale_factor = 1.;
        add_offset = 0.;
        if (gribScaleFactorStr != "") {
            scale_factor = string2type<double>(gribScaleFactorStr);
        }
        if (gribAddOffsetStr != "") {
            add_offset = string2type<double>(gribAddOffsetStr);
        }
        std::transform(levelData.begin(), levelData.end(), levelData.begin(), UnScale(scale_factor, add_offset));
    } else if (size > 1) {
        throw CDMException("several entries in grib-config at " + configFile + ": " + verticalAxisXPath);
    } else {
        LOG4FIMEX(logger, Logger::WARN,  "could not find vertical Axis " << verticalAxisXPath << " in " << configFile << ", skipping parameter " << varName);
    }
    return levelData;
}

std::vector<FimexTime> GribApiCDMWriter_ImplAbstract::getTimes(const std::string& varName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getTimes(" << varName << ")" );
    typedef std::vector<boost::shared_ptr<const CoordinateSystem> > CoordSysList;
    CoordSysList css = listCoordinateSystems(cdmReader);
    const CDM& cdm = cdmReader->getCDM();
    std::string time = cdm.getTimeAxis(varName);
    std::vector<FimexTime> timeData;
    std::vector<double> timeDataVector;
    if (time != "") {
        const boost::shared_array<double> timeDataArray = cdmReader->getData(time)->asDouble();
        timeDataVector.insert(timeDataVector.begin(), &timeDataArray[0], &timeDataArray[cdm.getDimension(time).getLength()]);
    } else {
        // find a somewhat useful default, wild guess: first time in first time-axis found
        for (CoordSysList::iterator csit = css.begin(); csit != css.end(); ++csit) {
            CoordinateSystem::ConstAxisPtr timeAxis = (*csit)->getTimeAxis();
            if (timeAxis.get() != 0) {
                time = timeAxis->getName();
                const boost::shared_array<double> timeDataArray = cdmReader->getData(time)->asDouble();
                timeDataVector.insert(timeDataVector.begin(), timeDataArray[0]);
            }
        }
    }
    if (timeDataVector.size() > 0) {
        TimeUnit tu(cdm.getAttribute(time, "units").getStringValue());
        std::transform(timeDataVector.begin(),
                timeDataVector.end(),
                std::back_inserter(timeData),
                std::bind1st(std::mem_fun(&TimeUnit::unitTime2fimexTime), &tu));
    }
    return timeData;
}

void GribApiCDMWriter_ImplAbstract::writeGribHandleToFile()
{
    LOG4FIMEX(logger, Logger::DEBUG, "writeGribHandleToFile");
    // write data to file
    size_t size;
    const void* buffer;
    /* get the coded message in a buffer */
    GRIB_CHECK(grib_get_message(gribHandle.get(),&buffer,&size),0);
    gribFile.write(reinterpret_cast<const char*>(buffer), size);
}

bool GribApiCDMWriter_ImplAbstract::hasNodePtr(const std::string& varName, std::string& usedXPath)
{
    std::string baseXPath("/cdm_gribwriter_config/variables/parameter");
    std::string parameterXPath = baseXPath + "[@name=\"" + varName + "\"]";
    parameterXPath += "/grib"+type2string(gribVersion);
    // try first with name
    XPathObjPtr xpathObj = xmlConfig->getXPathObject(parameterXPath);
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    size_t found = (nodes) ? (nodes->nodeNr > 0) : 0;
    if (found == 0) {
        CDMAttribute attr;
        if (cdmReader->getCDM().getAttribute(varName, "standard_name", attr)) {
            std::string stdNameXPath = baseXPath + "[@standard_name=\"" + attr.getData()->asString() + "\"]";
            stdNameXPath += "/grib"+type2string(gribVersion);
            XPathObjPtr xpathObj2 = xmlConfig->getXPathObject(stdNameXPath);
            nodes = xpathObj2->nodesetval;
            found = (nodes) ? (nodes->nodeNr > 0) : 0;
            if (found > 0)
                usedXPath = stdNameXPath;
        }
    } else {
        usedXPath = parameterXPath;
    }
    return found > 0;
}

xmlNode* GribApiCDMWriter_ImplAbstract::getNodePtr(const std::string& varName, double levelValue)
{
    xmlNodePtr node = 0;
    std::vector<std::map<std::string, std::string> > levelParameters;
    std::vector<int> possibleNodes;
    std::string usedXPath;
    if (hasNodePtr(varName, usedXPath)) {
        XPathObjPtr xpathObj = xmlConfig->getXPathObject(usedXPath);
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        assert(size > 0); // checked with hasNodePtr
        if (size == 1) {
            // find node with corresponding level
            for (int i = 0; i < size; i++) {
                xmlNodePtr node = nodes->nodeTab[i];
                xmlNodePtr parent = node->parent;
                std::string level = getXmlProp(parent, "level");
                if (level != "") {
                    LOG4FIMEX(logger, Logger::DEBUG, "found parameter with level " << level << " in xml");
                    double xLevelValue = string2type<double>(level);
                    if (std::fabs(levelValue - xLevelValue) < (1e-6*levelValue)) {
                        LOG4FIMEX(logger, Logger::DEBUG, "level matches value");
                        possibleNodes.push_back(i);
                    }
                } else {
                    LOG4FIMEX(logger, Logger::DEBUG, "found parameter without level");
                    possibleNodes.push_back(i);
                }
            }
            if (possibleNodes.size() == 1) {
                node = nodes->nodeTab[possibleNodes[0]];
            } else {
                throw CDMException("found "+type2string(possibleNodes.size())+" entries in grib-config at " + configFile);
            }
        } else if (size > 1) {
            throw CDMException(type2string(size)+" entries of '" + varName + "' in grib-config at " + configFile);
        }
    } else {
        throw CDMException("could not find " + varName + " in " + configFile + " , skipping parameter");
    }
    return node;
}


}
