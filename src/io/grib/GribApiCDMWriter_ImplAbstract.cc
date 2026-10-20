/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "GribUtils.h"

#include "fimex/CDM.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/FimexTime.h"
#include "fimex/Logger.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/TimeUnit.h"
#include "fimex/TimeUtils.h"
#include "fimex/Type2String.h"
#include "fimex/Units.h"
#include "fimex/XMLUtils.h"
#include "fimex/coordSys/CoordSysDecl.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include "enumerate.h"

#include <grib_api.h>

#include <algorithm>
#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <functional>
#include <numeric>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.GribApi_CDMWriter");

/** helper classes Scale and UnScale to transform double vectors */
class Scale
{
    const double scale_factor;
    const double add_offset;

public:
    Scale(double scale_factor, double add_offset)
        : scale_factor(scale_factor)
        , add_offset(add_offset)
    {
    }
    virtual ~Scale() {}
    virtual double operator()(double x) const { return x * scale_factor + add_offset; }
};

class UnScale : public Scale
{
public:
    UnScale(double scale_factor, double add_offset)
        : Scale(1 / scale_factor, -1 * add_offset / scale_factor)
    {
    }
    virtual ~UnScale() {}
};

GribApiCDMWriter_ImplAbstract::GribApiCDMWriter_ImplAbstract(int gribVersion, CDMReader_p cdmReader, const std::string& outputFile, const XMLInput& config)
    : CDMWriter(cdmReader, outputFile)
    , gribVersion(gribVersion)
    , xmlConfig(config.getXMLDoc())
    , omitEmptyFields(true)
{
    {
        std::string templXPath("/cdm_gribwriter_config/template_file");
        XPathNodeSet nodes(xmlConfig, templXPath);
        if (nodes.size() == 1) {
            const auto gribTemplate = getXmlProp(nodes[0], "name");
            int error;
            FILE* fh = std::fopen(gribTemplate.c_str(), "r");
            if (!fh)
                throw CDMException("unable to open grib template file '" + gribTemplate + "', error is:" + std::strerror(errno));
            gribHandle = std::shared_ptr<grib_handle>(grib_handle_new_from_file(0, fh, &error), grib_handle_delete);
            if (!gribHandle)
                throw CDMException("unable to create grib handle for template '" + gribTemplate + "'");
        } else {
            const std::string gribTemplate("GRIB" + type2string(gribVersion));
            gribHandle = std::shared_ptr<grib_handle>(grib_handle_new_from_samples(0, gribTemplate.c_str()), grib_handle_delete);
            if (!gribHandle)
                throw CDMException("unable to create grib handle from samples '" + gribTemplate + "'");
        }
    }
    // check the file
    {
        std::ios_base::openmode mode = std::ios::binary | std::ios::out;
        std::string templXPath("/cdm_gribwriter_config/output_file[@type]");
        XPathNodeSet nodes(xmlConfig, templXPath);
        if (nodes.size() > 0) {
            std::string openMode = getXmlProp(nodes[0], "type");
            if (openMode == "append") {
                // append or create
                gribFile.open(outputFile.c_str(), mode | std::ios::app);
                if (gribFile.is_open()) {
                    LOG4FIMEX(logger, Logger::DEBUG, "opened '" << outputFile << "' in append-mode");
                } else {
                    gribFile.open(outputFile.c_str(), mode);
                    LOG4FIMEX(logger, Logger::DEBUG, "created '" << outputFile << "' in append-mode");
                }
            } else {
                gribFile.open(outputFile.c_str(), mode);
                LOG4FIMEX(logger, Logger::DEBUG, "opened '" << outputFile << "' in overwrite-mode");
            }
        } else {
            gribFile.open(outputFile.c_str(), mode);
            LOG4FIMEX(logger, Logger::DEBUG, "opened '" << outputFile << "' in overwrite-mode");
        }
        if (!gribFile.is_open())
            throw CDMException("Cannot write grib-file: " + outputFile);
    }
    // check processing options
    {
        std::string templXPath("/cdm_gribwriter_config/processing_options/omitEmptyFields");
        XPathNodeSet nodes(xmlConfig, templXPath);
        if (nodes.size() > 0) {
            const std::string omitEmpty = getXmlContent(nodes[0]);
            if (omitEmpty == "1" || omitEmpty == "yes" || omitEmpty == "true")
                omitEmptyFields = true;
            else if (omitEmpty == "0" || omitEmpty == "no" || omitEmpty == "false")
                omitEmptyFields = false;
            else if (omitEmpty != "")
                LOG4FIMEX(logger, Logger::WARN, "ignoring unknown value '" << omitEmpty << "' for processing option 'omitEmptyFields'");
        }
    }
}

GribApiCDMWriter_ImplAbstract::~GribApiCDMWriter_ImplAbstract() {}

std::vector<FimexTime> convertToFimexTime(shared_array<long long> timeValues, size_t timeCount, const std::string& timeUnitText)
{
    std::vector<FimexTime> times;
    times.reserve(timeCount);
    const TimeUnit timeUnit(timeUnitText);
    std::transform(&timeValues[0], &timeValues[0] + timeCount, std::back_inserter(times), [&](auto tv) { return timeUnit.unitTime2fimexTime(tv); });
    return times;
}

static const std::string SECONDS_SINCE_1970 = "seconds since 1970-01-01 00:00:00";

static std::string makeTimeUnitText(const FimexTime& time)
{
    if (time.invalid()) {
        return SECONDS_SINCE_1970;
    } else {
        std::ostringstream timeUnitText;
        timeUnitText << "seconds since " << make_time_string_extended(time);
        return timeUnitText.str();
    }
}

void GribApiCDMWriter_ImplAbstract::run()
{
    LOG4FIMEX(logger, Logger::DEBUG, "GribApiCDMWriter_ImplAbstract::run()");

    // default to grid_second_order
    //    string pType("grid_second_order");
    //    try {
    //        size_t pTypeSize = pType.size();
    //        GRIB_CHECK(grib_set_string(gribHandle.get(), "packingType", pType.c_str(), &pTypeSize), "setting packingType ");
    //    } catch (...) {
    //        LOG4FIMEX(logger, Logger::WARN, "unable to set packingType to " << pType );
    //    }
    setGlobalAttributes();

    // get all coordinate systems from file, usually one, but may be a few (theoretical limit: # of variables)
    const auto allCoordSys = listCoordinateSystems(cdmReader);
    const auto& cdm = cdmReader->getCDM();
    const auto& vars = cdm.getVariables();

    std::map<std::string, std::string> variableWarnings;
    std::set<std::string> unwrittenVariables;
    for (const auto& var : vars) {
        unwrittenVariables.insert(var.getName());
    }

    for (const auto& coordSys : allCoordSys) {
        if (!coordSys->isSimpleSpatialGridded()) {
            continue;
        }

        // find variables in this coordSys that appear in GRIB writer configuration
        std::string dummyUsedXPath;
        std::vector<std::string> csVars;
        for (const auto& var : vars) {
            if (coordSys->isCSAndCompleteFor(var.getName())) {
                if (hasNodePtr(var.getName(), dummyUsedXPath)) {
                    csVars.push_back(var.getName());
                } else {
                    LOG4FIMEX(logger, Logger::WARN, "cannot write variable " << var.getName() << ": not declared in config-file " << configFile);
                }
            }
        }
        if (csVars.empty()) {
            continue;
        }

        try {
            // TODO: this would be nicer with varSysIt->getProjection() instead of csVars[0]
            setProjection(csVars.at(0));
        } catch (CDMException& e) {
            LOG4FIMEX(logger, Logger::WARN,
                      "cannot write variable " << join(csVars.begin(), csVars.end(), ", ") << " due to projection problems: " << e.what());
            continue;
        }

        const auto zAxis = coordSys->getGeoZAxis(); // Z
        const auto tAxis = coordSys->getTimeAxis(); // time

        // build vec of reference times
        const auto refTimes = getReferenceTimes(coordSys);

        // fetch the levels from first variable
        const auto levels = getLevels(csVars.at(0));

        // TODO: handle ensembles

        CoordinateSystemSliceBuilder sb(cdm, coordSys);
        sb.setAll(coordSys->getGeoXAxis()); // X or Lon
        sb.setAll(coordSys->getGeoYAxis()); // Y or Lat
        // set reftime, time, vertical dimensions for unset dimension test below
        sb.setReferenceTimePos(0);
        sb.setStartAndSize(zAxis, 0, 1); // silently ignores null zAxis
        if (tAxis) {
            sb.setTimeStartAndSize(0, 1); // would print warning for null tAxis
        }
        // reduce all unset dimensions to the first slice
        for (const auto& dim : sb.getUnsetDimensionNames()) {
            LOG4FIMEX(logger, Logger::WARN, "unknown dimension in grib-writer: '" << dim << "' using first slice");
            sb.setStartAndSize(dim, 0, 1);
        }

        // loop over ref-times, times, variables and levels
        for (const auto [refTimePos, refTime] : enumerate(refTimes)) {
            sb.setReferenceTimePos(refTimePos);

            FimexTime actualRefTime = refTime;
            if (actualRefTime.getSecond() != 0) {
                LOG4FIMEX(logger, Logger::WARN, "GRIB cannot handle reference time second=" << static_cast<int>(actualRefTime.getSecond()) << ", overwriting with 0");
                actualRefTime.setSecond(0);
            }
            std::vector<long long> vTimes; // valid times in seconds since actualRefTime
            std::string stepUnit = "s";    // step unit
            long long stepSeconds = 1;     // number of seconds for one step unit
            if (tAxis) {
                const auto timeUnitText = makeTimeUnitText(actualRefTime);
                auto tsb = sb.getTimeVariableSliceBuilder(); // new slicebuild for time axis ...
                tsb.setAll(tAxis->getName());                // ... fetching all time values for the current reference time
                auto vTimeData = cdmReader->getScaledDataSliceInUnit(tAxis->getName(), timeUnitText, tsb);
                auto vTimeValues = vTimeData->asInt64();
                vTimes = std::vector<long long>(&vTimeValues[0], &vTimeValues[0] + vTimeData->size());
                if (vTimes[0] < 0) {
                    // shift vTimes and actualRefTime to avoid negative vTime values (unsupported in GRIB1)
                    const auto shift = vTimes[0];
                    actualRefTime = add(actualRefTime, shift);
                    for (auto& vt : vTimes) {
                        vt -= shift;
                    }
                }
                if (vTimes.size() >= 2) {
                    size_t idx = (vTimes[0] > 0) ? 0 : 1;
                    auto step_gcd = vTimes[idx++];
                    for (; idx < vTimes.size(); ++idx) {
                        step_gcd = std::gcd(step_gcd, vTimes[idx]);
                    }
                    stepUnit = gribSeconds2stepUnits(step_gcd);
                    stepSeconds = gribStepUnits2seconds(stepUnit);
                }
            } else {
                vTimes = {0};
            }

            for (const auto [vtPos, vTime] : enumerate(vTimes)) {
                if (tAxis) {
                    sb.setTimeStartAndSize(vtPos, 1); // would print warning for null tAxis
                }
                for (const auto& var : csVars) {
                    unwrittenVariables.erase(var);
                    for (const auto [levelPos, levelVal] : enumerate(levels)) {
                        sb.setStartAndSize(zAxis, levelPos, 1); // silently ignores null zAxis
                        try {
                            DataPtr data = cdmReader->getDataSlice(var, sb);
                            if (data && data->size() != 0) {
                                auto da = data->asDouble();
                                bool writeData = true;
                                if (omitEmptyFields) {
                                    const size_t countMissing = std::count(&da[0], &da[0] + data->size(), cdm.getFillValue(var));
                                    writeData = (countMissing < data->size());
                                }
                                if (writeData) {
                                    setTime(actualRefTime, vTime, stepSeconds, stepUnit);
                                    setLevel(var, levelVal, levelPos); // level and var are dependent due to splitting possibilities
                                    setParameter(var, levelVal);
                                    setData(handleTypeScaleAndMissingData(var, levelVal, data));
                                    writeGribHandleToFile();
                                } else {
                                    LOG4FIMEX(logger, Logger::DEBUG, "all vals invalid, dropping " << var << " level " << levelVal << " time " << vTime);
                                }
                            }
                        } catch (CDMException& ex) {
                            variableWarnings[var] = ex.what();
                        }
                    }
                }
            }
        }
        for (const auto& [var, warning] : variableWarnings) {
            LOG4FIMEX(logger, Logger::WARN, "unable to write parameter " << var << ": " << warning);
        }
    }
    for (const auto& uwvar : unwrittenVariables) {
        LOG4FIMEX(logger, Logger::DEBUG, "variable not written to grib: " << uwvar);
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
    for (auto node : XPathNodeSet(xmlConfig->getXPathObject(attName, reinterpret_cast<xmlNodePtr>(node)))) {
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
            throw CDMException("unknown type for grib attributes: " + type);
        }
    }
}

void GribApiCDMWriter_ImplAbstract::setData(const DataPtr& data)
{
    GRIB_CHECK(grib_set_double_array(gribHandle.get(), "values", data->asDouble().get(), data->size()), "setting values");
}

static long gribDate(const FimexTime& time)
{
    return time.getYear() * 10000 + time.getMonth() * 100 + time.getMDay();
}

static long gribTime(const FimexTime& time)
{
    return time.getHour() * 100 + time.getMinute();
}

void GribApiCDMWriter_ImplAbstract::setGribTime(const FimexTime& dataDateTime, const std::string& stepUnits, long long startStep, long long endStep)
{
    GRIB_CHECK(grib_set_long(gribHandle.get(), "dataDate", gribDate(dataDateTime)), "setting dataDate");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "dataTime", gribTime(dataDateTime)), "setting dataTime");
    size_t sSize = stepUnits.size();
    GRIB_CHECK(grib_set_string(gribHandle.get(), "stepUnits", stepUnits.c_str(), &sSize), "setting stepUnits");
    GRIB_CHECK(grib_set_long(gribHandle.get(), "startStep", startStep), "setting startStep");
    // TODO: step length should be determined by bounds
    GRIB_CHECK(grib_set_long(gribHandle.get(), "endStep", endStep), "setting endStep");
}

std::vector<FimexTime> GribApiCDMWriter_ImplAbstract::getReferenceTimes(CoordinateSystem_cp coordSys)
{
    std::vector<FimexTime> refTimes;
    // time-Axis might be multi-dimensional, i.e. forecast_reference_time
    if (CoordinateAxis_cp rtAxis = coordSys->findAxisOfType(CoordinateAxis::ReferenceTime)) {
        auto refTimeData = cdmReader->getScaledDataInUnit(rtAxis->getName(), SECONDS_SINCE_1970);
        auto refTimeValues = refTimeData->asInt64();
        refTimes = convertToFimexTime(refTimeValues, refTimeData->size(), SECONDS_SINCE_1970);
    }
    if (refTimes.empty()) {
        FimexTime refTime;
        try {
            refTime = getUniqueForecastReferenceTimeFT(cdmReader);
        } catch (CDMException& ex) {
        }
        refTimes = {refTime};
    }
    return refTimes;
}

std::vector<double> GribApiCDMWriter_ImplAbstract::getLevels(const std::string& varName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getLevels(" << varName << ")");
    Units units;
    std::vector<double> levelData;
    // TODO: check what to do with hybrid levels
    const CDM& cdm = cdmReader->getCDM();
    std::string verticalAxis = cdm.getVerticalAxis(varName);
    std::string verticalAxisXPath("/cdm_gribwriter_config/axes/vertical_axis");
    std::string unit;
    if (verticalAxis != "") {
        DataPtr myLevelData = cdmReader->getData(verticalAxis);
        const auto levelDataArray = myLevelData->asDouble();
        levelData = std::vector<double>(&levelDataArray[0], &levelDataArray[myLevelData->size()]);
        CDMAttribute attr;
        if (cdm.getAttribute(verticalAxis, "standard_name", attr)) {
            verticalAxisXPath += "[@standard_name=\"" + attr.getData()->asString() + "\"]";
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
    XPathNodeSet nodes(xmlConfig, verticalAxisXPath);
    if (nodes.size() == 1) {
        auto node = nodes[0];
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
    } else if (nodes.size() > 1) {
        throw CDMException("several entries in grib-config at " + configFile + ": " + verticalAxisXPath);
    } else {
        LOG4FIMEX(logger, Logger::WARN, "could not find vertical Axis " << verticalAxisXPath << " in " << configFile << ", skipping parameter " << varName);
    }
    return levelData;
}

std::vector<FimexTime> GribApiCDMWriter_ImplAbstract::getTimes(const std::string& varName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getTimes(" << varName << ")");
    CoordinateSystem_cp_v css = listCoordinateSystems(cdmReader);
    const CDM& cdm = cdmReader->getCDM();
    std::string time = cdm.getTimeAxis(varName);
    std::vector<FimexTime> timeData;
    std::vector<double> timeDataVector;
    if (time != "") {
        const auto timeDataArray = cdmReader->getData(time)->asDouble();
        timeDataVector.insert(timeDataVector.begin(), &timeDataArray[0], &timeDataArray[cdm.getDimension(time).getLength()]);
    } else {
        // find a somewhat useful default, wild guess: first time in first time-axis found
        for (CoordinateSystem_cp_v::iterator csit = css.begin(); csit != css.end(); ++csit) {
            CoordinateAxis_cp timeAxis = (*csit)->getTimeAxis();
            if (timeAxis.get() != 0) {
                time = timeAxis->getName();
                const auto timeDataArray = cdmReader->getData(time)->asDouble();
                timeDataVector.insert(timeDataVector.begin(), timeDataArray[0]);
            }
        }
    }
    if (timeDataVector.size() > 0) {
        TimeUnit tu(cdm.getAttribute(time, "units").getStringValue());
        std::transform(timeDataVector.begin(), timeDataVector.end(), std::back_inserter(timeData),
                       std::bind(std::mem_fn(&TimeUnit::unitTime2fimexTime), &tu, std::placeholders::_1));
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
    GRIB_CHECK(grib_get_message(gribHandle.get(), &buffer, &size), 0);
    gribFile.write(reinterpret_cast<const char*>(buffer), size);
}

bool GribApiCDMWriter_ImplAbstract::hasNodePtr(const std::string& varName, std::string& usedXPath)
{
    std::string baseXPath("/cdm_gribwriter_config/variables/parameter");
    std::string parameterXPath = baseXPath + "[@name=\"" + varName + "\"]";
    parameterXPath += "/grib" + type2string(gribVersion);
    // try first with name
    XPathNodeSet nodesP(xmlConfig, parameterXPath);
    if (nodesP.size() > 0) {
        usedXPath = parameterXPath;
        return true;
    }

    CDMAttribute attr;
    if (cdmReader->getCDM().getAttribute(varName, "standard_name", attr)) {
        std::string stdNameXPath = baseXPath + "[@standard_name=\"" + attr.getData()->asString() + "\"]";
        stdNameXPath += "/grib" + type2string(gribVersion);
        XPathNodeSet nodesS(xmlConfig, stdNameXPath);
        if (nodesS.size() > 0) {
            usedXPath = stdNameXPath;
            return true;
        }
    }

    return false;
}

xmlNode* GribApiCDMWriter_ImplAbstract::getNodePtr(const std::string& varName, double levelValue)
{
    std::string usedXPath;
    if (!hasNodePtr(varName, usedXPath)) {
        throw CDMException("could not find " + varName + " in " + configFile + " , skipping parameter");
    }

    const XPathNodeSet nodes(xmlConfig, usedXPath);
    if (nodes.size() != 1) {
        throw CDMException(type2string(nodes.size()) + " entries of '" + varName + "' in grib-config at " + configFile);
    }

    // find node with corresponding level
    xmlNodePtr node = nodes[0];
    const auto level = getXmlProp(node->parent, "level");
    if (level.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "found parameter without level");
        return node;
    }

    LOG4FIMEX(logger, Logger::DEBUG, "found parameter with level " << level << " in xml");
    double xLevelValue = string2type<double>(level);
    if (std::fabs(levelValue - xLevelValue) < (1e-6 * levelValue)) {
        LOG4FIMEX(logger, Logger::DEBUG, "level matches value");
        return node;
    }

    throw CDMException("found 0 entries in grib-config at " + configFile);
}

} // namespace MetNoFimex
