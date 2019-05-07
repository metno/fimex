/*
 * Fimex, CDMPressureConversions.cc
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
 *
 *  Created on: Aug 12, 2011
 *      Author: Heiko Klein
 */

#include "fimex/CDMPressureConversions.h"
#include "fimex/CDMVerticalInterpolator.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/coordSys/verticalTransform/ToVLevelConverter.h"
#include "fimex/Logger.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#include "fimex/Logger.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"
#include "fimex/vertical_coordinate_transformations.h"
#include "fimex/Logger.h"
#include "coordSys/CoordSysUtils.h"

#include <memory>

namespace MetNoFimex
{

static Logger_p logger = getLogger("fimex.CDMPressureConversions");

using namespace std;

namespace {

template<class T>
boost::shared_array<T> dataAs(DataPtr data);

template<>
boost::shared_array<float> dataAs<float>(DataPtr data) {
    return data->asFloat();
}

template<>
boost::shared_array<double> dataAs<double>(DataPtr data) {
    return data->asDouble();
}

template<typename T>
void convert_omega_to_vertical_wind(size_t size, const T* o, const T* p, const T* t, T* w);

template<>
void convert_omega_to_vertical_wind<double>(size_t size, const double* o, const double* p, const double* t, double* w)
{
    mifi_omega_to_vertical_wind(size, o, p, t, w);
}

template<>
void convert_omega_to_vertical_wind<float>(size_t size, const float* o, const float* p, const float* t, float* w)
{
    mifi_omega_to_vertical_wind_f(size, o, p, t, w);
}

typedef float VerticalData_t;
typedef boost::shared_array<VerticalData_t> VerticalDataArray;

const float relative_humidity_scale_factor = 25000;

typedef std::map<std::string, std::string> string_string_m;

const string ADD_OFFSET = "add_offset";
const string COORDINATES = "coordinates";
const string LONG_NAME = "long_name";
const string SCALE_FACTOR = "scale_factor";
const string STANDARD_NAME = "standard_name";
const string UNITS = "units";
const string VALID_MAX = "valid_max";
const string VALID_MIN = "valid_min";
const string VALID_RANGE = "valid_range";
const string FILL_VALUE = "_FillValue";

const string AIR_PRESSURE = "air_pressure";
const string AIR_PRESSURE4D = "air_pressure4D";
const string AIR_POTENTIAL_TEMPERATURE = "air_potential_temperature";
const string AIR_TEMPERATURE = "air_temperature";
const string RELATIVE_HUMIDITY = "relative_humidity";
const string SPECIFIC_HUMIDITY = "specific_humidity";
const string UPWARD_AIR_VELOCITY = "upward_air_velocity";
const string OMEGA_REGEX = "(omega|lagrangian_tendency_of_air_pressure|vertical_air_velocity_expressed_as_tendency_of_pressure)";

class Converter  {
public:
    Converter(const std::string& varName)
        : varName_(varName) { }
    virtual ~Converter();

    const std::string& getName() const
        { return varName_; }

    virtual DataPtr getDataSlice(size_t unLimDimPos) = 0;

private:
    std::string varName_;
};
typedef std::shared_ptr<Converter> Converter_p;
typedef std::vector<Converter_p> Converter_pv;

Converter::~Converter()
{
}

// ------------------------------------------------------------------------------------------------

struct ConverterFactory {
    struct Environment {
        CDMReader_p inputReader;
        const CDM& inputCDM;
        const CoordinateSystem_cp_v inputCoordSys;

        std::shared_ptr<CDM> outputCDM;

        Environment(CDMReader_p inReader, std::shared_ptr<CDM> outCDM)
            : inputReader(inReader)
            , inputCDM(inputReader->getCDM())
            , inputCoordSys(listCoordinateSystems(inputReader))
            , outputCDM(outCDM)
        {
        }
    };

    struct Operation {
        std::string conversion;
        string_string_m options;
        bool has_option(const std::string& key) const
            { return (options.find(key) != options.end()); }
        const std::string& option(const std::string& key) const
            { return option(key, key); }
        const std::string& option(const std::string& key, const std::string& default_value) const
            { string_string_m::const_iterator it = options.find(key); return (it != options.end()) ? it->second : default_value; }
    };

    virtual ~ConverterFactory() { }
    virtual const std::string name() const = 0;
    virtual int match(const Environment& env, const std::string& conversion) const = 0;
    virtual Converter_pv createConverter(Environment& env, const Operation& oper) const = 0;
};
typedef std::shared_ptr<ConverterFactory> ConverterFactoryPtr;

// ------------------------------------------------------------------------------------------------

vector<string> findVariables(ConverterFactory::Environment& env, const ConverterFactory::Operation& oper,
                             const std::string& optionkey, const std::string& standard_name)
{
    vector<string> found;

    string_string_m::const_iterator itOptName = oper.options.find(optionkey);
    if (itOptName != oper.options.end()) {
        if (env.inputCDM.hasVariable(itOptName->second)) {
            found.push_back(itOptName->second);
        } else {
            LOG4FIMEX(logger, Logger::WARN, "no variable '" << itOptName->second << "' found");
        }
    } else {
        vector<string> dims;
        map<string, string> atts;
        atts[STANDARD_NAME] = standard_name;
        found = env.inputCDM.findVariables(atts, dims);
    }
    return found;
}

vector<string> findVariables(ConverterFactory::Environment& env, const ConverterFactory::Operation& oper,
                             const std::string& optionkey)
{
    return findVariables(env, oper, optionkey, optionkey);
}

std::string deriveVariableName(const ConverterFactory::Operation& oper, const std::string& key, const std::string& base,
                               const std::string& replace_this, const std::string& replace_with)
{
    if (oper.has_option(key))
        return oper.option(key);

    std::string derived = base;
    const boost::regex replace_this_re(replace_this);
    boost::regex_replace(derived, replace_this_re, replace_with);
    if (derived != base)
        return derived;
    else
        return key;
}

// ------------------------------------------------------------------------------------------------

class ThetaTemperatureConverter : public Converter {
public:
    ThetaTemperatureConverter(const std::string& temperature, CDMReader_p reader, CoordinateSystem_cp cs, const std::string& theta)
        : Converter(temperature)
        , reader_(reader)
        , cs_(cs)
        , theta_(theta)
    {
    }

    DataPtr getDataSlice(size_t unLimDimPos);

private:
    CDMReader_p reader_;
    CoordinateSystem_cp cs_;
    const std::string& theta_;
};

DataPtr ThetaTemperatureConverter::getDataSlice(size_t unLimDimPos)
{
    DataPtr pressureData = verticalData4D(cs_, reader_, unLimDimPos, MIFI_VINT_PRESSURE);
    boost::shared_array<VerticalData_t> pressureValues = dataAs<VerticalData_t>(pressureData);
    const size_t size = pressureData->size();

    const float add_offset = reader_->getCDM().getAddOffset(theta_);
    boost::shared_array<float> thetaValues = checkData(reader_->getDataSlice(theta_, unLimDimPos), size, theta_)->asFloat();

    const float cp = 1004.; // J/kgK
    const float R = MIFI_GAS_CONSTANT / MIFI_MOLAR_MASS_DRY_AIR; // J/K
    const float ps = 1000.; // hPa
    const float psX1 = 1/ps;
    const float Rcp = R/cp;
    for (size_t i = 0; i < size; i++) {
        // theta = T * (ps / p)^(R/cp) => T = theta * (p/ps)^(R/cp)
        thetaValues[i] = ((thetaValues[i]+add_offset)*pow(pressureValues[i]*psX1, Rcp))-add_offset;
    }
    return createData(size, thetaValues);
}

// ------------------------------------------------------------------------------------------------

class ThetaTemperatureConverterFactory : public ConverterFactory {
public:
    const std::string name() const
        { return "ThetaTemperature"; }

    int match(const Environment& env, const std::string& conversion) const
        { return conversion == "theta2T" ? 1 : 0; }

    Converter_pv createConverter(Environment& env, const Operation& oper) const;
};

Converter_pv ThetaTemperatureConverterFactory::createConverter(Environment& env, const Operation& oper) const
{
    Converter_pv converters;
    const vector<string> thetaV = findVariables(env, oper, AIR_POTENTIAL_TEMPERATURE);
    for (vector<string>::const_iterator itTheta = thetaV.begin(); itTheta != thetaV.end(); ++itTheta) {
        CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(env.inputCoordSys, *itTheta);
        if (!cs) {
            LOG4FIMEX(logger, Logger::WARN, "no coordinate system for " << *itTheta << " found");
            continue;
        }

        const string temperature = deriveVariableName(oper, AIR_TEMPERATURE, *itTheta, "_potential_", "");
        if (env.outputCDM->hasVariable(temperature)) {
            LOG4FIMEX(logger, Logger::WARN, "variable '" << temperature << "' exists, no conversion from " << *itTheta);
            continue;
        }

        const CDMVariable& vTheta = env.inputCDM.getVariable(*itTheta);
        const vector<string>& shape = vTheta.getShape();
        const CDMDataType thetaType = vTheta.getDataType();
        env.outputCDM->addVariable(CDMVariable(temperature, thetaType, shape));

        const vector<CDMAttribute> thetaAtts = env.inputCDM.getAttributes(*itTheta);
        for (vector<CDMAttribute>::const_iterator it = thetaAtts.begin(); it != thetaAtts.end(); ++it) {
            if (it->getName() != STANDARD_NAME && it->getName() != LONG_NAME) {
                env.outputCDM->addAttribute(temperature, *it);
            }
        }
        env.outputCDM->addAttribute(temperature, CDMAttribute(STANDARD_NAME, AIR_TEMPERATURE));
        env.outputCDM->removeVariable(*itTheta); // why?

        LOG4FIMEX(logger, Logger::INFO, "converter from " << *itTheta << " to " << temperature);
        converters.push_back(std::make_shared<ThetaTemperatureConverter>(temperature, env.inputReader, cs, *itTheta));
    }
    return converters;
}

// ------------------------------------------------------------------------------------------------

class HumidityConverter : public Converter {
public:
    HumidityConverter(const std::string& relative, CDMReader_p reader, CoordinateSystem_cp cs, const std::string& specific, const std::string& temperature)
        : Converter(relative)
        , reader_(reader)
        , cs_(cs)
        , specific_(specific)
        , temperature_(temperature)
    {
    }

    DataPtr getDataSlice(size_t unLimDimPos);

private:
    CDMReader_p reader_;
    CoordinateSystem_cp cs_;
    std::string specific_;
    std::string temperature_;
};

DataPtr HumidityConverter::getDataSlice(size_t unLimDimPos)
{
    DataPtr pressureData = verticalData4D(cs_, reader_, unLimDimPos, MIFI_VINT_PRESSURE);
    boost::shared_array<VerticalData_t> pressureValues = dataAs<VerticalData_t>(pressureData);
    const size_t size = pressureData->size();

    boost::shared_array<float> shValues = checkData(reader_->getScaledDataSliceInUnit(specific_, "1", unLimDimPos), size, specific_)->asFloat();
    boost::shared_array<float> airtValues = checkData(reader_->getScaledDataSliceInUnit(temperature_, "K", unLimDimPos), size, temperature_)->asFloat();
    boost::shared_array<short> rhValues(new short[size]);
    for (size_t i = 0; i < size; i++) {
        // we have pressure in hPa and need Pa for
        // mifi_specific_to_relative_humidity, we must multiply our
        // pressure with 100; we get relhum in % but want unit "1", ie
        // we have to divide by 100; as we know the formula, we drop
        // multiplying and then dividing
        const float c100 = 1;
        const float rh = mifi_specific_to_relative_humidity(shValues[i], airtValues[i], pressureValues[i]*c100) / c100;
        rhValues[i] = (short) (relative_humidity_scale_factor * rh + 0.5); // apply scale_factor
    }
    return createData(size, rhValues);
}

// ------------------------------------------------------------------------------------------------

class HumidityConverterFactory : public ConverterFactory {
public:
    const std::string name() const
        { return "Humidity"; }

    int match(const Environment& env, const std::string& conversion) const
        { return conversion == "specific2relative" ? 1 : 0; }

    Converter_pv createConverter(Environment& env, const Operation& oper) const;
};

Converter_pv HumidityConverterFactory::createConverter(Environment& env, const Operation& oper) const
{
    const vector<string> shV = findVariables(env, oper, SPECIFIC_HUMIDITY);
    const vector<string> tempV = findVariables(env, oper, AIR_TEMPERATURE);

    Converter_pv converters;
    for (vector<string>::const_iterator itSpecific = shV.begin(); itSpecific != shV.end(); ++itSpecific) {
        CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(env.inputCoordSys, *itSpecific);
        if (!cs.get()) {
            LOG4FIMEX(logger, Logger::WARN, "no coordinate system for '" << *itSpecific << "' found");
            continue;
        }
        vector<string>::const_iterator itTemp = tempV.begin();
        while (itTemp != tempV.end() && !cs->isCSAndCompleteFor(*itTemp))
            ++itSpecific;
        if (itTemp == tempV.end()) {
            LOG4FIMEX(logger, Logger::WARN, "no coordinate system for '" << *itSpecific << "' and '" << *itTemp << "' found");
            continue;
        }

        const string relativeHumName = deriveVariableName(oper, RELATIVE_HUMIDITY, *itSpecific, "specific", "relative");
        if (env.outputCDM->hasVariable(relativeHumName)) {
            LOG4FIMEX(logger, Logger::WARN, "variable '" << relativeHumName << "' exists, no conversion added");
            continue;
        }

        const CDMVariable& shVar = env.outputCDM->getVariable(*itSpecific);
        const vector<string>& shape = shVar.getShape();
        const vector<CDMAttribute> shAtts = env.outputCDM->getAttributes(*itSpecific);
        env.outputCDM->addVariable(CDMVariable(relativeHumName, CDM_SHORT, shape));

        for (vector<CDMAttribute>::const_iterator it = shAtts.begin(); it != shAtts.end(); ++it) {
            const string& aname = it->getName();
            if (aname != STANDARD_NAME && aname != LONG_NAME && aname != UNITS
                    && aname != SCALE_FACTOR && aname != ADD_OFFSET && aname != FILL_VALUE
                    && aname != VALID_MIN && aname != VALID_MAX && aname != VALID_RANGE)
            {
                env.outputCDM->addAttribute(relativeHumName, *it);
            }
        }
        env.outputCDM->addAttribute(relativeHumName, CDMAttribute(STANDARD_NAME, RELATIVE_HUMIDITY));
        env.outputCDM->addAttribute(relativeHumName, CDMAttribute(LONG_NAME, "relative humidity derived from " + *itSpecific + " and " + *itTemp));
        env.outputCDM->addAttribute(relativeHumName, CDMAttribute(UNITS, "1"));
        env.outputCDM->addAttribute(relativeHumName, CDMAttribute(VALID_MIN, 0));
        env.outputCDM->addAttribute(relativeHumName, CDMAttribute(VALID_MAX, (short)relative_humidity_scale_factor));
        env.outputCDM->addAttribute(relativeHumName, CDMAttribute(SCALE_FACTOR, 1/relative_humidity_scale_factor));

        converters.push_back(std::make_shared<HumidityConverter>(relativeHumName, env.inputReader, cs, *itSpecific, *itTemp));
    }
    LOG4FIMEX(logger, Logger::INFO, "have " << converters.size() << " converters to " << RELATIVE_HUMIDITY);
    return converters;
}

// ------------------------------------------------------------------------------------------------

class OmegaVerticalConverter : public Converter {
public:
    OmegaVerticalConverter(const std::string& vwind, CDMReader_p reader, CoordinateSystem_cp cs, const std::string& omega, const std::string& temperature)
        : Converter(vwind)
        , reader_(reader)
        , cs_(cs)
        , omega_(omega)
        , temperature_(temperature)
    {
    }

    DataPtr getDataSlice(size_t unLimDimPos);

private:
    CDMReader_p reader_;
    CoordinateSystem_cp cs_;
    std::string omega_;
    std::string temperature_;
};

DataPtr OmegaVerticalConverter::getDataSlice(size_t unLimDimPos)
{
    DataPtr pressureData = verticalData4D(cs_, reader_, unLimDimPos, MIFI_VINT_PRESSURE);
    boost::shared_array<VerticalData_t> pressureValues = dataAs<VerticalData_t>(pressureData); // unit: hPa
    const size_t size = pressureData->size();
    VerticalDataArray airtempValues = dataAs<VerticalData_t>(checkData(reader_->getScaledDataSliceInUnit(temperature_, "K", unLimDimPos), size, temperature_));
    VerticalDataArray omegaValues = dataAs<VerticalData_t>(checkData(reader_->getScaledDataSliceInUnit(omega_, "hPa/s", unLimDimPos), size, omega_));

    convert_omega_to_vertical_wind(size, omegaValues.get(), pressureValues.get(), airtempValues.get(), omegaValues.get());
    return createData(size, omegaValues);
}

// ------------------------------------------------------------------------------------------------

class OmegaVerticalConverterFactory : public ConverterFactory {
public:
    const std::string name() const
        { return "OmegaVertical"; }

    int match(const Environment& env, const std::string& conversion) const
        { return conversion == "omega2vwind" ? 1 : 0; }

    std::vector<Converter_p> createConverter(Environment& env, const Operation& oper) const;
};

Converter_pv OmegaVerticalConverterFactory::createConverter(Environment& env, const Operation& oper) const
{
    const vector<string> omegaV = findVariables(env, oper, "omega", OMEGA_REGEX);
    if (omegaV.size() != 1) {
        LOG4FIMEX(logger, Logger::WARN, "found no, or more than one, variables " << OMEGA_REGEX);
        return Converter_pv();
    }

    const vector<string> tempV = findVariables(env, oper, AIR_TEMPERATURE);
    if (tempV.size() != 1) {
        LOG4FIMEX(logger, Logger::WARN, "found no, or more than one, variables " << AIR_TEMPERATURE);
        return Converter_pv();
    }

    const std::string& omega = omegaV.front(), temperature = tempV.front();

    CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(env.inputCoordSys, omega);
    if (!cs || !cs->isCSAndCompleteFor(temperature)) {
        LOG4FIMEX(logger, Logger::WARN, "no coordinate system for '" << omega << "' and '" << temperature << "'' found");
        return Converter_pv();
    }

    const string verticalWindName = deriveVariableName(oper, UPWARD_AIR_VELOCITY, omega, OMEGA_REGEX, UPWARD_AIR_VELOCITY);
    if (env.outputCDM->hasVariable(verticalWindName)) {
        LOG4FIMEX(logger, Logger::WARN, "variable '" << verticalWindName << "' exists, not adding conversion");
        return Converter_pv();
    }

    const vector<string>& shape = env.inputCDM.getVariable(omega).getShape();
    env.outputCDM->addVariable(CDMVariable(verticalWindName, CDM_FLOAT, shape));
    env.outputCDM->addAttribute(verticalWindName, CDMAttribute(STANDARD_NAME, UPWARD_AIR_VELOCITY));
    env.outputCDM->addAttribute(verticalWindName, CDMAttribute(UNITS, "m/s"));
    CDMAttribute xatt;
    if (env.inputCDM.getAttribute(verticalWindName, COORDINATES, xatt)) {
        env.outputCDM->addAttribute(verticalWindName, xatt);
    }
    env.outputCDM->removeVariable(omega); // why?

    return Converter_pv(1, std::make_shared<OmegaVerticalConverter>(verticalWindName, env.inputReader, cs, omega, temperature));
}

// ------------------------------------------------------------------------------------------------

class AddPressure4DConverter : public Converter {
public:
    AddPressure4DConverter(const std::string& pressure4d, CDMReader_p reader, VerticalConverter_p vc, const std::vector<std::string>& shape4d)
        : Converter(pressure4d)
        , reader_(reader)
        , vc_(vc)
        , shape4d_(shape4d)
    {
    }

    DataPtr getDataSlice(size_t unLimDimPos);

private:
    CDMReader_p reader_;
    VerticalConverter_p vc_;
    std::vector<std::string> shape4d_;
};

DataPtr AddPressure4DConverter::getDataSlice(size_t unLimDimPos)
{
    const CDM& rcdm = reader_->getCDM();
    DataPtr vcdata = verticalData4D(vc_, rcdm, unLimDimPos);
    if (vc_->getShape() != shape4d_) {
        LOG4FIMEX(logger, Logger::WARN, "4d pressure shape change not implemented, results are probably wrong");
        // FIXME vcdata = reshape(rcdm, vc_->getShape(), shape4d_, vcdata);
    }
    return vcdata;
}

// ------------------------------------------------------------------------------------------------

class AddPressure4DConverterFactory : public ConverterFactory {
public:
    const std::string name() const
        { return "AddPressure4D"; }

    int match(const Environment& env, const std::string& conversion) const
        { return conversion == "add4Dpressure" ? 1 : 0; }

    std::vector<Converter_p> createConverter(Environment& env, const Operation& oper) const;
};

Converter_pv AddPressure4DConverterFactory::createConverter(Environment& env, const Operation& oper) const
{
    const std::string& pressureName = oper.option(AIR_PRESSURE4D);
    if (env.outputCDM->hasVariable(pressureName)) {
        LOG4FIMEX(logger, Logger::WARN, "variable '" << pressureName << "' exists, not adding conversion");
        return Converter_pv();
    }

    VerticalConverter_p vc;
    size_t dimsizeOld = 0;
    vector<string> shape4d;
    for (CoordinateSystem_cp_v::const_iterator it = env.inputCoordSys.begin(); it != env.inputCoordSys.end(); ++it) {
        CoordinateSystem_cp csi = *it;
        CoordinateAxis_cp csiX = csi->getGeoXAxis();
        CoordinateAxis_cp csiY = csi->getGeoYAxis();
        CoordinateAxis_cp csiZ = csi->getGeoZAxis();
        CoordinateAxis_cp csiT = csi->getTimeAxis();
        if (!(csiX && csiY && csiZ && csiT))
            continue;

        // use the largest coordinate-system with largest zAxis
        const size_t dimsize = env.inputCDM.getDimension(csi->getGeoZAxis()->getShape()[0]).getLength();
        if (vc && dimsize <= dimsizeOld)
            continue;

        if (!csi->hasVerticalTransformation())
            continue;

        VerticalConverter_p vconv;
        try {
            vconv = csi->getVerticalTransformation()->getConverter(env.inputReader, csi, MIFI_VINT_PRESSURE);
            if (!vconv)
                continue;
        } catch (CDMException& ex) {
            LOG4FIMEX(logger, Logger::WARN, "exception when retrieving pressure converter for coordinate system, skipped: " << ex.what());
        }

        vector<string> pshape(4);
        pshape[0] = csiX->getShape().front();
        pshape[1] = csiY->getShape().front();
        pshape[2] = csiZ->getShape().front();
        pshape[3] = csiT->getShape().front();

        bool hasOtherDimensions = false;
        const vector<string> vshape = vconv->getShape();
        for (vector<string>::const_iterator itS = vshape.begin(); itS != vshape.end(); ++itS) {
            if (std::find(pshape.begin(), pshape.end(), *itS) != pshape.end())
                continue;
            const size_t length = env.inputCDM.getDimension(*itS).getLength();
            if (length > 1) {
                hasOtherDimensions = true;
                LOG4FIMEX(logger, Logger::DEBUG, "vertical converter has dimension " << *itS << " with length > 1");
            }
        }
        if (hasOtherDimensions) {
            LOG4FIMEX(logger, Logger::DEBUG, "vertical converter has dimensions with length > 1 other than x,y,z,t, skipped");
            continue;
        }

        vc = vconv;
        dimsizeOld = dimsize;
        shape4d = pshape;
    }
    if (!vc) {
        throw CDMException("no x,y,z,t 4D-coordinate system found");
        return Converter_pv();
    }

    env.outputCDM->addVariable(CDMVariable(pressureName, CDM_FLOAT, shape4d));
    env.outputCDM->addAttribute(pressureName, CDMAttribute(UNITS, "hPa"));
    env.outputCDM->addAttribute(pressureName, CDMAttribute(STANDARD_NAME, AIR_PRESSURE));

    return Converter_pv(1, std::make_shared<AddPressure4DConverter>(pressureName, env.inputReader, vc, shape4d));
}

} // anonymous namespace

struct CDMPressureConversionsImpl {
    std::map<std::string, Converter_p> converters;
};

namespace {

std::vector<ConverterFactoryPtr> converterfactories;

void initfactories() {
    if (converterfactories.empty()) {
        converterfactories.push_back(std::make_shared<HumidityConverterFactory>());
        converterfactories.push_back(std::make_shared<ThetaTemperatureConverterFactory>());
        converterfactories.push_back(std::make_shared<OmegaVerticalConverterFactory>());
        converterfactories.push_back(std::make_shared<AddPressure4DConverterFactory>());
    }
}

} // namespace


CDMPressureConversions::CDMPressureConversions(CDMReader_p dataReader, std::vector<std::string> operations)
    : dataReader_(dataReader), p_(new CDMPressureConversionsImpl())
{
    initfactories();

    *cdm_ = dataReader_->getCDM();
    ConverterFactory::Environment env(dataReader_, cdm_);

    for (vector<string>::iterator op = operations.begin(); op != operations.end(); ++op) {
        ConverterFactory::Operation oper;
        oper.conversion = *op;
        const char SEP = ';', EQ = '=';
        std::string::size_type pos_sep = oper.conversion.find(SEP);
        if (pos_sep != string::npos) {
            oper.conversion.erase(pos_sep);

            while (pos_sep != string::npos && pos_sep + 1 < op->size()) {
                std::string::size_type pos_opt_start = pos_sep + 1;
                std::string::size_type pos_opt_eq = op->find(EQ, pos_opt_start);
                if (pos_opt_eq == std::string::npos) {
                    const std::string key = op->substr(pos_opt_start);
                    oper.options.insert(std::make_pair(key, ""));
                    LOG4FIMEX(logger, Logger::DEBUG, "oper.option '" << key << "' (empty)");
                    break;
                }

                std::string::size_type pos_opt_end = op->find(SEP, pos_opt_eq);
                pos_sep = pos_opt_end;
                if (pos_opt_end == std::string::npos)
                    pos_opt_end = op->size();
                const std::string key = op->substr(pos_opt_start, pos_opt_eq - pos_opt_start);
                const std::string val = op->substr(pos_opt_eq+1, pos_opt_end - pos_opt_eq-1);
                oper.options.insert(std::make_pair(key, val));
                LOG4FIMEX(logger, Logger::DEBUG, "oper.option '" << key << "'=>'" << val << "'");
            }
            LOG4FIMEX(logger, Logger::DEBUG, "oper.conversion='" << oper.conversion << "'");
        }

        int best_match = 0;
        ConverterFactoryPtr best_factory;
        for (std::vector<ConverterFactoryPtr>::const_iterator it = converterfactories.begin(); it != converterfactories.end(); ++it) {
            int match = (*it)->match(env, oper.conversion);
            if (match > best_match) {
                best_match = match;
                best_factory = *it;
            }
        }
        if (best_factory) {
            LOG4FIMEX(logger, Logger::DEBUG, "factory: '" << best_factory->name() << "'");

            Converter_pv convs = best_factory->createConverter(env, oper);
            for (Converter_pv::const_iterator itC = convs.begin(); itC != convs.end(); ++itC) {
                const std::string& varName = (*itC)->getName();
                p_->converters.insert(std::make_pair(varName, *itC));
            }
        } else {
            throw CDMException("unknown CDMPressureConversion-operation: '" + oper.conversion + "'");
        }
    }
}

CDMPressureConversions::~CDMPressureConversions() {}

DataPtr CDMPressureConversions::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    std::map<std::string, Converter_p>::const_iterator itC = p_->converters.find(varName);
    if (itC == p_->converters.end()) {
        return dataReader_->getDataSlice(varName, unLimDimPos);
    } else {
        return (itC->second)->getDataSlice(unLimDimPos);
    }
}

}
