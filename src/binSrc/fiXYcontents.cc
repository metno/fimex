/*
 * Fimex, fiXYcontents.cc
 *
 * (C) Copyright 2015, met.no
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
 *  Created on: Apr 7, 2015
 *      Author: Heiko Klein
 */

#include "fimex/CDM.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/StringUtils.h"
#include "fimex/ThreadPool.h"
#include "fimex/TimeUnit.h"
#include "fimex/TokenizeDotted.h"
#include "fimex/Units.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include <mi_programoptions.h>

#include <functional>
#include <iostream>
#include <numeric>

namespace po = miutil::program_options;
using namespace std;
using namespace MetNoFimex;

namespace {

void writeUsage(ostream& out, const po::option_set& generic)
{
    out << "usage: fiXYcontents --input.file  FILENAME [--input.type  INPUT_TYPE]" << endl;
    out << "             [--input.config CFGFILENAME]" << endl;
    out << "             [--input.optional OPT1 --input.optional OPT2 ...]" << endl;
    out << "             [--num_threads ...]" << endl;
    out << "             [--verticalLayer [units:hPa|no|stdname]]" << endl;
    out << "             [--layerValue ... | layerPosition ...]" << endl;
    out << "             [--varName VAR1 --varName VAR2 --stdName STDNM3]" << endl;
    out << "             [--forecastTime ... ]" << endl;
    out << "             [--stats mean,median,min,max,stddev,def,undef]" << endl;
    out << endl;
    generic.help(out);
}

const po::option op_help = po::option("help", "help message").set_shortkey("h");
const po::option op_version = po::option("version", "program version");
const po::option op_debug = po::option("debug", "debug program");
const po::option op_log4cpp = po::option("log4cpp", "log4cpp property file (- = log4cpp without prop-file)");
const po::option op_num_threads = po::option("num_threads", "number of threads").set_shortkey("n");
const po::option op_input_file = po::option("input.file", "input file");
const po::option op_input_type = po::option("input.type", "filetype of input file, e.g. nc, nc4, ncml, felt, grib1, grib2");
const po::option op_input_config = po::option("input.config", "non-standard input configuration");
const po::option op_input_optional = po::option("input.optional", "additional options, e.g. multiple files for grib").set_composing();
const po::option op_verticalLayer = po::option("verticalLayer", "vertical layer definitions, defined by standard_name or units (or no for no (or size 1) "
                                                                "vertical axis), e.g. units:hPa, no, atmosphere_hybrid_sigma_pressure_coordinate")
                                        .set_composing();
const po::option op_layerValue = po::option("layerValue", "vertical layer values, e.g. 1000,950,...,500,300,100");
const po::option op_layerPosition = po::option("layerPosition", "vertical layer positions, e.g. 0,1,2,...,90");
const po::option op_stdName = po::option("stdName", "standard_name of parameters").set_composing();
const po::option op_varName = po::option("varName", "variable name of parameters").set_composing();
const po::option op_forecastTime = po::option("forecastTime", "forecast hours since reference time, e.g. 3,6,12,15,...,24,36");
const po::option op_stats = po::option("stats", "comma-separated list of stats to show, possible: mean,median,min,max,stddev,def,undef (=all)");

// FIXME same as fimex.cc
CDMReader_p getCDMFileReader(po::value_set& vm)
{
    const string& name = vm.value(op_input_file);
    string type;
    if (vm.is_set(op_input_type)) {
        type = vm.value(op_input_type);
    } else {
        type = mifi_get_filetype_name(CDMFileReaderFactory::detectFileType(name));
    }
    string config;
    if (vm.is_set(op_input_config)) {
        config = vm.value(op_input_config);
    }
    vector<string> opts;
    if (vm.is_set(op_input_optional)) {
        opts = vm.values(op_input_optional);
    }
    return CDMFileReaderFactory::create(type, name, config, opts);
}

double quantile(vector<double> &v, float quant)
{
    size_t n = v.size() * quant;
    nth_element(v.begin(), v.begin()+n, v.end());
    return v.at(n);
}

map<string, double> calcStats(DataPtr data)
{
    map<string, double> stats;
    vector<double> vec;
    size_t undefs = 0;
    { // count and remove undefs
        vec.reserve(data->size());
        shared_array<double> array = data->asDouble();
        for (size_t i = 0; i < data->size(); i++) {
            if (mifi_isnan(array[i])) {
                undefs++;
            } else {
                vec.push_back(array[i]);
            }
        }
    }
    stats["undef"] = undefs;
    stats["def"] = data->size() - undefs;

    if (vec.size() > 0) {
        double sum = std::accumulate(vec.begin(), vec.end(), 0.0);
        double mean = sum / vec.size();
        double sq_sum = 0., min = vec.at(0), max = vec.at(0);
        for (vector<double>::iterator vit = vec.begin(); vit != vec.end(); ++vit) {
            sq_sum += (*vit - mean) * (*vit - mean);
            max = std::max(max, *vit);
            min = std::min(min, *vit);
        }
        stats["stddev"] = std::sqrt(sq_sum / (vec.size()-1));
        stats["median"] = quantile(vec, .5);
        stats["sum"] = sum;
        stats["mean"] = mean;
        stats["min"] = min;
        stats["max"] = max;
    }
    return stats;
}

void runStats(po::value_set& vm, CDMReader_p reader)
{
    CoordinateSystem_cp_v coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();
    const CDM::VarVec& variables = cdm.getVariables();
    set<string> varNames;
    if (vm.is_set(op_varName) || vm.is_set(op_stdName)) {
        if (vm.is_set(op_varName)) {
            const vector<string>& vars = vm.values(op_varName);
            varNames.insert(vars.begin(), vars.end());
        }
        if (vm.is_set(op_stdName)) {
            const vector<string>& stds = vm.values(op_stdName);
            for (const string& std : stds) {
                const vector<string> vars = cdm.findVariables("standard_name", std + "(\\s.*)?");
                varNames.insert(vars.begin(), vars.end());
            }
        }
    } else {
        for (CDM::VarVec::const_iterator varIt = variables.begin(); varIt != variables.end(); ++varIt) {
            varNames.insert(varIt->getName());
        }
    }

    enum VLType {
        vlUnit, vlStd, vlNo
    };
    VLType vlType = vlStd;
    string vlName = "not_defined";
    if (vm.is_set(op_verticalLayer)) {
        const string& vl = vm.value(op_verticalLayer);
        if (vl == "no") {
            vlType = vlNo;
        } else if (vl.substr(0,6) == "units:") {
            vlType = vlUnit;
            vlName = vl.substr(6);
        } else {
            vlType = vlStd;
            vlName = vl;
        }
    }

    // find an appropriate coordinate system for a variable
    Units units;
    for (set<string>::const_iterator varIt = varNames.begin(); varIt != varNames.end(); ++varIt) {
        size_t xSize = 0;
        size_t ySize = 0;
        vector<CoordinateSystemSliceBuilder> csbs;
        DataPtr tData;
        DataPtr zData;
        FimexTime refTime(FimexTime::max_date_time);
        map<string, DataPtr> dimData; // hours since refTime

        CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordSys, *varIt);
        if (cs.get()) {
            if (vm.is_set(op_verticalLayer)) {
                CoordinateAxis_cp zAxis = cs->getGeoXAxis(); // X or Lon
                bool use = false;
                switch (vlType) {
                    case vlNo: if (zAxis == 0 || zAxis->getData()->size() <= 1) use = true; break;
                    case vlUnit: if (zAxis != 0) {
                        CDMAttribute attr;
                        if (cdm.getAttribute(zAxis->getName(), "units", attr)) {
                            if (units.areConvertible(attr.getStringValue(), vlName)) {
                                use = true;
                            }
                        }
                    } break;
                    case vlStd: if (zAxis != 0) {
                        CDMAttribute attr;
                        if (cdm.getAttribute(zAxis->getName(), "standard_name", attr)) {
                            if (attr.getStringValue().find(vlName) != std::string::npos) {
                                use = true;
                            }
                        }
                    } break;
                }
                if (use == false) continue;
            }
            if (cs->isSimpleSpatialGridded()) {
                CoordinateAxis_cp xAxis = cs->getGeoXAxis(); // X or Lon
                CoordinateAxis_cp yAxis = cs->getGeoYAxis(); // Y or Lat
                xSize = cdm.getDimension(xAxis->getName()).getLength();
                ySize = cdm.getDimension(yAxis->getName()).getLength();
                CoordinateAxis_cp tAxis = cs->getTimeAxis(); // time

                CoordinateSystemSliceBuilder sb(cdm, cs);
                // handling of time
                try {
                    refTime = getUniqueForecastReferenceTimeFT(reader);
                } catch (runtime_error& rex) {
                    // ignore
                }
                if (tAxis.get() != 0) {
                    // time-Axis, eventually multi-dimensional, i.e. forecast_reference_time
                    if (cs->hasAxisType(CoordinateAxis::ReferenceTime)) {
                        CoordinateAxis_cp rtAxis = cs->findAxisOfType(CoordinateAxis::ReferenceTime);
                        DataPtr refTimes = reader->getScaledDataInUnit(rtAxis->getName(),"minutes since 1970-01-01 00:00:00 +00:00");
                        TimeUnit tu("minutes since 1970-01-01 00:00:00 +00:00");
                        /* do something with the refTimes and select the wanted Position */
                        size_t refTimePos = refTimes->size()-1; /* choose latest refTime */
                        sb.setReferenceTimePos(refTimePos);
                        refTime = tu.unitTime2fimexTime(refTimes->asDouble()[refTimes->size() - 1]);
                    }
                    if (is_invalid_time_point(refTime)) {
                        // use first time-stamp
                        TimeUnit tu("minutes since 1970-01-01 00:00:00 +00:00");
                        tData = reader->getScaledDataSliceInUnit(tAxis->getName(), "minutes since 1970-01-01 00:00:00 +00:00", sb.getTimeVariableSliceBuilder());
                        refTime = tu.unitTime2fimexTime(tData->asDouble()[0]);
                    }
                    tData = reader->getScaledDataSliceInUnit(tAxis->getName(), "hours since " + make_time_string_extended(refTime),
                                                             sb.getTimeVariableSliceBuilder());
                    shared_array<float> tArray = tData->asFloat();
                    set<long> forecastTimes60;
                    if (vm.is_set(op_forecastTime)) {
                        vector<float> ft = tokenizeDotted<float>(vm.value(op_forecastTime), ",");
                        for (size_t i = 0; i < ft.size(); i++) {
                            forecastTimes60.insert(ft.at(i)*60);
                        }
                    }
                    for (size_t i = 0; i < tData->size(); i++) {
                        if (forecastTimes60.size() > 0) {
                            long t60 = tArray[i] * 60;
                            if (forecastTimes60.find(t60) == forecastTimes60.end()) {
                                continue; // skip time
                            }
                        }
                        sb.setTimeStartAndSize(i, 1);
                        sb.setAll(xAxis);
                        sb.setAll(yAxis);
                        csbs.push_back(sb);
                    }
                }

                CoordinateAxis_cp zAxis = cs->getGeoZAxis(); // Y or Lat
                if (zAxis != 0) {
                    if (vlType == vlUnit) {
                        zData = reader->getScaledDataInUnit(zAxis->getName(), vlName);
                    } else {
                        zData = reader->getScaledData(zAxis->getName());
                    }
                    vector<CoordinateSystemSliceBuilder> csbs_temp;
                    set<size_t> layerPos;
                    if (vm.is_set(op_layerPosition)) {
                        vector<size_t> lp = tokenizeDotted<size_t>(vm.value(op_layerPosition));
                        layerPos.insert(lp.begin(), lp.end());
                    }
                    set<long> layerVal10;
                    if (vm.is_set(op_layerValue)) {
                        vector<float> lp = tokenizeDotted<float>(vm.value(op_layerValue));
                        transform(lp.begin(), lp.end(), lp.begin(), bind1st(multiplies<float>(),10.));
                        layerVal10.insert(lp.begin(), lp.end());
                    }
                    shared_array<float> zArray = zData->asFloat();
                    for (vector<CoordinateSystemSliceBuilder>::iterator sbIt = csbs.begin(); sbIt != csbs.end(); ++sbIt) {
                        for (size_t i = 0; i < zData->size(); i++) {
                            if (layerPos.size() && layerPos.find(i) == layerPos.end()) continue;
                            if (layerVal10.size() && layerVal10.find(zArray[i]*10) == layerVal10.end()) continue;
                            sbIt->setStartAndSize(zAxis, i, 1);
                            csbs_temp.push_back(*sbIt);
                        }
                    }
                    csbs = csbs_temp;
                }

                // by default, all other dimensions are fetched at maximum size
                // here, I reduce them to the first slice
                if (csbs.size() > 0) {
                    vector<string> dims = csbs.at(0).getUnsetDimensionNames();
                    for (vector<string>::iterator dim = dims.begin(); dim != dims.end(); ++dim) {
                        dimData[*dim] = reader->getScaledData(*dim);
                        vector<CoordinateSystemSliceBuilder> csbs_temp;
                        for (vector<CoordinateSystemSliceBuilder>::iterator sbIt = csbs.begin(); sbIt != csbs.end(); ++sbIt) {
                            for (size_t i = 0; i < dimData[*dim]->size(); i++) {
                                sbIt->setStartAndSize(*dim, i, 1);
                                csbs_temp.push_back(*sbIt);
                            }
                        }
                        csbs = csbs_temp;
                    }
                }

                /* do something with the data */
            } else {
                // var not simple spatial gridded
            }
        } else {
            // var without coordsys (ignore on default)
        }


        if (csbs.size() > 0) {
            CDMAttribute attr;
            string stdName, unit;
            if (cdm.getAttribute(*varIt, "standard_name", attr)) {
                stdName = attr.getStringValue();
            }
            unit = cdm.getUnits(*varIt);
            printf("Var: %17s %17s %10s %15s %ldx%ld: %ld\n", varIt->c_str(), stdName.c_str(), unit.c_str(), make_time_string(refTime).c_str(), xSize, ySize,
                   csbs.size());
            shared_array<float> tArray, zArray;
            if (tData.get() != 0) tArray = tData->asFloat();
            if (zData.get() != 0) zArray = zData->asFloat();
            for (vector<CoordinateSystemSliceBuilder>::const_iterator sbIt = csbs.begin(); sbIt != csbs.end(); ++sbIt) {
                vector<string> dimNames = sbIt->getDimensionNames();
                vector<size_t> pos = sbIt->getDimensionStartPositions();
                vector<CoordinateAxis::AxisType> axisTypes = sbIt->getAxisTypes();
                printf("  ");
                for (long i = dimNames.size()-1; i >= 0; i--) {
                    switch (axisTypes.at(i)) {
                    case CoordinateAxis::Time:
                        printf("t=%ld(%.1f) ", pos.at(i), tArray[pos.at(i)]);
                        break;
                    case CoordinateAxis::GeoZ:
                    case CoordinateAxis::Pressure:
                    case CoordinateAxis::Height:
                    case CoordinateAxis::Depth:
                        printf("k=%ld(%.1f) ", pos.at(i), zArray[pos.at(i)]);
                        break;
                    case CoordinateAxis::GeoX:
                    case CoordinateAxis::GeoY:
                    case CoordinateAxis::Lon:
                    case CoordinateAxis::Lat:
                        break; // do nothing on horizontal
                    case CoordinateAxis::Undefined:
                    default:
                        printf("%10s=%.1f ", dimNames.at(i).c_str(), dimData[dimNames.at(i)]->asFloat()[pos.at(i)]);
                        break;
                    }
                }
                // fetch the data
                if (vm.is_set(op_stats)) {
                    DataPtr data = reader->getScaledDataSlice(*varIt, *sbIt);
                    map<string, double> stats = calcStats(data);
                    string statStr = vm.value(op_stats);
                    if (statStr == "all" || statStr == "") {
                        statStr = "def,mean,median,stddev,min,max,undef";
                    }

                    vector<string> vs = tokenize(statStr, ",");
                    for (size_t i = 0; i < vs.size(); ++i) {
                        if (vs.at(i) == "def") {
                            if (stats["def"] > 0) {
                                printf("def=T ");
                            } else {
                                printf("def=F ");
                            }
                        } else if (vs.at(i) == "median") {
                            if (stats["def"] > 0) printf("median=%.1f ", stats["median"]);
                        } else if (vs.at(i) == "mean") {
                            if (stats["def"] > 0) printf("mean=%.1f ", stats["mean"]);
                        } else if (vs.at(i) == "stddev") {
                            if (stats["def"] > 0) printf("stddev=%.1f ", stats["stddev"]);
                        } else if (vs.at(i) == "sum") {
                            if (stats["def"] > 0) printf("sum=%.1f ", stats["sum"]);
                        } else if (vs.at(i) == "min") {
                            if (stats["def"] > 0) printf("min=%.1f ", stats["min"]);
                        } else if (vs.at(i) == "max") {
                            if (stats["def"] > 0) printf("max=%.1f ", stats["max"]);
                        } else if (vs.at(i) == "undef") {
                            if (stats["def"] > 0) printf("undef=%d ", static_cast<int>(stats["undef"]));
                        } else {
                            printf("%s=unkown ", vs.at(i).c_str());
                        }
                    }
                }
                cout << endl;
            }
        }
    }
}

int run(int argc, char* args[])
{
    po::option_set generic;
    generic
        << op_help
        << op_version
        << op_debug
        << op_log4cpp
        << op_num_threads
        << op_input_file
        << op_input_type
        << op_input_config
        << op_input_optional
        << op_verticalLayer
        << op_layerValue
        << op_layerPosition
        << op_stdName
        << op_varName
        << op_forecastTime
        << op_stats
        ;

    po::string_v positional;
    po::value_set vm = po::parse_command_line(argc, args, generic, positional);
    if (argc == 1 || vm.is_set(op_help)) {
        writeUsage(cout, generic);
        return 0;
    }
    if (vm.is_set(op_version)) {
        cout << "fimex version " << fimexVersion() <<" (" << mifi_version_major() << "." << mifi_version_minor() << "." << mifi_version_patch() << "." << mifi_version_status() << ")" << endl;
        return 0;
    }

    defaultLogLevel(Logger::WARN);
    if (vm.is_set(op_log4cpp)) {
#ifdef HAVE_LOG4CPP
        Logger::setClass(Logger::LOG4CPP);
        std::string propFile = vm["log4cpp"].as<string>();
        if (propFile != "-") {
            log4cpp::PropertyConfigurator::configure(propFile);
        }
#else
        defaultLogLevel(Logger::DEBUG);
#endif
    }
    if (vm.is_set(op_debug)) {
        defaultLogLevel(Logger::DEBUG);
    }
    int num_threads = 1;
    if (vm.is_set(op_num_threads))
        num_threads = string2type<int>(vm.value(op_num_threads));
    mifi_setNumThreads(num_threads);

    Logger_p logger = getLogger("fimex");
    if (!(vm.is_set(op_input_file))) {
        writeUsage(cerr, generic);
        LOG4FIMEX(logger, Logger::FATAL, "input.file required");
        return 1;
    }

    CDMReader_p dataReader = getCDMFileReader(vm);
    runStats(vm, dataReader);

    return 0;
}

} // namespace

int main(int argc, char* args[])
{
    // wrapping main-functions in run to catch all exceptions
#ifndef DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN
    try {
#else
#warning Not catching exceptions in main method
#endif

        return run(argc, args);

#ifndef DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN
    } catch (const po::option_error& ex) {
        clog << "invalid options: " << ex.what() << endl;
        return 1;
    } catch (exception& ex) {
        clog << "exception occured: " << ex.what() << endl;
        return 1;
    }
#endif
}

