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

#include "../../config.h"
#include <iostream>
#include <fstream>
#include <cctype>
#include <numeric>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/version.hpp>
#include <boost/program_options.hpp>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>
#include <boost/shared_ptr.hpp>
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMExtractor.h"
#include "fimex/CDMQualityExtractor.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/CDMVerticalInterpolator.h"
#include "fimex/CDMPressureConversions.h"
#include "fimex/CDMProcessor.h"
#include "fimex/CDMMerger.h"
#include "fimex/CDMBorderSmoothing_Linear.h"
#include "fimex/FillWriter.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"
#include "fimex/TimeUnit.h"
#include "fimex/ThreadPool.h"
#include "fimex/Utils.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/NcmlCDMReader.h"
/* currently no factory for writer */
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NetCDF_CDMReader.h"
#endif
#ifdef HAVE_GRIB_API_H
#include "fimex/GribApiCDMWriter.h"
#endif
#ifdef HAVE_METGM_H
#include "fimex/MetGmCDMWriter.h"
#endif
#ifdef HAVE_LOG4CPP
#include "log4cpp/PropertyConfigurator.hh"
#endif

namespace po = boost::program_options;
using namespace std;
using namespace MetNoFimex;

static LoggerPtr logger = getLogger("fimex");

static void writeUsage(ostream& out, const po::options_description& generic, const po::options_description& config) {
    out << "usage: fimex --input.file  FILENAME [--input.type  INPUT_TYPE]" << endl;
    out << "             [--output.file FILENAME | --output.fillFile [--output.type OUTPUT_TYPE]]" << endl;
    out << "             [--input.config CFGFILENAME] [--output.config CFGFILENAME]" << endl;
    out << "             [--input.optional OPT1 --input.optional OPT2 ...]" << endl;
    out << "             [--num_threads ...]" << endl;
    out << "             [--process....]" << endl;
    out << "             [--qualityExtract....]" << endl;
    out << "             [--extract....]" << endl;
    out << "             [--interpolate....]" << endl;
    out << "             [--verticalInterpolate....]" << endl;
    out << "             [--timeInterpolate....]" << endl;
    out << "             [--merge....]" << endl;
    out << "             [--qualityExtract2....]" << endl;
    out << "             [--ncml.config NCMLFILE]" << endl;
    out << endl;
    out << generic << endl;
    out << config << endl;
}

static void printReaderStatements(const string& readerName, const po::variables_map& vm, boost::shared_ptr<CDMReader> reader)
{
    if (vm.count(readerName+".printNcML")) {
        cout << readerName << " as NcML:" << endl;
        if (vm[readerName+".printNcML"].as<string>() == "-") {
            reader->getCDM().toXMLStream(cout);
            cout << endl;
        } else {
            ofstream file;
            file.open(vm[readerName+".printNcML"].as<string>().c_str(), ios::out);
            if (file.is_open()) {
                reader->getCDM().toXMLStream(file);
                file.close();
            } else {
                throw CDMException("cannot write ncml-file: " + vm[readerName+".printNcML"].as<string>());
            }
        }
    }
    if (vm.count(readerName+".printCS")) {
        cout << readerName + " CoordinateSystems: ";
        vector<boost::shared_ptr<const CoordinateSystem> > csVec = listCoordinateSystems(reader);
        cout << csVec.size() << ": ";
        cout << joinPtr(csVec.begin(), csVec.end(), " | ");
        cout << endl;
    }
    if (vm.count(readerName+".printSize")) {
        cout << readerName << " size: ~" << ceil(estimateCDMDataSize(reader->getCDM())/1024./1024.) << "MB";
        cout << endl;
    }
}

template<typename T>
static void writeOption(ostream& out, const string& var, const po::variables_map& vm) {
    if (vm.count(var)) {
        out << var << ": ";
#if defined(__GNUC__) && __GNUC__ < 4
// Use erroneous syntax hack to work around a compiler bug.
        out << vm[var].template as<T>();
#else
        out << vm[var].as<T>();
#endif
        out << endl;
    }
}

void writeOptionAny(ostream& out, const string& var, const po::variables_map& vm) {
    // variables without real value, just set or unset
    if (vm.count(var)) {
        out << var  << endl;
    }
}

static void writeVectorOptionString(ostream& out, const string& var, const po::variables_map& vm) {
    if (vm.count(var)) {
        vector<string> vals = vm[var].as<vector<string> >();
        typedef vector<string>::iterator VIT;
        for (VIT it = vals.begin(); it != vals.end(); ++it) {
            out << var << ": " << *it << endl;
        }
    }
}
static void writeVectorOptionInt(ostream& out, const string& var, const po::variables_map& vm) {
    if (vm.count(var)) {
        vector<int> vals = vm[var].as<vector<int> >();
        typedef vector<int>::iterator VIT;
        for (VIT it = vals.begin(); it != vals.end(); ++it) {
            out << var << ": " << *it << endl;
        }
    }
}



static void writeOptions(ostream& out, const po::variables_map& vm) {
    out << "Currently active options: " << endl;
    writeOptionAny(out, "help", vm);
    writeOptionAny(out, "version", vm);
    writeOption<string>(out, "log4cpp", vm);
    writeOptionAny(out, "debug", vm);
    writeOptionAny(out, "print-options", vm);
    writeOption<string>(out, "config", vm);
    writeOption<int>(out, "num_threads", vm);
    writeOption<string>(out, "input.file", vm);
    writeOption<string>(out, "input.type", vm);
    writeOption<string>(out, "input.config", vm);
    writeVectorOptionString(out, "input.optional", vm);
    writeOption<string>(out, "input.printNcML", vm);
    writeOptionAny(out, "input.printCS", vm);
    writeOptionAny(out, "input.printSize", vm);
    writeOption<string>(out, "output.file", vm);
    writeOption<string>(out, "output.fillFile", vm);
    writeOption<string>(out, "output.type", vm);
    writeOption<string>(out, "output.config", vm);
    writeOption<string>(out, "output.printNcML", vm);
    writeOptionAny(out, "output.printCS", vm);
    writeOptionAny(out, "output.printSize", vm);
    writeVectorOptionString(out, "process.accumulateVariable", vm);
    writeVectorOptionString(out, "process.deaccumulateVariable", vm);
    writeVectorOptionString(out, "process.rotateVectorToLatLonX", vm);
    writeVectorOptionString(out, "process.rotateVectorToLatLonY", vm);
    writeVectorOptionString(out, "process.rotateVector.x", vm);
    writeVectorOptionString(out, "process.rotateVector.y", vm);
    writeOptionAny(out, "process.rotateVector.all", vm);
    writeVectorOptionString(out, "process.rotateVector.stdNameX", vm);
    writeVectorOptionString(out, "process.rotateVector.stdNameY", vm);
    writeOptionAny(out, "process.addVerticalVelocity", vm);
    writeOption<string>(out, "process.printNcML", vm);
    writeOption<string>(out, "process.printCS", vm);
    writeOption<string>(out, "process.printSize", vm);
    writeOption<string>(out, "qualityExtract.autoConfigString", vm);
    writeOption<string>(out, "qualityExtract.config", vm);
    writeOption<string>(out, "qualityExtract.printNcML", vm);
    writeOption<string>(out, "qualityExtract.printCS", vm);
    writeOption<string>(out, "qualityExtract.printSize", vm);
    writeVectorOptionString(out, "extract.removeVariable", vm);
    writeVectorOptionString(out, "extract.selectVariables", vm);
    writeOptionAny(out, "extract.selectVariables.noAuxiliary", vm);
    writeVectorOptionString(out, "extract.reduceDimension.name", vm);
    writeVectorOptionInt(out, "extract.reduceDimension.start", vm);
    writeVectorOptionInt(out, "extract.reduceDimension.end", vm);
    writeOption<string>(out, "extract.reduceTime.start", vm);
    writeOption<string>(out, "extract.reduceTime.end", vm);
    writeOption<string>(out, "extract.reduceVerticalAxis.unit", vm);
    writeOption<double>(out, "extract.reduceVerticalAxis.start", vm);
    writeOption<double>(out, "extract.reduceVerticalAxis.end", vm);
    writeOption<double>(out, "extract.reduceToBoundingBox.south", vm);
    writeOption<double>(out, "extract.reduceToBoundingBox.north", vm);
    writeOption<double>(out, "extract.reduceToBoundingBox.west", vm);
    writeOption<double>(out, "extract.reduceToBoundingBox.east", vm);
    writeOption<string>(out, "extract.printNcML", vm);
    writeOptionAny(out, "extract.printCS", vm);
    writeOptionAny(out, "extract.printSize", vm);
    writeOption<string>(out, "interpolate.projString", vm);
    writeOption<string>(out, "interpolate.method", vm);
    writeOption<string>(out, "interpolate.latitudeValues", vm);
    writeOption<string>(out, "interpolate.longitudeValues", vm);
    writeOption<string>(out, "interpolate.vcrossNames", vm);
    writeOption<string>(out, "interpolate.vcrossNoPoints", vm);
    writeOption<string>(out, "interpolate.xAxisValues", vm);
    writeOption<string>(out, "interpolate.yAxisValues", vm);
    writeOption<string>(out, "interpolate.xAxisUnit", vm);
    writeOption<string>(out, "interpolate.yAxisUnit", vm);
    writeOption<string>(out, "interpolate.xAxisType", vm);
    writeOption<string>(out, "interpolate.yAxisType", vm);
    writeOption<double>(out, "interpolate.distanceOfInterest", vm);
    writeOption<string>(out, "interpolate.latitudeName", vm);
    writeOption<string>(out, "interpolate.longitudeName", vm);
    writeOption<string>(out, "interpolate.preprocess", vm);
    writeOption<string>(out, "interpolate.printNcML", vm);
    writeOptionAny(out, "interpolate.printCS", vm);
    writeOptionAny(out, "interpolate.printSize", vm);
    writeOption<string>(out, "verticalInterpolate.method", vm);
    writeOption<string>(out, "verticalInterpolate.type", vm);
    writeOption<string>(out, "verticalInterpolate.level1", vm);
    writeOption<string>(out, "verticalInterpolate.level2", vm);
    writeVectorOptionString(out, "verticalInterpolate.dataConversion", vm);
    writeOption<string>(out, "verticalInterpolate.printNcML", vm);
    writeOptionAny(out, "verticalInterpolate.printCS", vm);
    writeOptionAny(out, "verticalInterpolate.printSize", vm);
    writeOption<string>(out, "timeInterpolate.timeSpec", vm);
    writeOption<string>(out, "timeInterpolate.printNcML", vm);
    writeOptionAny(out, "timeInterpolate.printCS", vm);
    writeOptionAny(out, "timeInterpolate.printSize", vm);
    writeOption<string>(out, "merge.inner.file", vm);
    writeOption<string>(out, "merge.inner.type", vm);
    writeOption<string>(out, "merge.inner.config", vm);
    writeOption<string>(out, "merge.smoothing", vm);
    writeOption<string>(out, "merge.method", vm);
    writeOption<string>(out, "merge.projString", vm);
    writeOption<string>(out, "merge.xAxisValues", vm);
    writeOption<string>(out, "merge.yAxisValues", vm);
    writeOption<string>(out, "merge.xAxisUnit", vm);
    writeOption<string>(out, "merge.yAxisUnit", vm);
    writeOption<string>(out, "merge.xAxisType", vm);
    writeOption<string>(out, "merge.yAxisType", vm);
    writeOption<string>(out, "merge.printNcML", vm);
    writeOptionAny(out, "merge.printCS", vm);
    writeOptionAny(out, "merge.printSize", vm);
    writeOption<string>(out, "qualityExtract2.autoConfigString", vm);
    writeOption<string>(out, "qualityExtract2.config", vm);
    writeOption<string>(out, "qualityExtract2.printNcML", vm);
    writeOption<string>(out, "qualityExtract2.printCS", vm);
    writeOption<string>(out, "qualityExtract2.printSize", vm);
    writeOption<string>(out, "ncml.config", vm);
    writeOption<string>(out, "ncml.printNcML", vm);
    writeOptionAny(out, "ncml.printCS", vm);
    writeOptionAny(out, "ncml.printSize", vm);
}

static string getType(const string& io, po::variables_map& vm) {
    string type;
    if (vm.count(io+".type")) {
        type = vm[io+".type"].as<string>();
    } else {
        string file;
        if (vm.count(io+".file")) {
            file = ".file";
        } else if (vm.count(io+".fillFile")) {
            file = ".fillFile";
        }
        if (file != "") {
            boost::smatch what;
            if (boost::regex_match(vm[io+file].as<string>(), what, boost::regex(".*\\.(\\w+)$"))) {
                type = what[1].str();
            }
        }
    }
    std::transform(type.begin(), type.end(), type.begin(), (int(*)(int)) tolower);
    return type;
}

static boost::shared_ptr<CDMReader> getCDMFileReader(po::variables_map& vm, const string& io="input") {
    string type = getType(io, vm);
    boost::shared_ptr<CDMReader> returnPtr;
    if (type == "flt" || type == "dat" || type == "felt" || type == "flt2" || type == "dat2" || type == "felt2") {
        string config(PKGDATADIR);
        config += "/felt2nc_variables.xml";
        if (vm.count(io+".config")) {
            config = vm[io+".config"].as<string>();
        }
        // use FELT library for all flt2 files, and for flt files if LIBMIC not available
        LOG4FIMEX(logger, Logger::DEBUG, "reading Felt-File2 " << vm[io+".file"].as<string>() << " with config " << config);
        returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_FELT, vm[io+".file"].as<string>(), config);
    }
    if (type == "grb" || type == "grib" ||
            type == "grb1" || type == "grib1" ||
                type == "grb2" || type == "grib2") {
        std::string config;
        if (vm.count(io+".config")) {
            config = vm[io+".config"].as<string>();
        }
        std::vector<std::string> optional;
        if (vm.count(io+".optional")) {
            optional = vm[io+".optional"].as<vector<string> >();
        }
        LOG4FIMEX(logger, Logger::DEBUG, "reading GribFile " << vm[io+".file"].as<string>() << " with config " << config);
        returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_GRIB, vm[io+".file"].as<string>(), config, optional);
    }
    if (type == "nc" || type == "cdf" || type == "netcdf" || type == "nc4") {
        if (vm.count(io+".config")) {
            std::string config = vm[io+".config"].as<string>();
            LOG4FIMEX(logger, Logger::DEBUG, "reading Netcdf-File " << vm[io+".file"].as<string>() << " without config and applying ncml config");
            returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, vm[io+".file"].as<string>(), config);
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "reading Netcdf-File " << vm[io+".file"].as<string>() << " without config");
            returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, vm[io+".file"].as<string>());
        }
    }

    if (returnPtr.get() == 0) {
        // cover all other types as defined in CDMConstants/CDMFileReaderFactory
        int maxId = mifi_get_max_filetype_number();
        for (int i = 0; i <= maxId; i++) {
            const char* iType= mifi_get_filetype_name(i);
            if (type == iType) {
                string config;
                if (vm.count(io+".config")) {
                    config = vm[io+".config"].as<string>();
                }
                LOG4FIMEX(logger, Logger::DEBUG, "reading file via "<<type<<" file '" << vm[io+".file"].as<string>() << "' with config '" << config << "'");
                returnPtr = CDMFileReaderFactory::create(type, vm[io+".file"].as<string>(), config);
            }
        }
    }

    if (returnPtr.get() == 0) {
        LOG4FIMEX(logger, Logger::FATAL, "unable to read type: " << type);
        exit(1);
    } else {
        printReaderStatements(io, vm, returnPtr);
    }

    return returnPtr;
}

static int getInterpolationMethod(po::variables_map& vm, const string& key) {
    int method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
    if( vm.count(key) ) {
        const string& m = vm[key].as<string>();
        if (m == "bilinear") {
            method = MIFI_INTERPOL_BILINEAR;
        } else if (m == "nearestneighbor") {
            method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
        } else if (m == "bicubic") {
            method = MIFI_INTERPOL_BICUBIC;
        } else if (m == "coord_nearestneighbor") {
            method = MIFI_INTERPOL_COORD_NN;
        } else if (m == "coord_kdtree") {
            method = MIFI_INTERPOL_COORD_NN_KD;
        } else if (m == "forward_sum") {
            method = MIFI_INTERPOL_FORWARD_SUM;
        } else if (m == "forward_mean") {
            method = MIFI_INTERPOL_FORWARD_MEAN;
        } else if (m == "forward_median") {
            method = MIFI_INTERPOL_FORWARD_MEDIAN;
        } else if (m == "forward_max") {
            method = MIFI_INTERPOL_FORWARD_MAX;
        } else if (m == "forward_min") {
            method = MIFI_INTERPOL_FORWARD_MIN;
        } else {
            cerr << "WARNING: unknown " << key << ": " << m << " using nearestneighbor" << endl;
        }
    }
    return method;
}

static boost::shared_ptr<CDMReader> getCDMProcessor(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    if (! (vm.count("process.accumulateVariable") || vm.count("process.deaccumulateVariable") ||
            vm.count("process.rotateVectorToLatLonX") || vm.count("process.rotateVector.direction") ||
            vm.count("process.addVerticalVelocity"))) {
        LOG4FIMEX(logger, Logger::DEBUG, "process.[de]accumulateVariable or rotateVector.direction or addVerticalVelocity not found, no process used");
        return dataReader;
    }
    boost::shared_ptr<CDMProcessor> processor(new CDMProcessor(boost::shared_ptr<CDMReader>(dataReader)));
    if (vm.count("process.deaccumulateVariable")) {
        vector<string> vars = vm["process.deaccumulateVariable"].as<vector<string> >();
        for (size_t i = 0; i < vars.size(); i++) {
            processor->deAccumulate(vars.at(i));
        }
    }
    if (vm.count("process.accumulateVariable")) {
        vector<string> vars = vm["process.accumulateVariable"].as<vector<string> >();
        for (size_t i = 0; i < vars.size(); i++) {
            processor->accumulate(vars.at(i));
        }
    }
    if (vm.count("process.rotateVectorToLatLonX")) {
        vector<string> xvars = vm["process.rotateVectorToLatLonX"].as<vector<string> >();
        vector<string> yvars = vm["process.rotateVectorToLatLonY"].as<vector<string> >();
        processor->rotateVectorToLatLon(true, xvars, yvars);
    }
    if (vm.count("process.rotateVector.direction")) {
        bool toLatLon = true;
        if (vm["process.rotateVector.direction"].as<string>() == "latlon") {
            toLatLon = true;
        } else if (vm["process.rotateVector.direction"].as<string>() == "grid") {
            toLatLon = false;
        } else {
            cerr << "process.rotateVector.direction != 'latlon' or 'grid' : " << vm["process.rotateVector.direction"].as<string>() << " invalid" << endl;
            exit(1);
        }
        if (vm.count("process.rotateVector.x") && vm.count("process.rotateVector.x")) {
            vector<string> xvars = vm["process.rotateVector.x"].as<vector<string> >();
            vector<string> yvars = vm["process.rotateVector.y"].as<vector<string> >();
            vector<string> stdX, stdY;
            if (vm.count("process.rotateVector.stdNameX"))
                stdX = vm["process.rotateVector.stdNameX"].as<vector<string> >();
            if (vm.count("process.rotateVector.stdNameY"))
                stdY = vm["process.rotateVector.stdNameY"].as<vector<string> >();
            processor->rotateVectorToLatLon(toLatLon, xvars, yvars, stdX, stdY);
        } else if (vm.count("process.rotateVector.all")) {
            processor->rotateAllVectorsToLatLon(toLatLon);
        } else if (vm.count("process.rotateVector.angle")) {
            // do nothing here, but don't abort either (see below)
        } else {
            cerr << "process.rotateVector.x and process.rotateVector.y, or process.rotateVector.angle not found" << endl;
            exit(1);
        }
        if (vm.count("process.rotateVector.angle")) {
               vector<string> angles = vm["process.rotateVector.angle"].as<vector<string> >();
               processor->rotateDirectionToLatLon(toLatLon, angles);
        }
    }
    if (vm.count("process.addVerticalVelocity")) {
        processor->addVerticalVelocity();
    }
    return processor;
}

static boost::shared_ptr<CDMReader> getCDMExtractor(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    if (! (vm.count("extract.reduceDimension.name") || vm.count("extract.removeVariable") ||
           vm.count("extract.selectVariables") || vm.count("extract.reduceTime.start") ||
           vm.count("extract.reduceTime.start") || vm.count("extract.reduceVerticalAxis.unit") ||
           vm.count("extract.reduceToBoundingBox.south") || vm.count("extract.reduceToBoundingBox.north") ||
           vm.count("extract.reduceToBoundingBox.west") || vm.count("extract.reduceToBoundingBox.east"))) {
        LOG4FIMEX(logger, Logger::DEBUG, "extract.reduceDimension.name and extract.removeVariable not found, no extraction used");
        return dataReader;
    }
    boost::shared_ptr<CDMExtractor> extractor(new CDMExtractor(boost::shared_ptr<CDMReader>(dataReader)));
    if (vm.count("extract.reduceDimension.name")) {
        vector<string> vars = vm["extract.reduceDimension.name"].as<vector<string> >();
        vector<int> startPos;
        vector<int> endPos;
        if (vm.count("extract.reduceDimension.start")) {
            startPos = vm["extract.reduceDimension.start"].as<vector<int> >();
        }
        if (startPos.size() != vars.size()) {
            cerr << "extract.reduceDimension.start has not same no. of elements than extract.reduceDimension.name" << endl;
            cerr << "use start = 0 if you don't want to reduce the start-position" << endl;
        }
        if (vm.count("extract.reduceDimension.end")) {
            endPos = vm["extract.reduceDimension.end"].as<vector<int> >();
        }
        if (endPos.size() != vars.size()) {
            cerr << "extract.reduceDimension.end has not same no. of elements than extract.reduceDimension.name" << endl;
            cerr << "use end = 0 (with start != 0) if you don't want to reduce the end-position" << endl;
        }
        for (size_t i = 0; i < vars.size(); ++i) {
            if (startPos.at(i) == 0 && endPos.at(i) == 0) {
                // exception to be able to extract only first element
                extractor->reduceDimension(vars.at(i), 0, 1);
            } else {
                extractor->reduceDimensionStartEnd(vars[i], startPos[i], endPos[i]);
            }
        }
    }
    if (vm.count("extract.reduceTime.start") || vm.count("extract.reduceTime.end")) {
        FimexTime start(FimexTime::min_date_time);
        if (vm.count("extract.reduceTime.start")) {
            if (! start.parseISO8601(vm["extract.reduceTime.start"].as<string>()) ) {
                cerr << "cannot parse time " << vm["extract.reduceTime.start"].as<string>() << endl;
                exit(1);
            }
        }
        FimexTime end(FimexTime::max_date_time);
        if (vm.count("extract.reduceTime.end")) {
            if (! end.parseISO8601(vm["extract.reduceTime.end"].as<string>()) ) {
                cerr << "cannot parse time " << vm["extract.reduceTime.end"].as<string>() << endl;
                exit(1);
            }
        }
        extractor->reduceTime(start, end);
    }
    if (vm.count("extract.reduceVerticalAxis.unit")) {
        if (!(vm.count("extract.reduceVerticalAxis.start") && vm.count("extract.reduceVerticalAxis.end"))) {
            cerr << "extract.reduceVerticalAxis requires all 'start','end','unit'" << endl;
            exit(1);
        }
        string unit = vm["extract.reduceVerticalAxis.unit"].as<string>();
        double start = vm["extract.reduceVerticalAxis.start"].as<double>();
        double end = vm["extract.reduceVerticalAxis.end"].as<double>();
        extractor->reduceVerticalAxis(unit, start, end);
    }
    if (vm.count("extract.reduceToBoundingBox.south") || vm.count("extract.reduceToBoundingBox.north") ||
        vm.count("extract.reduceToBoundingBox.west") || vm.count("extract.reduceToBoundingBox.east")) {
        string bb[4] = {"south", "north", "west", "east"};
        double bbVals[4];
        for (int i = 0; i < 4; i++) {
            if (!vm.count("extract.reduceToBoundingBox."+bb[i])) {
                cerr << "extract.reduceToBoundingBox." << bb[i] << " missing";
                exit(1);
            }
            bbVals[i] = vm["extract.reduceToBoundingBox."+bb[i]].as<double>();
        }
        LOG4FIMEX(logger, Logger::DEBUG, "reduceLatLonBoudingBox(" << join(&bbVals[0], &bbVals[0]+4, ",")<<")");
        extractor->reduceLatLonBoundingBox(bbVals[0], bbVals[1], bbVals[2], bbVals[3]);
    }
    if (vm.count("extract.selectVariables")) {
        vector<string> vars = vm["extract.selectVariables"].as<vector<string> >();
        if (vm.count("extract.selectVariables.noAuxiliary")) {
            extractor->selectVariables(set<string>(vars.begin(), vars.end()), false);
        } else {
            extractor->selectVariables(set<string>(vars.begin(), vars.end()), true);
        }
    }
    if (vm.count("extract.removeVariable")) {
        vector<string> vars = vm["extract.removeVariable"].as<vector<string> >();
        for (vector<string>::iterator it = vars.begin(); it != vars.end(); ++it) {
            extractor->removeVariable(*it);
        }
    }
    printReaderStatements("extract", vm, extractor);

    return boost::shared_ptr<CDMReader>(extractor);
}

static boost::shared_ptr<CDMReader> getCDMQualityExtractor(string version, po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    string autoConf, config;
    if (vm.count("qualityExtract"+version+".autoConfigString")) autoConf = vm["qualityExtract"+version+".autoConfigString"].as<string>();
    if (vm.count("qualityExtract"+version+".config")) config = vm["qualityExtract"+version+".config"].as<string>();
    if (autoConf != "" || config != "") {
        LOG4FIMEX(logger, Logger::DEBUG, "adding CDMQualityExtractor with (" << autoConf << "," << config <<")");
        dataReader = boost::shared_ptr<CDMReader>(new CDMQualityExtractor(boost::shared_ptr<CDMReader>(dataReader), autoConf, config));
    }
    printReaderStatements("qualityExtract"+version, vm, dataReader);
    return dataReader;
}


static boost::shared_ptr<CDMReader> getCDMTimeInterpolator(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    if (! vm.count("timeInterpolate.timeSpec")) {
        return dataReader;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "timeInterpolate.timeSpec found with spec: " << vm["timeInterpolate.timeSpec"].as<string>());
    boost::shared_ptr<CDMTimeInterpolator> timeInterpolator(new CDMTimeInterpolator(boost::shared_ptr<CDMReader>(dataReader)));
    timeInterpolator->changeTimeAxis(vm["timeInterpolate.timeSpec"].as<string>());
    printReaderStatements("timeInterpolate", vm, timeInterpolator);

    return boost::shared_ptr<CDMReader>(timeInterpolator);
}

static boost::shared_ptr<CDMReader> getCDMVerticalInterpolator(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    if (! vm.count("verticalInterpolate.type")) {
        return dataReader;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "verticalInterpolate found");
    if (vm.count("verticalInterpolate.dataConversion")) {
        vector<string> operations = vm["verticalInterpolate.dataConversion"].as<vector<string> >();
        boost::shared_ptr<CDMPressureConversions> pressConv;
        try {
            pressConv = boost::shared_ptr<CDMPressureConversions>(new CDMPressureConversions(dataReader, operations));
            dataReader = pressConv;
        } catch (CDMException& ex) {
            LOG4FIMEX(logger, Logger::ERROR, "invalid verticalInterpolate.dataConversion: " + join(operations.begin(), operations.end(), ",") + " " + ex.what());
            exit(1);
        }
    }
    if (! (vm.count("verticalInterpolate.method") && vm.count("verticalInterpolate.level1"))) {
        LOG4FIMEX(logger, Logger::ERROR, "verticalInterpolate needs method and level1");
        exit(1);
    }
    vector<double> level1 = tokenizeDotted<double>(vm["verticalInterpolate.level1"].as<string>(),",");
    vector<double> level2;
    if (vm.count("verticalInterpolate.level2"))
        level2 = tokenizeDotted<double>(vm["verticalInterpolate.level2"].as<string>(),",");
    boost::shared_ptr<CDMVerticalInterpolator> vInterpolator(new CDMVerticalInterpolator(boost::shared_ptr<CDMReader>(dataReader),
                                                                                         vm["verticalInterpolate.type"].as<string>(),
                                                                                         vm["verticalInterpolate.method"].as<string>(),
                                                                                         level1,
                                                                                         level2));
    printReaderStatements("verticalInterpolate", vm, vInterpolator);

    return boost::shared_ptr<CDMReader>(vInterpolator);
}


static boost::shared_ptr<CDMReader> getCDMInterpolator(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {

    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(boost::shared_ptr<CDMReader>(dataReader)));
    if (vm.count("interpolate.latitudeName")) {
        interpolator->setLatitudeName(vm["interpolate.latitudeName"].as<string>());
    }
    if (vm.count("interpolate.longitudeName")) {
        interpolator->setLongitudeName(vm["interpolate.longitudeName"].as<string>());
    }

    if (vm.count("interpolate.preprocess")) {
        boost::smatch what;
        if (boost::regex_match(vm["interpolate.preprocess"].as<string>(), what, boost::regex("\\s*fill2d\\(([^,]+),([^,]+),([^)]+)\\).*"))) {
            double critx = string2type<double>(what[1]);
            double cor = string2type<double>(what[2]);
            size_t maxLoop = string2type<size_t>(what[3]);
            LOG4FIMEX(logger, Logger::DEBUG, "running interpolate preprocess: fill2d("<<critx<<","<<cor<<","<<maxLoop<<")");
            interpolator->addPreprocess(boost::shared_ptr<InterpolatorProcess2d>(new InterpolatorFill2d(critx,cor,maxLoop)));
        } else if (boost::regex_match(vm["interpolate.preprocess"].as<string>(), what, boost::regex("\\s*creepfill2d\\((.+)\\).*"))) {
            vector<string> vals = tokenize(what[1], ",");
            if (vals.size() == 2) {
                unsigned short repeat = string2type<unsigned short>(vals.at(0));
                char setWeight = string2type<char>(vals.at(1));
                LOG4FIMEX(logger, Logger::DEBUG, "running interpolate preprocess: creepfill2d("<<repeat<<","<<setWeight<<")");
                interpolator->addPreprocess(boost::shared_ptr<InterpolatorProcess2d>(new InterpolatorCreepFill2d(repeat, setWeight)));
            } else if (vals.size() == 3) {
                unsigned short repeat = string2type<unsigned short>(vals.at(0));
                char setWeight = string2type<char>(vals.at(1));
                float defVal = string2type<float>(vals.at(2));
                LOG4FIMEX(logger, Logger::DEBUG, "running interpolate preprocess: creepfillval2d("<<repeat<<","<<setWeight<<","<<defVal<<")");
                interpolator->addPreprocess(boost::shared_ptr<InterpolatorProcess2d>(new InterpolatorCreepFillVal2d(repeat, setWeight,defVal)));
            } else {
                throw CDMException("creepfill requires two or three arguments, got " + what[1]);
            }
        } else {
            throw CDMException("undefined interpolate.preprocess: " + vm["interpolate.preprocess"].as<string>());
        }
    }

    int method = getInterpolationMethod(vm, "interpolate.method");

    if (vm.count("interpolate.projString")) {

        if (!(vm.count("interpolate.xAxisUnit") && vm.count("interpolate.yAxisUnit"))) {
                cerr << "ERROR: xAxisUnit and yAxisUnit required" << endl;
                exit(1);
        }

        if (!(vm.count("interpolate.xAxisValues") && vm.count("interpolate.yAxisValues"))) {
            cerr << "ERROR: xAxisValues and yAxisValues required" << endl;
            exit(1);
        }
        if (vm.count("interpolate.distanceOfInterest")) {
            interpolator->setDistanceOfInterest(vm["interpolate.distanceOfInterest"].as<double>());
        }

        interpolator->changeProjection(method, vm["interpolate.projString"].as<string>(),
                                               vm["interpolate.xAxisValues"].as<string>(), vm["interpolate.yAxisValues"].as<string>(),
                                               vm["interpolate.xAxisUnit"].as<string>(), vm["interpolate.yAxisUnit"].as<string>(),
                                               vm["interpolate.xAxisType"].as<string>(), vm["interpolate.yAxisType"].as<string>());
    } else if (vm.count("interpolate.template")) {
        interpolator->changeProjection(method, vm["interpolate.template"].as<string>());
    } else if (vm.count("interpolate.vcrossNames")) {
        if (!vm.count("interpolate.vcrossNoPoints")) {
            cerr << "ERROR: interpolate.vcrossNames requires vcrossNoPoints, too" << endl;
            exit(1);
        }
        if (!vm.count("interpolate.longitudeValues")) {
            cerr << "ERROR: interpolate.vcrossNames requires longitudeValues, too" << endl;
            exit(1);
        }
        if (!vm.count("interpolate.latitudeValues")) {
            cerr << "ERROR: interpolate.vcrossNames requires latitudeValues, too" << endl;
            exit(1);
        }
        vector<string> vNames = tokenize(vm["interpolate.vcrossNames"].as<string>(), ",");
        vector<size_t> pointNo = tokenizeDotted<size_t>(vm["interpolate.vcrossNoPoints"].as<string>());
        vector<double> latVals = tokenizeDotted<double>(vm["interpolate.latitudeValues"].as<string>());
        vector<double> lonVals = tokenizeDotted<double>(vm["interpolate.longitudeValues"].as<string>());
        if (vNames.size() != pointNo.size()) {
            cerr << "ERROR: interpolate.vcrossNames and vcrossNoPoints need same size: " << vNames.size() << "!=" << pointNo.size() << endl;
            exit(1);
        }
        size_t pointSum = std::accumulate(pointNo.begin(),pointNo.end(),0);
        if (pointSum != latVals.size()) {
            cerr << "ERROR: interpolate.latitudeValues size does not match the sum of vcrossNoPoints: " << latVals.size() << "!=" << pointSum << endl;
            exit(1);
        }
        if (latVals.size() != lonVals.size()) {
            cerr << "ERROR: interpolate.latitudeValues size does not match longitudeVals size: " << latVals.size() << "!=" << lonVals.size() << endl;
            exit(1);
        }
        vector<CrossSectionDefinition> csd;
        size_t llPos = 0;
        for (size_t i = 0; i < vNames.size(); i++) {
            vector<pair<double, double> > lonLatVals;
            size_t points = pointNo.at(i);
            for (size_t j = 0; j < points; ++j) {
                lonLatVals.push_back(make_pair(lonVals.at(llPos+j), latVals.at(llPos+j)));
            }
            csd.push_back(CrossSectionDefinition(vNames.at(i), lonLatVals));
            llPos += points;
        }
        interpolator->changeProjectionToCrossSections(method, csd);
    } else if (vm.count("interpolate.latitudeValues")) {
        if (!vm.count("interpolate.longitudeValues")) {
            cerr << "ERROR: interpolate.latitudeValues requires longitudeValues, too" << endl;
            exit(1);
        }
        vector<double> latVals = tokenizeDotted<double>(vm["interpolate.latitudeValues"].as<string>());
        vector<double> lonVals = tokenizeDotted<double>(vm["interpolate.longitudeValues"].as<string>());
        interpolator->changeProjection(method, lonVals, latVals);
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "interpolate.projString, interpolate.template or interpolate.latitudeValues not found, no interpolation used");
        return dataReader;
    }


    printReaderStatements("interpolate", vm, interpolator);

    return boost::shared_ptr<CDMReader>(interpolator);
}

static boost::shared_ptr<CDMReader> getCDMMerger(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {

    if (not (vm.count("merge.inner.file") or vm.count("merge.inner.type") or vm.count("merge.inner.config")
                    or vm.count("merge.smoothing") or vm.count("merge.method")))
        return dataReader;

    boost::shared_ptr<CDMReader> readerI = getCDMFileReader(vm, "merge.inner");
    if( not readerI )
        throw CDMException("could not create reader for inner in merge");

    boost::shared_ptr<CDMMerger> merger = boost::shared_ptr<CDMMerger>(new CDMMerger(readerI, dataReader));

    if( vm.count("merge.smoothing") ) {
        const string& v = vm["merge.smoothing"].as<string>();
        if (boost::starts_with(v, "LINEAR")) {
            boost::smatch what;
            if (boost::regex_match(v, what, boost::regex("^LINEAR\\(([^,]+),([^,]+)\\)$"))) {
                try {
                    int transition = boost::lexical_cast<int>(what[1]);
                    int border     = boost::lexical_cast<int>(what[2]);
                    merger->setSmoothing(CDMBorderSmoothing::SmoothingFactoryPtr(new CDMBorderSmoothing_LinearFactory(transition, border)));
                } catch (boost::bad_lexical_cast&) {
                    throw CDMException("problem parsing parameters for linear smoothing: " + vm["merge.smoothing"].as<string>());
                }
            } else {
                throw CDMException("malformed linear smoothing specification: " + vm["merge.smoothing"].as<string>());
            }
        } else {
            throw CDMException("unknown smoothing: " + vm["merge.smoothing"].as<string>());
        }
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "no merge.smoothing given, using default as defined by CDMMerger");
    }

    int method = getInterpolationMethod(vm, "merge.method");
    merger->setGridInterpolationMethod(method);

    if (vm.count("merge.projString")) {
        if (not (vm.count("merge.xAxisUnit") && vm.count("merge.yAxisUnit")))
            throw CDMException("merge.xAxisUnit and merge.yAxisUnit required");

        if (not (vm.count("merge.xAxisValues") && vm.count("merge.yAxisValues")))
            throw CDMException("merge.xAxisValues and merge.yAxisValues required");

        merger->setTargetGrid(vm["merge.projString"].as<string>(),
                vm["merge.xAxisValues"].as<string>(), vm["merge.yAxisValues"].as<string>(),
                vm["merge.xAxisUnit"].as<string>(), vm["merge.yAxisUnit"].as<string>(),
                vm["merge.xAxisType"].as<string>(), vm["merge.yAxisType"].as<string>());
    } else {
        merger->setTargetGridFromInner();
    }

    printReaderStatements("merge", vm, merger);
    return merger;
}

static boost::shared_ptr<CDMReader> getNcmlCDMReader(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    if (! vm.count("ncml.config")) {
        return dataReader;
    }
    boost::shared_ptr<NcmlCDMReader> ncmlReader(new NcmlCDMReader(boost::shared_ptr<CDMReader>(dataReader),XMLInputFile(vm["ncml.config"].as<string>())));
    printReaderStatements("ncml", vm, ncmlReader);

    return boost::shared_ptr<CDMReader>(ncmlReader);
}

static void fillWriteCDM(boost::shared_ptr<CDMReader> dataReader, po::variables_map& vm) {
    if (!(vm.count("output.fillFile"))) {
        return;
    }
    string type = getType("output", vm);
    // boost::shared_ptr to shared_ptr
    boost::shared_ptr<CDMReader> sharedDataReader(dataReader);
#ifdef HAVE_NETCDF_H
    if (type == "nc" || type == "cdf" || type == "netcdf" || type == "nc4") {
        LOG4FIMEX(logger, Logger::DEBUG, "filling NetCDF-file " << vm["output.fillFile"].as<string>() << " without config");
        boost::shared_ptr<CDMReaderWriter> readerWriter = boost::shared_ptr<CDMReaderWriter>(new NetCDF_CDMReader(vm["output.fillFile"].as<string>(), true));
        FillWriter(sharedDataReader, readerWriter);
        return;
    }
#endif
    LOG4FIMEX(logger, Logger::ERROR, "output.fillFile with type " << type << " not possible");
    return;
}


static void writeCDM(boost::shared_ptr<CDMReader> dataReader, po::variables_map& vm) {
    printReaderStatements("output", vm, dataReader);
    if (!vm.count("output.file")) {
        LOG4FIMEX(logger, Logger::DEBUG, "no output.file selected");
        return;
    }
    string type = getType("output", vm);
    // boost::shared_ptr to shared_ptr
    boost::shared_ptr<CDMReader> sharedDataReader(dataReader);
#ifdef HAVE_NETCDF_H
    if (type == "nc" || type == "cdf" || type == "netcdf" || type == "nc4") {
        int version = 3;
        if (type == "nc4") version = 4;
        if (vm.count("output.config")) {
            LOG4FIMEX(logger, Logger::DEBUG, "writing NetCDF-file " << vm["output.file"].as<string>() << " with config " << vm["output.config"].as<string>());
            NetCDF_CDMWriter(sharedDataReader, vm["output.file"].as<string>(), vm["output.config"].as<string>(), version);
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "writing NetCDF-file " << vm["output.file"].as<string>() << " without config");
            NetCDF_CDMWriter(sharedDataReader, vm["output.file"].as<string>(), "", version);
        }
        return;
    }
#endif
#ifdef HAVE_GRIB_API_H
    if (type == "grb" || type == "grib" ||
            type == "grb1" || type == "grib1" ||
            type == "grb2" || type == "grib2") {
        int gribVersion = 1;
        if (type == "grb2" || type == "grib2") {
            gribVersion = 2;
        }
        LOG4FIMEX(logger, Logger::DEBUG, "writing grib-file " << vm["output.file"].as<string>() << " with config " << vm["output.config"].as<string>());
        if (!vm.count("output.config")) throw CDMException("Cannot write grib-file without config");
        GribApiCDMWriter(sharedDataReader, vm["output.file"].as<string>(), gribVersion, vm["output.config"].as<string>());
        return;
    }
#endif
#ifdef HAVE_METGM_H
    if (type == "metgm") {
        LOG4FIMEX(logger, Logger::DEBUG, "writing metgm-file " << vm["output.file"].as<string>() << " with config " << vm["output.config"].as<string>());
        if (!vm.count("output.config")) throw CDMException("Cannot write metgm-file without config");
        MetGmCDMWriter(sharedDataReader, vm["output.file"].as<string>(), vm["output.config"].as<string>());
        return;
    }
#endif

    if (type == "null") {
        LOG4FIMEX(logger, Logger::DEBUG, "emulating writing without file without config");
        Null_CDMWriter(sharedDataReader, vm["output.file"].as<string>());
        return;
    }
    cerr << "unable to write type: " << type << endl;
    exit(1);
}


int run(int argc, char* args[])
{
    // Declare the supported options.
    po::options_description generic("Generic options");
    std::string configFile("fimex.cfg");
    int num_threads = 1;
    generic.add_options()
        ("help,h", "help message")
        ("version", "program version")
        ("debug", "debug program")
        ("log4cpp", po::value<string>(), "log4cpp property file (- = log4cpp without prop-file)")
        ("print-options", "print all options")
        ("config,c", po::value<string>(&configFile)->default_value(configFile), "configuration file")
        ("num_threads,n", po::value<int>(&num_threads)->default_value(num_threads), "number of threads")
        ;

    // Declare a group of options that will be
    // allowed both on command line and in
    // config file
    po::options_description config("Configurational options");
    config.add_options()
        ("input.file", po::value<string>(), "input file")
        ("input.type", po::value<string>(), "filetype of input file, e.g. nc, nc4, ncml, felt, grib1, grib2, wdb")
        ("input.config", po::value<string>(), "non-standard input configuration")
        ("input.optional", po::value<vector<string> >()->composing(), "additional options, e.g. multiple files for grib")
#if BOOST_VERSION >= 104000
        ("input.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of input")
#else
        ("input.printNcML", po::value<string>(), "print NcML description of input (use - for command-line")
#endif
        ("input.printCS", "print CoordinateSystems of input file")
        ("input.printSize", "print size estimate")
        ("output.file", po::value<string>(), "output file")
        ("output.fillFile", po::value<string>(), "existing output file to be filled")
        ("output.type", po::value<string>(), "filetype of output file, e.g. nc, nc4, grib1, grib2")
        ("output.config", po::value<string>(), "non-standard output configuration")
#if BOOST_VERSION >= 104000
        ("output.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of input")
#else
        ("output.printNcML", po::value<string>(), "print NcML description of input (use - for command-line")
#endif
        ("output.printCS", "print CoordinateSystems of input file")
        ("output.printSize", "print size estimate")
        ("process.accumulateVariable", po::value<vector<string> >()->composing(), "accumulate variable along unlimited dimension")
        ("process.deaccumulateVariable", po::value<vector<string> >()->composing(), "deaccumulate variable along unlimited dimension")
        ("process.rotateVectorToLatLonX", po::value<vector<string> >()->composing(), "deprecated: rotate this vector x component from grid-direction to latlon direction")
        ("process.rotateVectorToLatLonY", po::value<vector<string> >()->composing(), "deprecated: rotate this vector y component from grid-direction to latlon direction")
        ("process.rotateVector.direction", po::value<string>(), "set direction: to latlon or grid")
        ("process.rotateVector.angle", po::value<vector<string> >()->composing(), "rotate these angles (in degree) to the direction ")
        ("process.rotateVector.x", po::value<vector<string> >()->composing(), "rotate this vector x component to direction")
        ("process.rotateVector.stdNameX", po::value<vector<string> >()->composing(), "new standard_name for the rotated vector")
        ("process.rotateVector.y", po::value<vector<string> >()->composing(), "rotate this vector y component from grid-direction to latlon direction")
        ("process.rotateVector.stdNameY", po::value<vector<string> >()->composing(), "new standard_name for the rotated vector")
        ("process.rotateVector.all", "rotate all known vectors (e.g. standard_name) to given direction")
        ("process.addVerticalVelocity", "calculate upward_air_velocity_ml")
#if BOOST_VERSION >= 104000
        ("process.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of process")
#else
        ("process.printNcML", po::value<string>(), "print NcML description of process (use - for command-line")
#endif
        ("process.printCS", "print CoordinateSystems of process")
        ("process.printSize", "print size estimate")
        ("extract.removeVariable", po::value<vector<string> >()->composing(), "remove variables")
        ("extract.selectVariables", po::value<vector<string> >()->composing(), "select only those variables")
        ("extract.selectVariables.noAuxiliary", "don't add auxiliary variables")
        ("extract.reduceDimension.name", po::value<vector<string> >()->composing(), "name of a dimension to reduce")
        ("extract.reduceDimension.start", po::value<vector<int> >()->composing(), "start position of the dimension to reduce (>=0)")
        ("extract.reduceDimension.end", po::value<vector<int> >()->composing(), "end position of the dimension to reduce")
        ("extract.reduceTime.start", po::value<string>(), "start-time as iso-string")
        ("extract.reduceTime.end", po::value<string>(), "end-time by iso-string")
        ("extract.reduceVerticalAxis.unit", po::value<string>(), "unit of vertical axis to reduce")
        ("extract.reduceVerticalAxis.start", po::value<double>(), "start value of vertical axis")
        ("extract.reduceVerticalAxis.end", po::value<double>(), "end value of the vertical axis")
        ("extract.reduceToBoundingBox.south", po::value<double>(), "geographical bounding-box in degree")
        ("extract.reduceToBoundingBox.north", po::value<double>(), "geographical bounding-box in degree")
        ("extract.reduceToBoundingBox.east", po::value<double>(), "geographical bounding-box in degree")
        ("extract.reduceToBoundingBox.west", po::value<double>(), "geographical bounding-box in degree")
#if BOOST_VERSION >= 104000
        ("extract.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("extract.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("extract.printCS", "print CoordinateSystems of extractor")
        ("extract.printSize", "print size estimate")
        ("qualityExtract.autoConfString", po::value<string>(), "configure the quality-assignment using CF-1.3 status-flag")
        ("qualityExtract.config", po::value<string>(), "configure the quality-assignment with a xml-config file")
#if BOOST_VERSION >= 104000
        ("qualityExtract.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("qualityExtract.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("qualityExtract.printCS", "print CoordinateSystems of extractor")
        ("qualityExtract.printSize", "print size estimate")
        ("interpolate.projString", po::value<string>(), "proj4 input string describing the new projection")
        ("interpolate.method", po::value<string>(), "interpolation method, one of nearestneighbor, bilinear, bicubic, coord_nearestneighbor, coord_kdtree, forward_max, forward_mean, forward_median or forward_sum")
        ("interpolate.xAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("interpolate.yAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("interpolate.xAxisUnit", po::value<string>(), "unit of x-Axis given as udunits string, i.e. m or degrees_east")
        ("interpolate.yAxisUnit", po::value<string>(), "unit of y-Axis given as udunits string, i.e. m or degrees_north")
        ("interpolate.xAxisType", po::value<string>()->default_value("double"), "datatype of x-axis (double,float,int,short)")
        ("interpolate.yAxisType", po::value<string>()->default_value("double"), "datatype of y-axis")
        ("interpolate.distanceOfInterest", po::value<double>(), "optional distance of interest used differently depending on method")
        ("interpolate.latitudeName", po::value<string>(), "name for auto-generated projection coordinate latitude")
        ("interpolate.longitudeName", po::value<string>(), "name for auto-generated projection coordinate longitude")
        ("interpolate.preprocess", po::value<string>(), "add a 2d preprocess to before the interpolation, e.g. \"fill2d(critx=0.01,cor=1.6,maxLoop=100)\" or \"creepfill2d(repeat=20,weight=2[,defaultValue=0.0])\"")
        ("interpolate.latitudeValues", po::value<string>(), "string with latitude values in degree, i.e. 60.5,70,90")
        ("interpolate.longitudeValues", po::value<string>(), "string with longitude values in degree, i.e. -10.5,-10.5,29.5")
        ("interpolate.vcrossNames", po::value<string>(), "string with comma-separated names for vertical cross sections")
        ("interpolate.vcrossNoPoints", po::value<string>(), "string with comma-separated number of lat/lon values for each vertical cross sections")
        ("interpolate.template", po::value<string>(), "netcdf file containing lat/lon list used in interpolation")
#if BOOST_VERSION >= 104000
        ("interpolate.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("interpolate.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("interpolate.printCS", "print CoordinateSystems of interpolator")
        ("interpolate.printSize", "print size estimate")

        ("merge.inner.file", po::value<string>(), "inner file for merge")
        ("merge.inner.type", po::value<string>(), "filetype of inner merge file, e.g. nc, nc4, ncml, felt, grib1, grib2, wdb")
        ("merge.inner.config", po::value<string>(), "non-standard configuration for inner merge file")
        ("merge.smoothing", po::value<string>(), "smoothing function for merge, e.g. \"LINEAR(5,2)\" for linear smoothing, 5 grid points transition, 2 grid points border")
        ("merge.method", po::value<string>(), "interpolation method for grid conversions, one of nearestneighbor, bilinear, bicubic,"
                " coord_nearestneighbor, coord_kdtree, forward_max, forward_mean, forward_median or forward_sum")
        ("merge.projString", po::value<string>(), "proj4 input string describing the new projection")
        ("merge.xAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("merge.yAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("merge.xAxisUnit", po::value<string>(), "unit of x-Axis given as udunits string, i.e. m or degrees_east")
        ("merge.yAxisUnit", po::value<string>(), "unit of y-Axis given as udunits string, i.e. m or degrees_north")
        ("merge.xAxisType", po::value<string>()->default_value("double"), "datatype of x-axis (double,float,int,short)")
        ("merge.yAxisType", po::value<string>()->default_value("double"), "datatype of y-axis")
#if BOOST_VERSION >= 104000
        ("merge.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("merge.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("merge.printCS", "print CoordinateSystems of interpolator")
        ("merge.printSize", "print size estimate")

        ("verticalInterpolate.type", po::value<string>(), "pressure, height or depth")
        ("verticalInterpolate.method", po::value<string>(), "linear, log, loglog or nearestneighbor interpolation")
        ("verticalInterpolate.level1", po::value<string>(), "specification of first level, see Fimex::CDMVerticalInterpolator for a full definition")
        ("verticalInterpolate.level2", po::value<string>(), "specification of second level, only required for hybrid levels, see Fimex::CDMVerticalInterpolator for a full definition")
        ("verticalInterpolate.dataConversion", po::value<vector<string> >()->composing(), "vertical data-conversion: theta2T, omega2vwind or add4Dpressure")
#if BOOST_VERSION >= 104000
        ("verticalInterpolate.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("verticalInterpolate.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("verticalInterpolate.printCS", "print CoordinateSystems of vertical interpolator")
        ("verticalInterpolate.printSize", "print size estimate")
        ("timeInterpolate.timeSpec", po::value<string>(), "specification of times to interpolate to, see MetNoFimex::TimeSpec for a full definition")
#if BOOST_VERSION >= 104000
        ("timeInterpolate.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("timeInterpolate.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("timeInterpolate.printCS", "print CoordinateSystems of timeInterpolator")
        ("timeInterpolate.printSize", "print size estimate")
        ("qualityExtract2.autoConfString", po::value<string>(), "configure the quality-assignment using CF-1.3 status-flag")
        ("qualityExtract2.config", po::value<string>(), "configure the quality-assignment with a xml-config file")
#if BOOST_VERSION >= 104000
        ("qualityExtract2.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("qualityExtract2.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("qualityExtract2.printCS", "print CoordinateSystems of extractor")
        ("qualityExtract2.printSize", "print size estimate")
        ("ncml.config", po::value<string>(), "modify/configure with ncml-file")
#if BOOST_VERSION >= 104000
        ("ncml.printNcML", po::value<string>()->implicit_value("-"), "print NcML description of extractor")
#else
        ("ncml.printNcML", po::value<string>(), "print NcML description of extractor (use - for command-line")
#endif
        ("ncml.printCS", "print CoordinateSystems after ncml-configuration")
        ("ncml.printSize", "print size estimate")
        ;


    po::options_description cmdline_options;
    cmdline_options.add(generic).add(config);

    po::options_description config_file_options;
    config_file_options.add(config);

    po::positional_options_description p;
    p.add("input.file", 1);
    p.add("output.file", 1);

    // read first only cmd-line to get the configFile right
    po::variables_map genVm;
    po::store(po::command_line_parser(argc, args).options(cmdline_options).run(), genVm);
    po::notify(genVm);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(cmdline_options).positional(p).run(), vm);
    ifstream ifs(configFile.c_str());
    po::store(po::parse_config_file(ifs, config_file_options), vm);
    po::notify(vm);
    if (argc == 1 || vm.count("help")) {
        writeUsage(cout, generic, config);
        return 0;
    }
    if (vm.count("version")) {
        cout << "fimex version " << fimexVersion() <<" (" << mifi_version_major() << "." << mifi_version_minor() << "." << mifi_version_patch() << "." << mifi_version_status() << ")" << endl;
        return 0;
    }

    defaultLogLevel(Logger::WARN);
    if (vm.count("log4cpp")) {
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
    if (vm.count("debug") >= 1) {
        // TODO allow for multiple occurances and use INFO as == 1
        defaultLogLevel(Logger::DEBUG);
    } else if (vm.count("debug") > 1) {
        defaultLogLevel(Logger::DEBUG);
    }
    mifi_setNumThreads(num_threads);
    if (vm.count("print-options")) {
        writeOptions(cout, vm);
    }
    if (vm.count("debug") && !vm.count("print-options")) {
        writeOptions(cerr, vm);
    }
    if (!(vm.count("input.file"))) {
        writeUsage(cerr, generic, config);
        LOG4FIMEX(logger, Logger::FATAL, "input.file required");
        return 1;
    }

    boost::shared_ptr<CDMReader> dataReader = getCDMFileReader(vm);
    dataReader = getCDMProcessor(vm, dataReader);
    dataReader = getCDMQualityExtractor("", vm, dataReader);
    dataReader = getCDMExtractor(vm, dataReader);
    dataReader = getCDMTimeInterpolator(vm, dataReader);
    dataReader = getCDMInterpolator(vm, dataReader);
    dataReader = getCDMVerticalInterpolator(vm, dataReader);
    dataReader = getCDMMerger(vm, dataReader);
    dataReader = getCDMQualityExtractor("2", vm, dataReader);
    dataReader = getNcmlCDMReader(vm, dataReader);
    fillWriteCDM(dataReader, vm);
    writeCDM(dataReader, vm);

    return 0;
}

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
    } catch (const boost::program_options::error& ex) {
        clog << "invalid options: " << ex.what() << endl;
        return 1;
    } catch (exception& ex) {
        clog << "exception occured: " << ex.what() << endl;
        return 1;
    }
#endif
}

