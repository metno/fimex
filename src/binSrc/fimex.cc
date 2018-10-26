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

#include "fimex_config.h"
#include <iostream>
#include <fstream>
#include <cctype>
#include <numeric>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/version.hpp>
#include <boost/make_shared.hpp>
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
#ifdef HAVE_LOG4CPP
#include "log4cpp/PropertyConfigurator.hh"
#endif
#ifdef HAVE_MPI
#include "fimex/mifi_mpi.h"
#endif

namespace po = boost::program_options;
using namespace std;
using namespace MetNoFimex;

namespace {

LoggerPtr logger = getLogger("fimex");
po::options_description config_file_options;
CDMReader_p applyFimexStreamTasks(const po::variables_map& vm, CDMReader_p dataReader);

typedef boost::shared_ptr<CDMInterpolator> CDMInterpolator_p;

void writeUsage(ostream& out, const po::options_description& generic, const po::options_description& config)
{
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

template<typename T>
void writeOption(ostream& out, const string& var, const po::variables_map& vm)
{
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

void writeOptionAny(ostream& out, const string& var, const po::variables_map& vm)
{
    // variables without real value, just set or unset
    if (vm.count(var)) {
        out << var  << endl;
    }
}

void writeVectorOptionString(ostream& out, const string& var, const po::variables_map& vm)
{
    if (vm.count(var)) {
        vector<string> vals = vm[var].as<vector<string> >();
        typedef vector<string>::iterator VIT;
        for (VIT it = vals.begin(); it != vals.end(); ++it) {
            out << var << ": " << *it << endl;
        }
    }
}

void writeVectorOptionInt(ostream& out, const string& var, const po::variables_map& vm)
{
    if (vm.count(var)) {
        vector<int> vals = vm[var].as<vector<int> >();
        typedef vector<int>::iterator VIT;
        for (VIT it = vals.begin(); it != vals.end(); ++it) {
            out << var << ": " << *it << endl;
        }
    }
}

void writeOptions(ostream& out, const po::variables_map& vm)
{
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
    writeVectorOptionString(out, "extract.pickDimension.name", vm);
    writeVectorOptionString(out, "extract.pickDimension.list", vm);
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
    writeOption<string>(out, "interpolate.postprocess", vm);
    writeOption<string>(out, "interpolate.printNcML", vm);
    writeOptionAny(out, "interpolate.printCS", vm);
    writeOptionAny(out, "interpolate.printSize", vm);
    writeOption<string>(out, "verticalInterpolate.method", vm);
    writeOption<bool>(out, "verticalInterpolate.ignoreValidityMin", vm);
    writeOption<bool>(out, "verticalInterpolate.ignoreValidityMax", vm);
    writeOption<string>(out, "verticalInterpolate.type", vm);
    writeOption<string>(out, "verticalInterpolate.templateVar", vm);
    writeOption<string>(out, "verticalInterpolate.level1", vm);
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
    writeOption<string>(out, "merge.inner.cfg", vm);
    writeOption<string>(out, "merge.smoothing", vm);
    writeOptionAny(out, "merge.keepOuterVariables", vm);
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

string getType(const string& io, const po::variables_map& vm)
{
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

std::string getFile(const string& io, const po::variables_map& vm)
{
  return vm[io + ".file"].as<string>();
}

template<typename T>
T getOption(const string& key, const po::variables_map& vm)
{
  if (vm.count(key))
    return vm[key].as<T>();
  else
    return T();
}

template<typename T>
bool getOption(const string& key, const po::variables_map& vm, T& t)
{
  if (!vm.count(key))
    return false;
  t = vm[key].as<T>();
  return true;
}

std::string getString(const string& key, const po::variables_map& vm)
{
  return getOption<string>(key, vm);
}

std::string getConfig(const string& io, const po::variables_map& vm)
{
  return getString(io+".config", vm);
}

void printReaderStatements(const string& readerName, const po::variables_map& vm, CDMReader_p reader)
{
    string ncmlout;
    if (getOption(readerName+".printNcML", vm, ncmlout)) {
        cout << readerName << " as NcML:" << endl;
        if (ncmlout == "-") {
            reader->getCDM().toXMLStream(cout);
            cout << endl;
        } else {
            ofstream file;
            file.open(ncmlout.c_str(), ios::out);
            if (file.is_open()) {
                reader->getCDM().toXMLStream(file);
                file.close();
            } else {
                throw CDMException("cannot write ncml-file: '" + ncmlout + "'");
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

const string FELT_VARIABLES = (string(PKGDATADIR) + "/felt2nc_variables.xml");

CDMReader_p getCDMFileReader(const po::variables_map& vm, const string& io="input")
{
    const string type = getType(io, vm);
    const std::string fileName = getFile(io, vm);
    string config = getConfig(io, vm);
    std::vector<std::string> optional;

    int filetype = MIFI_FILETYPE_UNKNOWN;
    if (isFeltType(type)) {
      filetype = MIFI_FILETYPE_FELT;
      if (config.empty())
        config = FELT_VARIABLES;
    } else if (isGribType(type)) {
      filetype = MIFI_FILETYPE_GRIB;
      optional = getOption< vector<string> >(io+".optional", vm);
    } else if (isNetCDFType(type)) {
      filetype = MIFI_FILETYPE_NETCDF;
    } else {
      filetype = mifi_get_filetype(type.c_str());
    }

    LOG4FIMEX(logger, Logger::DEBUG, "reading file of type '" << mifi_get_filetype_name(filetype)
              << "' file '" << fileName << "' with config '" << config << "'");
    if (CDMReader_p reader = CDMFileReaderFactory::create(filetype, fileName, config, optional)) {
      printReaderStatements(io, vm, reader);
      return reader;
    }

    LOG4FIMEX(logger, Logger::FATAL, "unable to read type: " << type);
    exit(1);
}

int getInterpolationMethod(const po::variables_map& vm, const string& key)
{
    int method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
    if (vm.count(key)) {
        const string& m = vm[key].as<string>();
        method = mifi_string_to_interpolation_method(m.c_str());
        if (method == MIFI_INTERPOL_UNKNOWN) {
            LOG4FIMEX(logger, Logger::WARN, "unknown " << key << ": " << m << " using nearestneighbor");
            method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
        }
    }
    return method;
}

CDMReader_p getCDMProcessor(const po::variables_map& vm, CDMReader_p dataReader)
{
    if (! (vm.count("process.accumulateVariable") || vm.count("process.deaccumulateVariable") ||
            vm.count("process.rotateVectorToLatLonX") || vm.count("process.rotateVector.direction") ||
            vm.count("process.addVerticalVelocity")))
    {
        LOG4FIMEX(logger, Logger::DEBUG, "process.[de]accumulateVariable or rotateVector.direction or addVerticalVelocity not found, no process used");
        return dataReader;
    }
    boost::shared_ptr<CDMProcessor> processor(new CDMProcessor(dataReader));
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
        const string direction = vm["process.rotateVector.direction"].as<string>();
        if (direction == "latlon") {
            toLatLon = true;
        } else if (direction == "grid") {
            toLatLon = false;
        } else {
            LOG4FIMEX(logger, Logger::FATAL, "process.rotateVector.direction != 'latlon' or 'grid' : " << direction << " invalid");
            exit(1);
        }
        if (vm.count("process.rotateVector.x") && vm.count("process.rotateVector.y")) {
            vector<string> xvars = vm["process.rotateVector.x"].as<vector<string> >();
            vector<string> yvars = vm["process.rotateVector.y"].as<vector<string> >();
            vector<string> stdX = getOption< vector<string> >("process.rotateVector.stdNameX", vm);
            vector<string> stdY = getOption< vector<string> >("process.rotateVector.stdNameY", vm);
            processor->rotateVectorToLatLon(toLatLon, xvars, yvars, stdX, stdY);
        } else if (vm.count("process.rotateVector.all")) {
            processor->rotateAllVectorsToLatLon(toLatLon);
        } else if (vm.count("process.rotateVector.angle")) {
            // do nothing here, but don't abort either (see below)
        } else {
            LOG4FIMEX(logger, Logger::FATAL, "process.rotateVector.x and process.rotateVector.y, or process.rotateVector.angle not found");
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

CDMReader_p getCDMExtractor(const po::variables_map& vm, CDMReader_p dataReader)
{
    if (! (vm.count("extract.reduceDimension.name") || vm.count("extract.pickDimension.name") || vm.count("extract.removeVariable") ||
           vm.count("extract.selectVariables") || vm.count("extract.reduceTime.start") ||
           vm.count("extract.reduceTime.start") || vm.count("extract.reduceVerticalAxis.unit") ||
           vm.count("extract.reduceToBoundingBox.south") || vm.count("extract.reduceToBoundingBox.north") ||
           vm.count("extract.reduceToBoundingBox.west") || vm.count("extract.reduceToBoundingBox.east")))
    {
        LOG4FIMEX(logger, Logger::DEBUG, "extract.reduceDimension.name and extract.removeVariable not found, no extraction used");
        return dataReader;
    }
    boost::shared_ptr<CDMExtractor> extractor(new CDMExtractor(dataReader));
    if (vm.count("extract.reduceDimension.name")) {
        vector<string> vars = vm["extract.reduceDimension.name"].as<vector<string> >();
        vector<int> startPos = getOption< vector<int> >("extract.reduceDimension.start", vm);
        vector<int> endPos = getOption< vector<int> >("extract.reduceDimension.end", vm);
        if (startPos.size() != vars.size()) {
            LOG4FIMEX(logger, Logger::ERROR, "extract.reduceDimension.start has different number of elements than extract.reduceDimension.name; "
                                             "use start = 0 if you don't want to reduce the start-position");
        }
        if (endPos.size() != vars.size()) {
            LOG4FIMEX(logger, Logger::ERROR, "extract.reduceDimension.end has different number of elements than extract.reduceDimension.name; "
                                             "use end = 0 (with start != 0) if you don't want to reduce the end-position");
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
    if (vm.count("extract.pickDimension.name")) {
        vector<string> dims = vm["extract.pickDimension.name"].as<vector<string> >();
        vector<string> lists = getOption< vector<string> >("extract.pickDimension.list", vm);
        if (dims.size() != lists.size()) {
            LOG4FIMEX(logger, Logger::ERROR, "extract.pickDimension.name has different number of elements than extract.pickDimension.list");
        }
        for (size_t i = 0; i < dims.size(); ++i) {
            vector<int> pos = tokenizeDotted<int>(lists.at(i)); // tokenizeDotted does not work with unsigned values
            set<size_t> posSet(pos.begin(), pos.end());
            extractor->reduceDimension(dims.at(i), posSet);
        }
    }
    if (vm.count("extract.reduceTime.start") || vm.count("extract.reduceTime.end")) {
        FimexTime start(FimexTime::min_date_time);
        if (vm.count("extract.reduceTime.start")) {
            const string tStart = vm["extract.reduceTime.start"].as<string>();
            if (!start.parseISO8601(tStart)) {
                LOG4FIMEX(logger, Logger::FATAL, "cannot parse time '" << tStart << "'");
                exit(1);
            }
        }
        FimexTime end(FimexTime::max_date_time);
        if (vm.count("extract.reduceTime.end")) {
            const string tEnd = vm["extract.reduceTime.end"].as<string>();
            if (! end.parseISO8601(tEnd) ) {
                LOG4FIMEX(logger, Logger::FATAL, "cannot parse time '" << tEnd << "'");
                exit(1);
            }
        }
        extractor->reduceTime(start, end);
    }
    if (vm.count("extract.reduceVerticalAxis.unit")) {
        if (!(vm.count("extract.reduceVerticalAxis.start") && vm.count("extract.reduceVerticalAxis.end"))) {
            LOG4FIMEX(logger, Logger::FATAL, "extract.reduceVerticalAxis requires all 'start','end','unit'");
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
                LOG4FIMEX(logger, Logger::FATAL, "extract.reduceToBoundingBox." << bb[i] << " missing");
                exit(1);
            }
            bbVals[i] = vm["extract.reduceToBoundingBox."+bb[i]].as<double>();
        }
        LOG4FIMEX(logger, Logger::DEBUG, "reduceLatLonBoudingBox(" << join(&bbVals[0], &bbVals[0]+4, ",")<<")");
        extractor->reduceLatLonBoundingBox(bbVals[0], bbVals[1], bbVals[2], bbVals[3]);
    }
    if (vm.count("extract.selectVariables")) {
        vector<string> vars = vm["extract.selectVariables"].as<vector<string> >();
        bool addAuxiliaryVariables = !vm.count("extract.selectVariables.noAuxiliary");
        extractor->selectVariables(set<string>(vars.begin(), vars.end()), addAuxiliaryVariables);
    }

    vector<string> vars = getOption< vector<string> >("extract.removeVariable", vm);
    for (vector<string>::iterator it = vars.begin(); it != vars.end(); ++it) {
      extractor->removeVariable(*it);
    }
    printReaderStatements("extract", vm, extractor);

    return extractor;
}

CDMReader_p getCDMQualityExtractor(const string& version, const po::variables_map& vm, CDMReader_p dataReader)
{
    const string io = "qualityExtract"+version, autoConfKey = io +".autoConfigString";
    string autoConf;
    if (vm.count(autoConfKey))
      autoConf = vm[autoConfKey].as<string>();
    const string config = getConfig(io, vm);
    if (autoConf != "" || config != "") {
        LOG4FIMEX(logger, Logger::DEBUG, "adding CDMQualityExtractor with (" << autoConf << "," << config <<")");
        dataReader = boost::make_shared<CDMQualityExtractor>(dataReader, autoConf, config);
    }
    printReaderStatements(io, vm, dataReader);
    return dataReader;
}

CDMReader_p getCDMTimeInterpolator(const po::variables_map& vm, CDMReader_p dataReader)
{
    string timeSpec;
    if (!getOption("timeInterpolate.timeSpec", vm, timeSpec)) {
        return dataReader;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "timeInterpolate.timeSpec found with spec: " << timeSpec);
    boost::shared_ptr<CDMTimeInterpolator> timeInterpolator(new CDMTimeInterpolator(dataReader));
    timeInterpolator->changeTimeAxis(timeSpec);
    printReaderStatements("timeInterpolate", vm, timeInterpolator);

    return timeInterpolator;
}

CDMReader_p getCDMVerticalInterpolator(const po::variables_map& vm, CDMReader_p dataReader)
{
    vector<string> operations;
    if (getOption("verticalInterpolate.dataConversion", vm, operations)) {
        try {
            dataReader = boost::make_shared<CDMPressureConversions>(dataReader, operations);
        } catch (CDMException& ex) {
            LOG4FIMEX(logger, Logger::FATAL, "invalid verticalInterpolate.dataConversion: " + join(operations.begin(), operations.end(), ",") + " " + ex.what());
            exit(1);
        }
    }
    string vtype, vmethod;
    if (!getOption("verticalInterpolate.type", vm, vtype) || !getOption("verticalInterpolate.method", vm, vmethod)) {
        return dataReader;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "verticalInterpolate found");
    boost::shared_ptr<CDMVerticalInterpolator> verticalReader = boost::make_shared<CDMVerticalInterpolator>(dataReader, vtype, vmethod);
    string template_var;
    if (getOption("verticalInterpolate.templateVar", vm, template_var)) {
        LOG4FIMEX(logger, Logger::DEBUG, "verticalInterpolate to template var");
        verticalReader->interpolateByTemplateVariable(template_var);
    } else {
        string level1_text, level2_text;
        if (!getOption("verticalInterpolate.level1", vm, level1_text)) {
            LOG4FIMEX(logger, Logger::FATAL, "verticalInterpolate needs level1");
            exit(1);
        }
        vector<double> level1 = tokenizeDotted<double>(level1_text,",");
        if (getOption("verticalInterpolate.level2", vm, level2_text)) {
            LOG4FIMEX(logger, Logger::WARN, "verticalInterpolate level2 ignored");
        }
        verticalReader->interpolateToFixed(level1);
    }
    bool ignoreValidityMin;
    if (getOption("verticalInterpolate.ignoreValidityMin", vm, ignoreValidityMin)) {
        verticalReader->ignoreValidityMin(ignoreValidityMin);
    }
    bool ignoreValidityMax;
    if (getOption("verticalInterpolate.ignoreValidityMax", vm, ignoreValidityMax)) {
        verticalReader->ignoreValidityMax(ignoreValidityMax);
    }
    printReaderStatements("verticalInterpolate", vm, verticalReader);
    return verticalReader;
}

boost::shared_ptr<InterpolatorProcess2d> parseProcess(const string& procString, const string& logProcess)
{
    boost::smatch what;
     if (boost::regex_match(procString, what, boost::regex("\\s*fill2d\\(([^,]+),([^,]+),([^)]+)\\).*"))) {
         double critx = string2type<double>(what[1]);
         double cor = string2type<double>(what[2]);
         size_t maxLoop = string2type<size_t>(what[3]);
         LOG4FIMEX(logger, Logger::DEBUG, "running interpolate "<< logProcess <<": fill2d("<<critx<<","<<cor<<","<<maxLoop<<")");
         return boost::make_shared<InterpolatorFill2d>(critx,cor,maxLoop);
     } else if (boost::regex_match(procString, what, boost::regex("\\s*creepfill2d\\((.+)\\).*"))) {
         vector<string> vals = tokenize(what[1], ",");
         if (vals.size() == 2) {
             unsigned short repeat = string2type<unsigned short>(vals.at(0));
             char setWeight = string2type<char>(vals.at(1));
             LOG4FIMEX(logger, Logger::DEBUG, "running interpolate "<< logProcess <<": creepfill2d("<<repeat<<","<<setWeight<<")");
             return boost::make_shared<InterpolatorCreepFill2d>(repeat, setWeight);
         } else if (vals.size() == 3) {
             unsigned short repeat = string2type<unsigned short>(vals.at(0));
             char setWeight = string2type<char>(vals.at(1));
             float defVal = string2type<float>(vals.at(2));
             LOG4FIMEX(logger, Logger::DEBUG, "running interpolate "<< logProcess <<": creepfillval2d("<<repeat<<","<<setWeight<<","<<defVal<<")");
             return boost::make_shared<InterpolatorCreepFillVal2d>(repeat, setWeight,defVal);
         } else {
             throw CDMException("creepfill requires two or three arguments, got " + what[1]);
         }
     }
     throw CDMException("undefined interpolate."+logProcess+": " + procString);
}

CDMInterpolator_p createCDMInterpolator(const po::variables_map& vm, CDMReader_p dataReader)
{
    CDMInterpolator_p interpolator = boost::make_shared<CDMInterpolator>(dataReader);
    string value;
    if (getOption("interpolate.latitudeName", vm, value)) {
        interpolator->setLatitudeName(value);
    }
    if (getOption("interpolate.longitudeName", vm, value)) {
        interpolator->setLongitudeName(value);
    }

    if (getOption("interpolate.preprocess", vm, value)) {
        interpolator->addPreprocess(parseProcess(value, "preprocess"));
    }
    if (getOption("interpolate.postprocess", vm, value)) {
        interpolator->addPostprocess(parseProcess(value, "postprocess"));
    }
    return interpolator;
}

CDMReader_p getCDMInterpolator(const po::variables_map& vm, CDMReader_p dataReader)
{
    CDMInterpolator_p interpolator;
    int method = getInterpolationMethod(vm, "interpolate.method");

    string proj4;
    if (getOption("interpolate.projString", vm, proj4)) {
        string xAxisUnit, yAxisUnit, xAxisValues, yAxisValues;
        if (!(getOption("interpolate.xAxisUnit", vm, xAxisUnit) && getOption("interpolate.yAxisUnit", vm, yAxisUnit))) {
            LOG4FIMEX(logger, Logger::FATAL, "xAxisUnit and yAxisUnit required");
            exit(1);
        }
        if (!(getOption("interpolate.xAxisValues", vm, xAxisValues) && getOption("interpolate.yAxisValues", vm, yAxisValues))) {
            LOG4FIMEX(logger, Logger::FATAL, "xAxisValues and yAxisValues required");
            exit(1);
        }
        interpolator = createCDMInterpolator(vm, dataReader);
        if (vm.count("interpolate.distanceOfInterest")) {
            interpolator->setDistanceOfInterest(vm["interpolate.distanceOfInterest"].as<double>());
        }
        interpolator->changeProjection(method, proj4, xAxisValues, yAxisValues, xAxisUnit, yAxisUnit,
                                       vm["interpolate.xAxisType"].as<string>(), vm["interpolate.yAxisType"].as<string>());
    } else if (vm.count("interpolate.template")) {
        interpolator = createCDMInterpolator(vm, dataReader);
        interpolator->changeProjection(method, vm["interpolate.template"].as<string>());
    } else if (vm.count("interpolate.vcrossNames")) {
        if (!vm.count("interpolate.vcrossNoPoints")) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames requires vcrossNoPoints, too");
            exit(1);
        }
        if (!vm.count("interpolate.longitudeValues")) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames requires longitudeValues, too");
            exit(1);
        }
        if (!vm.count("interpolate.latitudeValues")) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames requires latitudeValues, too");
            exit(1);
        }
        vector<string> vNames = tokenize(vm["interpolate.vcrossNames"].as<string>(), ",");
        vector<size_t> pointNo = tokenizeDotted<size_t>(vm["interpolate.vcrossNoPoints"].as<string>());
        vector<double> latVals = tokenizeDotted<double>(vm["interpolate.latitudeValues"].as<string>());
        vector<double> lonVals = tokenizeDotted<double>(vm["interpolate.longitudeValues"].as<string>());
        if (vNames.size() != pointNo.size()) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames and vcrossNoPoints need same size: " << vNames.size() << "!=" << pointNo.size());
            exit(1);
        }
        size_t pointSum = std::accumulate(pointNo.begin(),pointNo.end(),0);
        if (pointSum != latVals.size()) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.latitudeValues size does not match the sum of vcrossNoPoints: " << latVals.size() << "!=" << pointSum);
            exit(1);
        }
        if (latVals.size() != lonVals.size()) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.latitudeValues size does not match longitudeVals size: " << latVals.size() << "!=" << lonVals.size());
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
        interpolator = createCDMInterpolator(vm, dataReader);
        interpolator->changeProjectionToCrossSections(method, csd);
    } else if (vm.count("interpolate.latitudeValues")) {
        if (!vm.count("interpolate.longitudeValues")) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.latitudeValues requires longitudeValues, too");
            exit(1);
        }
        vector<double> latVals = tokenizeDotted<double>(vm["interpolate.latitudeValues"].as<string>());
        vector<double> lonVals = tokenizeDotted<double>(vm["interpolate.longitudeValues"].as<string>());
        interpolator = createCDMInterpolator(vm, dataReader);
        interpolator->changeProjection(method, lonVals, latVals);
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "interpolate.projString, interpolate.template or interpolate.latitudeValues not found, no interpolation used");
        return dataReader;
    }

    printReaderStatements("interpolate", vm, interpolator);
    return interpolator;
}

CDMReader_p getNcmlCDMReader(const po::variables_map& vm, CDMReader_p dataReader)
{
    const string config = getConfig("ncml", vm);
    if (!config.empty()) {
      dataReader = boost::make_shared<NcmlCDMReader>(dataReader, XMLInputFile(config));
      printReaderStatements("ncml", vm, dataReader);
    }
    return dataReader;
}

CDMReader_p getCDMMerger(const po::variables_map& vm, CDMReader_p dataReader)
{
    if (not (vm.count("merge.inner.file") or vm.count("merge.inner.type") or vm.count("merge.inner.config")
                    or vm.count("merge.smoothing") or vm.count("merge.method")))
        return dataReader;

    CDMReader_p readerI = getCDMFileReader(vm, "merge.inner");
    if( not readerI )
        throw CDMException("could not create reader for inner in merge");
    if (vm.count("merge.inner.cfg")) {
        po::variables_map mvm;
        ifstream ifs(vm["merge.inner.cfg"].as<string>().c_str());
        if (!ifs) {
            LOG4FIMEX(logger, Logger::ERROR, "missing merge.inner.cfg file '" << vm["merge.inner.cfg"].as<string>() << "'");
        } else {
            po::store(po::parse_config_file(ifs, config_file_options), mvm);
            // apply all fimex processing tasks to the merge.inner stream
            readerI = applyFimexStreamTasks(mvm, readerI);
        }
    }

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

    if (vm.count("merge.keepOuterVariables")) {
        merger->setKeepOuterVariables(true);
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

CDMReader_p applyFimexStreamTasks(const po::variables_map& vm, CDMReader_p dataReader)
{
    dataReader = getCDMProcessor(vm, dataReader);
    dataReader = getCDMQualityExtractor("", vm, dataReader);
    dataReader = getCDMExtractor(vm, dataReader);
    dataReader = getCDMTimeInterpolator(vm, dataReader);
    dataReader = getCDMInterpolator(vm, dataReader);
    dataReader = getCDMVerticalInterpolator(vm, dataReader);
    dataReader = getCDMMerger(vm, dataReader);
    dataReader = getCDMQualityExtractor("2", vm, dataReader);
    dataReader = getNcmlCDMReader(vm, dataReader);
    return dataReader;
}

void fillWriteCDM(CDMReader_p dataReader, po::variables_map& vm)
{
    string fillFile;
    if (!getOption("output.fillFile", vm, fillFile)) {
        return;
    }
    string type = getType("output", vm);
#ifdef HAVE_NETCDF_H
    if (isNetCDFType(type)) {
        LOG4FIMEX(logger, Logger::DEBUG, "filling NetCDF-file " << fillFile << " without NetCDF config");
        CDMReaderWriter_p rw = CDMFileReaderFactory::createReaderWriter(MIFI_FILETYPE_NETCDF|MIFI_FILETYPE_RW, fillFile);
        const string config = getConfig("output", vm);
        FillWriter(dataReader, rw, config);
        return;
    }
#endif
    LOG4FIMEX(logger, Logger::ERROR, "output.fillFile with type " << type << " not possible");
}

void writeCDM(CDMReader_p dataReader, const po::variables_map& vm)
{
    printReaderStatements("output", vm, dataReader);
    string fileName;
    if (!getOption("output.file", vm, fileName)) {
        LOG4FIMEX(logger, Logger::DEBUG, "no output.file selected");
        return;
    }
    const string type = getType("output", vm);
    const string config = getConfig("output", vm);
    try {
      createWriter(dataReader, type, fileName, config);
    } catch (CDMException& ex) {
      LOG4FIMEX(logger, Logger::FATAL, "CDMException while writing: " << ex.what());
      exit(1);
    } catch (exception& ex) {
      LOG4FIMEX(logger, Logger::FATAL, "exception while writing: " << ex.what());
      exit(1);
    }
}


int run(int argc, char* args[])
{
    // Declare the supported options.
    po::options_description generic("Generic options");
    int num_threads = 1;
    generic.add_options()
        ("help,h", "help message")
        ("version", "program version")
        ("debug", "debug program")
        ("log4cpp", po::value<string>(), "log4cpp property file (- = log4cpp without prop-file)")
        ("print-options", "print all options")
        ("config,c", po::value<string>(), "configuration file")
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
        ("extract.pickDimension.name", po::value<vector<string> >()->composing(), "name of a dimension to pick levels")
        ("extract.pickDimension.list", po::value<vector<string> >()->composing(), "list of dim-positions (including dots), starting at 0")
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
        ("interpolate.method", po::value<string>(), "interpolation method, one of nearestneighbor, bilinear, bicubic, coord_nearestneighbor, coord_kdtree, forward_max, forward_min, forward_mean, forward_median, forward_sum or forward_undef_*")
        ("interpolate.xAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("interpolate.yAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("interpolate.xAxisUnit", po::value<string>(), "unit of x-Axis given as udunits string, i.e. m or degrees_east")
        ("interpolate.yAxisUnit", po::value<string>(), "unit of y-Axis given as udunits string, i.e. m or degrees_north")
        ("interpolate.xAxisType", po::value<string>()->default_value("double"), "datatype of x-axis (double,float,int,short)")
        ("interpolate.yAxisType", po::value<string>()->default_value("double"), "datatype of y-axis")
        ("interpolate.distanceOfInterest", po::value<double>(), "optional distance of interest used differently depending on method")
        ("interpolate.latitudeName", po::value<string>(), "name for auto-generated projection coordinate latitude")
        ("interpolate.longitudeName", po::value<string>(), "name for auto-generated projection coordinate longitude")
        ("interpolate.preprocess", po::value<string>(), "add a 2d preprocess before the interpolation, e.g. \"fill2d(critx=0.01,cor=1.6,maxLoop=100)\" or \"creepfill2d(repeat=20,weight=2[,defaultValue=0.0])\"")
        ("interpolate.postprocess", po::value<string>(), "add a 2d postprocess after the interpolation, e.g. \"fill2d(critx=0.01,cor=1.6,maxLoop=100)\" or \"creepfill2d(repeat=20,weight=2[,defaultValue=0.0])\"")
        ("interpolate.latitudeValues", po::value<string>(), "latitude values, in degrees north, of a list of points to interpolate to, e.g. 60.5,70,90"
                                                            " (use with 'longitudeValues' -- to produce a grid, use 'projString', 'xAxisValues', 'yAxisValues', ...)")
        ("interpolate.longitudeValues", po::value<string>(), "longitude values, in degrees east, of a list of points to interpolate to, e.g. -10.5,-10.5,29.5"
                                                             " (use with 'latitudeValues' -- to produce a grid, use 'projString', 'xAxisValues', 'yAxisValues', ...)")
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
        ("merge.inner.cfg", po::value<string>(), "recursive fimex.cfg setup-file to enable all fimex-processing steps (i.e. not input and output) to the merge.inner source before merging")
        ("merge.smoothing", po::value<string>(), "smoothing function for merge, e.g. \"LINEAR(5,2)\" for linear smoothing, 5 grid points transition, 2 grid points border")
        ("merge.keepOuterVariables", "keep all outer variables, default: only keep variables existing in inner and outer")
        ("merge.method", po::value<string>(), "interpolation method for grid conversions, one of nearestneighbor, bilinear, bicubic,"
                " coord_nearestneighbor, coord_kdtree, forward_max, forward_min, forward_mean, forward_median, forward_sum or forward_undef_* (*=max,min,mean,median,sum")
        ("merge.projString", po::value<string>(), "proj4 input string describing the new projection, default: use inner projection extended to outer grid")
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

        ("verticalInterpolate.type", po::value<string>(), "pressure, height (above ground) or depth")
        ("verticalInterpolate.ignoreValidityMin", po::value<bool>(), "ignore minimum value from vertical transformation (e.g. ocean surface)")
        ("verticalInterpolate.ignoreValidityMax", po::value<bool>(), "ignore maximum value from vertical transformation (e.g. ocean bathymetry)")
        ("verticalInterpolate.method", po::value<string>(), "linear, linear_weak_extra, linear_no_extra, linear_const_extra, log, loglog or nearestneighbor interpolation")
        ("verticalInterpolate.templateVar", po::value<string>(), "specification template variable for interpolation to fixed or hybrid levels")
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
    if (vm.count("config")) {
        ifstream ifs(vm["config"].as<string>().c_str());
        if (!ifs) {
            cerr << "missing config file '" << vm["config"].as<string>() << "'" << endl;
            return -1;
        }
        po::store(po::parse_config_file(ifs, config_file_options), vm);
    }
    po::notify(vm);
    if (argc == 1 || vm.count("help")) {
        writeUsage(cout, generic, config);
        return 0;
    }
    if (vm.count("version")) {
        cout << "fimex version " << fimexVersion() <<" (" << mifi_version_major() << "." << mifi_version_minor() << "." << mifi_version_patch() << "-" << std::hex << mifi_version_status() << ")" << endl;
        return 0;
    }

    const int opt_debug = vm.count("debug");
    if (opt_debug >= 1) {
        // TODO allow for multiple occurances and use INFO as == 1
        defaultLogLevel(Logger::DEBUG);
    } else {
        defaultLogLevel(Logger::WARN);
    }

    if (vm.count("log4cpp")) {
#ifdef HAVE_LOG4CPP
        Logger::setClass(Logger::LOG4CPP);
        std::string propFile = vm["log4cpp"].as<string>();
        if (propFile != "-") {
            log4cpp::PropertyConfigurator::configure(propFile);
            if (opt_debug)
                LOG4FIMEX(logger, Logger::WARN, "--log4cpp config file overrides loglevel from --debug");
        }
#else
        Logger::setClass(Logger::LOG2STDERR);
        LOG4FIMEX(logger, Logger::WARN, "fimex was compiled without log4cpp");
#endif
    } else {
        Logger::setClass(Logger::LOG2STDERR);
    }

    mifi_setNumThreads(num_threads);
    if (vm.count("print-options")) {
        writeOptions(cout, vm);
    } else if (opt_debug) {
        writeOptions(cerr, vm);
    }
    if (!(vm.count("input.file"))) {
        writeUsage(cerr, generic, config);
        LOG4FIMEX(logger, Logger::FATAL, "input.file required");
        return 1;
    }

    CDMReader_p dataReader = getCDMFileReader(vm);
    dataReader = applyFimexStreamTasks(vm, dataReader);
    fillWriteCDM(dataReader, vm);
    writeCDM(dataReader, vm);

    return 0;
}

#ifdef HAVE_MPI
struct MPI_Init_Finalize {
  MPI_Init_Finalize(int argc, char* args[])
  {
    MPI_Init(&argc, &args);
    mifi_initialize_mpi(MPI_COMM_WORLD, MPI_INFO_NULL);
  }

  ~MPI_Init_Finalize()
  {
    mifi_free_mpi();
    MPI_Finalize();
  }
};
#endif

} // namespace

int main(int argc, char* args[])
{
    int retStatus = 0;

#ifdef HAVE_MPI
    MPI_Init_Finalize mpi(argc, args);
#endif

    // wrapping main-functions in run to catch all exceptions
#ifndef DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN
    try {
#else
#warning Not catching exceptions in main method
#endif

        retStatus = run(argc, args);

#ifndef DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN
    } catch (const boost::program_options::error& ex) {
        clog << "invalid options: " << ex.what() << endl;
        retStatus = 1;
    } catch (exception& ex) {
        clog << "exception occured: " << ex.what() << endl;
        retStatus = 1;
    }
#endif

    return retStatus;
}
