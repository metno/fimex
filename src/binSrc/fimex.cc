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

#include <iostream>
#include <fstream>
#include <cctype>
#include <boost/program_options.hpp>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>
#include <boost/shared_ptr.hpp>
#include "../../config.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/CDMExtractor.h"
#include "fimex/CDMQualityExtractor.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/Logger.h"
#include "fimex/TimeUnit.h"
#include "fimex/Utils.h"
/* FeltCDMReader not in factory - should be removed? */
#ifdef HAVE_LIBMIC
#include "fimex/FeltCDMReader.h"
#endif
#include "fimex/CDMconstants.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/NcmlCDMReader.h"
/* currently no factory for writer */
#ifdef MIFI_HAVE_NETCDF
#include "fimex/NetCDF_CDMWriter.h"
#endif
#ifdef HAVE_GRIBAPI_H
#include "fimex/GribApiCDMWriter.h"
#endif

namespace po = boost::program_options;
using namespace std;
using namespace MetNoFimex;

static LoggerPtr logger = getLogger("fimex");

static void writeUsage(ostream& out, const po::options_description& generic, const po::options_description& config) {
    out << "usage: fimex --input.file  FILENAME [--input.type  INPUT_TYPE]" << endl;
    out << "             [--output.file FILENAME [--output.type OUTPUT_TYPE]]" << endl;
    out << "             [--input.config CFGFILENAME] [--output.config CFGFILENAME]" << endl;
    out << "             [--extract....]" << endl;
    out << "             [--qualityExtract....]" << endl;
    out << "             [--interpolate....]" << endl;
    out << "             [--timeInterpolate....]" << endl;
    out << "             [--ncml.config NCMLFILE]" << endl;
    out << endl;
    out << generic << endl;
    out << config << endl;
}

static void printReaderStatements(const string& readerName, const po::variables_map& vm, const CDMReader* reader)
{
    if (vm.count(readerName+".printNcML")) {
        cout << readerName << " as NcML:" << endl;
        reader->getCDM().toXMLStream(cout);
        cout << endl;
    }
    if (vm.count(readerName+".printCS")) {
        cout << readerName + " CoordinateSystems: ";
        vector<boost::shared_ptr<const CoordinateSystem> > csVec = listCoordinateSystems(reader->getCDM());
        cout << csVec.size() << ": ";
        cout << joinPtr(csVec.begin(), csVec.end(), " | ");
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
	writeOptionAny(out, "debug", vm);
	writeOptionAny(out, "print-options", vm);
	writeOption<string>(out, "config", vm);
	writeOption<string>(out, "input.file", vm);
	writeOption<string>(out, "input.type", vm);
	writeOption<string>(out, "input.config", vm);
	writeOptionAny(out, "input.printNcML", vm);
    writeOptionAny(out, "input.printCS", vm);
	writeOption<string>(out, "output.file", vm);
	writeOption<string>(out, "output.type", vm);
	writeOption<string>(out, "output.config", vm);
    writeOption<string>(out, "qualityExtract.autoConfigString", vm);
    writeOption<string>(out, "qualityExtract.config", vm);
    writeOption<string>(out, "qualityExtract.printNcML", vm);
    writeOption<string>(out, "qualityExtract.printCS", vm);
	writeVectorOptionString(out, "extract.removeVariable", vm);
    writeVectorOptionString(out, "extract.selectVariables", vm);
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
    writeOptionAny(out, "extract.printNcML", vm);
    writeOptionAny(out, "extract.printCS", vm);
	writeOption<string>(out, "interpolate.projString", vm);
	writeOption<string>(out, "interpolate.method", vm);
	writeOption<string>(out, "interpolate.xAxisValues", vm);
	writeOption<string>(out, "interpolate.yAxisValues", vm);
	writeOption<string>(out, "interpolate.xAxisUnit", vm);
	writeOption<string>(out, "interpolate.yAxisUnit", vm);
	writeOption<string>(out, "interpolate.latitudeName", vm);
	writeOption<string>(out, "interpolate.longitudeName", vm);
    writeOption<string>(out, "interpolate.preprocess", vm);
	writeOptionAny(out, "interpolate.printNcML", vm);
    writeOptionAny(out, "interpolate.printCS", vm);
	writeOption<string>(out, "timeInterpolate.timeSpec", vm);
	writeOptionAny(out, "timeInterpolate.printNcML", vm);
	writeOptionAny(out, "timeInterpolate.printCS", vm);
	writeOption<string>(out, "ncml.config", vm);
    writeOptionAny(out, "ncml.printNcML", vm);
    writeOptionAny(out, "ncml.printCS", vm);
}

static string getType(const string& io, po::variables_map& vm) {
	string type;
	if (vm.count(io+".type")) {
		type = vm[io+".type"].as<string>();
	} else {
		boost::smatch what;
		if (boost::regex_match(vm[io+".file"].as<string>(), what, boost::regex(".*\\.(\\w+)$"))) {
			type = what[1].str();
		}
	}
	std::transform(type.begin(), type.end(), type.begin(), (int(*)(int)) tolower);
	return type;
}

static boost::shared_ptr<CDMReader> getCDMFileReader(po::variables_map& vm) {
	string type = getType("input", vm);
	boost::shared_ptr<CDMReader> returnPtr;
    if (type == "flt" || type == "dat" || type == "felt" || type == "flt2" || type == "dat2" || type == "felt2") {
        string config(DATADIR);
        config += "/flt2nc_variables.xml";
        if (vm.count("input.config")) {
            config = vm["input.config"].as<string>();
        }
#ifdef HAVE_LIBMIC
        if (type == "flt" || type == "dat" || type == "felt") {
            LOG4FIMEX(logger, Logger::DEBUG, "reading Felt-File " << vm["input.file"].as<string>() << " with config " << config);
            returnPtr = boost::shared_ptr<CDMReader>(new FeltCDMReader(vm["input.file"].as<string>(), config));
        } else {
#endif // HAVE_LIBMIC
        // use FELT library for all flt2 files, and for flt files if LIBMIC not available
        LOG4FIMEX(logger, Logger::DEBUG, "reading Felt-File2 " << vm["input.file"].as<string>() << " with config " << config);
        returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_FELT, vm["input.file"].as<string>(), config);
#ifdef HAVE_LIBMIC
        }
#endif // HAVE_LIBMIC
    }
#ifdef HAVE_GRIBAPI_H
    if (type == "grb" || type == "grib" ||
            type == "grb1" || type == "grib1" ||
                type == "grb2" || type == "grib2") {
        std::string config;
        if (vm.count("input.config")) {
            config = vm["input.config"].as<string>();
        }
        LOG4FIMEX(logger, Logger::DEBUG, "reading GribFile " << vm["input.file"].as<string>() << " with config " << config);
        returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_GRIB, vm["input.file"].as<string>(), config);
    }
#endif
#ifdef MIFI_HAVE_NETCDF
    if (type == "nc" || type == "cdf" || type == "netcdf" || type == "nc4") {
        if (vm.count("input.config")) {
            std::string config = vm["input.config"].as<string>();
            LOG4FIMEX(logger, Logger::DEBUG, "reading Netcdf-File " << vm["input.file"].as<string>() << " without config and applying ncml config");
            returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, vm["input.file"].as<string>(), config);
        } else {
            LOG4FIMEX(logger, Logger::DEBUG, "reading Netcdf-File " << vm["input.file"].as<string>() << " without config");
            returnPtr = CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, vm["input.file"].as<string>());
        }
	}
#endif

    if (returnPtr.get() == 0) {
        // cover all other types as defined in CDMConstants/CDMFileReaderFactory
        int maxId = mifi_get_max_filetype_number();
        for (int i = 0; i <= maxId; i++) {
            const char* iType= mifi_get_filetype_name(i);
            if (type == iType) {
                string config;
                if (vm.count("input.config")) {
                    config = vm["input.config"].as<string>();
                }
                LOG4FIMEX(logger, Logger::DEBUG, "reading file via "<<type<<" file '" << vm["input.file"].as<string>() << "' with config '" << config << "'");
                returnPtr = CDMFileReaderFactory::create(type, vm["input.file"].as<string>(), config);
            }
        }
    }

	if (returnPtr.get() == 0) {
		LOG4FIMEX(logger, Logger::FATAL, "unable to read type: " << type);
		exit(1);
	} else {
	    printReaderStatements("input", vm, returnPtr.get());
	}

	return returnPtr;
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
		if (vm.count("extract.reduceDimension.end")) {
			endPos = vm["extract.reduceDimension.end"].as<vector<int> >();
		}
		if (startPos.size() != vars.size()) {
			cerr << "extract.reduceDimension.start has not same no. of elements than extract.reduceDimension.name" << endl;
			cerr << "use start = 0 if you don't want to reduce the start-position" << endl;
		}
		if (endPos.size() != vars.size()) {
			cerr << "extract.reduceDimension.start has not same no. of elements than extract.reduceDimension.name" << endl;
			cerr << "use end = 0 if you don't want to reduce the end-position" << endl;
		}
		for (size_t i = 0; i < vars.size(); ++i) {
			extractor->reduceDimensionStartEnd(vars[i], startPos[i], endPos[i]);
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
        extractor->selectVariables(set<string>(vars.begin(), vars.end()));
	}
	if (vm.count("extract.removeVariable")) {
        vector<string> vars = vm["extract.removeVariable"].as<vector<string> >();
        for (vector<string>::iterator it = vars.begin(); it != vars.end(); ++it) {
            extractor->removeVariable(*it);
        }
    }
	printReaderStatements("extract", vm, extractor.get());

	return boost::shared_ptr<CDMReader>(extractor);
}

static boost::shared_ptr<CDMReader> getCDMQualityExtractor(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    string autoConf, config;
    if (vm.count("qualityExtract.autoConfigString")) autoConf = vm["qualityExtract.autoConfigString"].as<string>();
    if (vm.count("qualityExtract.config")) config = vm["qualityExtract.config"].as<string>();
    if (autoConf != "" || config != "") {
        LOG4FIMEX(logger, Logger::DEBUG, "adding CDMQualityExtractor with (" << autoConf << "," << config <<")");
        dataReader = boost::shared_ptr<CDMReader>(new CDMQualityExtractor(boost::shared_ptr<CDMReader>(dataReader), autoConf, config));
    }
    printReaderStatements("qualityExtract", vm, dataReader.get());

    return dataReader;
}


static boost::shared_ptr<CDMReader> getCDMTimeInterpolator(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
	if (! vm.count("timeInterpolate.timeSpec")) {
		LOG4FIMEX(logger, Logger::DEBUG, "timeInterpolate.timeSpec not found, no interpolation used");
		return dataReader;
	}
	boost::shared_ptr<CDMTimeInterpolator> timeInterpolator(new CDMTimeInterpolator(boost::shared_ptr<CDMReader>(dataReader)));
	timeInterpolator->changeTimeAxis(vm["timeInterpolate.timeSpec"].as<string>());
	printReaderStatements("timeInterpolate", vm, timeInterpolator.get());

	return boost::shared_ptr<CDMReader>(timeInterpolator);
}

static boost::shared_ptr<CDMReader> getCDMInterpolator(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
	if (! vm.count("interpolate.projString")) {
		LOG4FIMEX(logger, Logger::DEBUG, "interpolate.projString not found, no interpolation used");
		return dataReader;
	}
	boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(boost::shared_ptr<CDMReader>(dataReader)));
	if (vm.count("interpolate.latitudeName")) {
		interpolator->setLatitudeName(vm["interpolate.latitudeName"].as<string>());
	}
	if (vm.count("interpolate.longitudeName")) {
		interpolator->setLongitudeName(vm["interpolate.longitudeName"].as<string>());
	}

	if (vm.count("interpolate.preprocess")) {
	    boost::smatch what;
	    if (boost::regex_match(vm["interpolate.preprocess"].as<string>(), what, boost::regex(".*fill2d\\(([^,]+),([^,]+),([^)]+)\\).*"))) {
	        double critx = string2type<double>(what[1]);
	        double cor = string2type<double>(what[2]);
	        size_t maxLoop = string2type<size_t>(what[3]);
	        LOG4FIMEX(logger, Logger::DEBUG, "running interpolate preprocess: fill2d("<<critx<<","<<cor<<","<<maxLoop<<")");
	        interpolator->addPreprocess(boost::shared_ptr<InterpolatorProcess2d>(new InterpolatorFill2d(critx,cor,maxLoop)));
	    } else if (boost::regex_match(vm["interpolate.preprocess"].as<string>(), what, boost::regex(".*creepfill2d\\(([^,]+),([^,]+)\\).*"))) {
            unsigned short repeat = string2type<unsigned short>(what[1]);
            char setWeight = string2type<char>(what[2]);
            LOG4FIMEX(logger, Logger::DEBUG, "running interpolate preprocess: creepfill2d("<<repeat<<","<<setWeight<<")");
            interpolator->addPreprocess(boost::shared_ptr<InterpolatorProcess2d>(new InterpolatorCreepFill2d(repeat, setWeight)));
	    } else {
	        throw CDMException("undefined interpolate.preprocess: " + vm["interpolate.preprocess"].as<string>());
	    }
	}

	int method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
	if (vm.count("interpolate.method")) {
		string m = vm["interpolate.method"].as<string>();
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
			cerr << "WARNING: unknown interpolate.method: " << m << " using nearestneighbor" << endl;
		}
	}

	if (!(vm.count("interpolate.xAxisUnit") && vm.count("interpolate.yAxisUnit"))) {
		cerr << "ERROR: xAxisUnit and yAxisUnit required" << endl;
		exit(1);
	}

    if (!(vm.count("interpolate.xAxisValues") && vm.count("interpolate.yAxisValues"))) {
	    cerr << "ERROR: xAxisValues and yAxisValues required" << endl;
	    exit(1);
	}

	interpolator->changeProjection(method, vm["interpolate.projString"].as<string>(), vm["interpolate.xAxisValues"].as<string>(), vm["interpolate.yAxisValues"].as<string>(), vm["interpolate.xAxisUnit"].as<string>(), vm["interpolate.yAxisUnit"].as<string>());
	printReaderStatements("interpolate", vm, interpolator.get());

	return boost::shared_ptr<CDMReader>(interpolator);
}

static boost::shared_ptr<CDMReader> getNcmlCDMReader(po::variables_map& vm, boost::shared_ptr<CDMReader> dataReader) {
    if (! vm.count("ncml.config")) {
        return dataReader;
    }
    boost::shared_ptr<NcmlCDMReader> ncmlReader(new NcmlCDMReader(boost::shared_ptr<CDMReader>(dataReader),vm["ncml.config"].as<string>()));
    printReaderStatements("ncml", vm, ncmlReader.get());

    return boost::shared_ptr<CDMReader>(ncmlReader);
}


static void writeCDM(boost::shared_ptr<CDMReader> dataReader, po::variables_map& vm) {
	string type = getType("output", vm);
	// boost::shared_ptr to shared_ptr
	boost::shared_ptr<CDMReader> sharedDataReader(dataReader);
#ifdef MIFI_HAVE_NETCDF
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
#ifdef HAVE_GRIBAPI_H
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
	generic.add_options()
	    ("help,h", "help message")
	    ("version", "program version")
	    ("debug", "debug program")
	    ("print-options", "print all options")
	    ("config,c", po::value<string>(&configFile)->default_value(configFile), "configuration file")
	    ;

	// Declare a group of options that will be
	// allowed both on command line and in
	// config file
	po::options_description config("Configurational options");
	config.add_options()
		("input.file", po::value<string>(), "input file")
		("input.type", po::value<string>(), "filetype of input file, e.g. nc, nc4, ncml, felt, grib1, grib2, wdb")
		("input.config", po::value<string>(), "non-standard input configuration")
		("input.printNcML", "print NcML description of input file")
		("input.printCS", "print CoordinateSystems of input file")
		("output.file", po::value<string>(), "output file")
		("output.type", po::value<string>(), "filetype of output file, e.g. nc, nc4, grib1, grib2")
		("output.config", po::value<string>(), "non-standard output configuration")
		("extract.removeVariable", po::value<vector<string> >()->composing(), "remove variables")
        ("extract.selectVariables", po::value<vector<string> >()->composing(), "select only those variables")
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
        ("extract.printNcML", "print NcML description of extractor")
        ("extract.printCS", "print CoordinateSystems of extractor")
        ("qualityExtract.autoConfString", po::value<string>(), "configure the quality-assignment using CF-1.3 status-flag")
        ("qualityExtract.config", po::value<string>(), "configure the quality-assignment with a xml-config file")
        ("qualityExtract.printNcML", "print NcML description of extractor")
        ("qualityExtract.printCS", "print CoordinateSystems of extractor")
        ("interpolate.projString", po::value<string>(), "proj4 input string describing the new projection")
        ("interpolate.method", po::value<string>(), "interpolation method, one of nearestneighbor, bilinear, bicubic, coord_nearestneighbor, coord_kdtree, forward_max, forward_mean, forward_median or forward_sum")
        ("interpolate.xAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("interpolate.yAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition")
        ("interpolate.xAxisUnit", po::value<string>(), "unit of x-Axis given as udunits string, i.e. m or degrees_east")
        ("interpolate.yAxisUnit", po::value<string>(), "unit of y-Axis given as udunits string, i.e. m or degrees_north")
        ("interpolate.latitudeName", po::value<string>(), "name for auto-generated projection coordinate latitude")
        ("interpolate.longitudeName", po::value<string>(), "name for auto-generated projection coordinate longitude")
        ("interpolate.preprocess", po::value<string>(), "add a 2d preprocess to before the interpolation, e.g. \"fill2d(critx=0.01,cor=1.6,maxLoop=100)\" or \"creepfill2d(repeat=20,weight=2)\"")
        ("interpolate.printNcML", "print NcML description of interpolator")
        ("interpolate.printCS", "print CoordinateSystems of interpolator")
        ("timeInterpolate.timeSpec", po::value<string>(), "specification of times to interpolate to, see Fimex::TimeSpec for a full definition")
        ("timeInterpolate.printNcML", "print NcML description of timeInterpolator")
        ("timeInterpolate.printCS", "print CoordinateSystems of timeInterpolator")
        ("ncml.config", po::value<string>(), "modify/configure with ncml-file")
        ("ncml.printNcML", "print NcML description after ncml-configuration")
        ("ncml.printCS", "print CoordinateSystems after ncml-configuration")
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
    	cout << "fimex version " << VERSION << endl;
    	return 0;
    }
    if (vm.count("debug") >= 1) {
    	// TODO allow for multiple occurances and use INFO as == 1
    	defaultLogLevel(Logger::DEBUG);
    } else if (vm.count("debug") > 1) {
    	defaultLogLevel(Logger::DEBUG);
    }
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
	dataReader = getCDMQualityExtractor(vm, dataReader);
	dataReader = getCDMExtractor(vm, dataReader);
	dataReader = getCDMTimeInterpolator(vm, dataReader);
	dataReader = getCDMInterpolator(vm, dataReader);
	dataReader = getNcmlCDMReader(vm, dataReader);
	if (vm.count("output.file")) {
	    writeCDM(dataReader, vm);
	} else {
	    clog << "no output.file selected, only data-structure analysis possible" << endl;
	}

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
    } catch (exception& ex) {
        clog << "exception occured: " << ex.what() << endl;
        return 1;
    }
#endif
}

