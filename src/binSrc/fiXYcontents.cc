/*
 * Fimex, fiContents.cc
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


#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"
#include "fimex/Logger.h"
#include "fimex/ThreadPool.h"
#include "fimex/TimeUnit.h"


namespace po = boost::program_options;
using namespace std;
using namespace MetNoFimex;

static void writeUsage(ostream& out, const po::options_description& generic) {
    out << "usage: fiXYcontents --input.file  FILENAME [--input.type  INPUT_TYPE]" << endl;
    out << "             [--input.config CFGFILENAME]" << endl;
    out << "             [--input.optional OPT1 --input.optional OPT2 ...]" << endl;
    out << "             [--num_threads ...]" << endl;
    out << "             [--verticalLayer [units:hPa|no|stdname]]" << endl;
    out << "             [--layerValue ... | layerPosition]" << endl;
    out << "             [--varName VAR1 --varName VAR2 --stdName STDNM3]" << endl;
    out << "             [--forecastTime ... | --dates ...]" << endl;
    out << "             [--stats mean,median,min,max,stddev,def,undef]" << endl;
    out << endl;
    out << generic << endl;
}

static boost::shared_ptr<CDMReader> getCDMFileReader(po::variables_map& vm) {
    string name = vm["input.file"].as<string>();
    string type;
    if (vm.count("input.type")) {
        type = vm["input.type"].as<string>();
    } else {
        type = mifi_get_filetype_name(CDMFileReaderFactory::detectFileType(name));
    }
    string config;
    if (vm.count("input.config")) {
        config = vm["input.config"].as<string>();
    }
    vector<string> opts;
    if (vm.count("input.optional")) {
        opts = vm["input.optional"].as<vector<string> >();
    }
    return CDMFileReaderFactory::create(type, name, config, opts);
}

static void runStats(po::variables_map& vm, boost::shared_ptr<CDMReader>& reader)
{
    vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(reader);
    const CDM& cdm = reader->getCDM();
    const CDM::VarVec& variables = cdm.getVariables();
    set<string> varNames;
    if (vm.count("varName") || vm.count("stdName")) {
        if (vm.count("varName")) {
            vector<string> vars = vm["varName"].as<vector<string> >();
            varNames.insert(vars.begin(), vars.end());
        }
        if (vm.count("stdName")) {
            vector<string> stds = vm["stdName"].as<vector<string> >();
            for (vector<string>::iterator it = stds.begin(); it != stds.end(); ++it) {
                vector<string> vars = cdm.findVariables("standard_name", *it + "(\\s.*)?");
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
    if (vm.count("verticalLayer")) {
        string vl = vm["verticalLayer"].as<string>();
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
        boost::posix_time::ptime refTime = boost::posix_time::not_a_date_time;
        map<string, DataPtr> dimData; // hours since refTime


        vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(*varIt));
        if (varSysIt != coordSys.end()) {
            if (vm.count("verticalLayer")) {
                CoordinateSystem::ConstAxisPtr zAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
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
            if ((*varSysIt)->isSimpleSpatialGridded()) {
                CoordinateSystem::ConstAxisPtr xAxis = (*varSysIt)->getGeoXAxis(); // X or Lon
                CoordinateSystem::ConstAxisPtr yAxis = (*varSysIt)->getGeoYAxis(); // Y or Lat
                xSize = cdm.getDimension(xAxis->getName()).getLength();
                ySize = cdm.getDimension(yAxis->getName()).getLength();
                CoordinateSystem::ConstAxisPtr tAxis = (*varSysIt)->getTimeAxis(); // time

                CoordinateSystemSliceBuilder sb(cdm, *varSysIt);
                // handling of time
                try {
                    refTime = getUniqueForecastReferenceTime(reader);
                } catch (runtime_error& rex) {
                    // ignore
                }
                if (tAxis.get() != 0) {
                    // time-Axis, eventually multi-dimensional, i.e. forecast_reference_time
                    if ((*varSysIt)->hasAxisType(CoordinateAxis::ReferenceTime)) {
                        CoordinateSystem::ConstAxisPtr rtAxis = (*varSysIt)->findAxisOfType(CoordinateAxis::ReferenceTime);
                        DataPtr refTimes = reader->getScaledDataInUnit(rtAxis->getName(),"minutes since 1970-01-01 00:00:00 +00:00");
                        TimeUnit tu("minutes since 1970-01-01 00:00:00 +00:00");
                        /* do something with the refTimes and select the wanted Position */
                        size_t refTimePos = refTimes->size()-1; /* choose latest refTime */
                        sb.setReferenceTimePos(refTimePos);
                        refTime = tu.unitTime2fimexTime(refTimes->asDouble()[refTimes->size()-1]).asPosixTime();
                    }
                    if (refTime == boost::posix_time::not_a_date_time) {
                        // use first time-stamp
                        TimeUnit tu("minutes since 1970-01-01 00:00:00 +00:00");
                        tData = reader->getScaledDataSliceInUnit(tAxis->getName(), "minutes since 1970-01-01 00:00:00 +00:00", sb.getTimeVariableSliceBuilder());
                        refTime = tu.unitTime2fimexTime(tData->asDouble()[0]).asPosixTime();
                    }
                    tData = reader->getScaledDataSliceInUnit(tAxis->getName(), "hours since "+boost::posix_time::to_iso_extended_string(refTime), sb.getTimeVariableSliceBuilder());
                    /* select the desired startTime and the sice for the time-slices */
                    // fetch the 2nd and 3rd time-step of the 4th run
                    for (size_t i = 0; i < tData->size(); i++) {
                        sb.setTimeStartAndSize(i, 1);
                        sb.setAll(xAxis);
                        sb.setAll(yAxis);
                        csbs.push_back(sb);
                    }
                }

                CoordinateSystem::ConstAxisPtr zAxis = (*varSysIt)->getGeoZAxis(); // Y or Lat
                if (zAxis != 0) {
                    if (vlType == vlUnit) {
                        zData = reader->getScaledDataInUnit(zAxis->getName(), vlName);
                    } else {
                        zData = reader->getScaledData(zAxis->getName());
                    }
                    vector<CoordinateSystemSliceBuilder> csbs_temp;
                    for (vector<CoordinateSystemSliceBuilder>::iterator sbIt = csbs.begin(); sbIt != csbs.end(); ++sbIt) {
                        for (size_t i = 0; i < zData->size(); i++) {
                            sbIt->setStartAndSize(zAxis, i, 1);
                            csbs_temp.push_back(*sbIt);
                        }
                    }
                    csbs = csbs_temp;
                }

                // by default, all other dimensions are fetched at maximum size
                // here, I reduce them to the first slice
                vector<string> dims = sb.getUnsetDimensionNames();
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

                /* do something with the data */
            } else {
                // TODO info var not simple spatial gridded
            }
        } else {
            // TODO var without coordsys (ignore on default)
        }


        if (csbs.size() > 0) {
            CDMAttribute attr;
            string stdName, unit;
            if (cdm.getAttribute(*varIt, "standard_name", attr)) {
                stdName = attr.getStringValue();
            }
            if (cdm.getAttribute(*varIt, "units", attr)) {
                unit = attr.getStringValue();
            }
            printf("Var: %17s %17s %10s %15s %dx%d: %d\n", varIt->c_str(), stdName.c_str(), unit.c_str(), boost::posix_time::to_iso_string(refTime).c_str(), xSize, ySize, csbs.size());

            // fetch the data
            //DataPtr data = reader->getDataSlice(*varIt, sb);
        }

    }
    return;
}

int run(int argc, char* args[])
{
    // Declare the supported options.
    po::options_description generic("Options");
    int num_threads = 1;
    generic.add_options()
        ("help,h", "help message")
        ("version", "program version")
        ("debug", "debug program")
        ("log4cpp", po::value<string>(), "log4cpp property file (- = log4cpp without prop-file)")
        ("num_threads,n", po::value<int>(&num_threads)->default_value(num_threads), "number of threads")
        ("input.file", po::value<string>(), "input file")
        ("input.type", po::value<string>(), "filetype of input file, e.g. nc, nc4, ncml, felt, grib1, grib2, wdb")
        ("input.config", po::value<string>(), "non-standard input configuration")
        ("input.optional", po::value<vector<string> >()->composing(), "additional options, e.g. multiple files for grib")
        ("verticalLayer", po::value<vector<string> >()->composing(), "vertical layer definitions, defined by standard_name or units (or no for no (or size 1) vertical axis), e.g. units:hPa, no, atmosphere_hybrid_sigma_pressure_coordinate")
        ("layerValue", po::value<string>(), "vertical layer values, e.g. 1000,950,...,500,300,100")
        ("stdName", po::value<vector<string> >()->composing(), "standard_name of parameters")
        ("varName", po::value<vector<string> >()->composing(), "variable_name of parameters")
        ("forecastTime", po::value<string>(), "forecast hours since reference time, e.g. 3,6,12,15,...,24,36")
        ("stats", po::value<string>()->default_value("def,mean"), "comma-separated list of stats to show, possible: mean,median,min,max,stddev,def,undef")
        ;


    po::options_description cmdline_options;
    cmdline_options.add(generic);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(cmdline_options).run(), vm);
    po::notify(vm);
    if (argc == 1 || vm.count("help")) {
        writeUsage(cout, generic);
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
    LoggerPtr logger = getLogger("fimex");
    if (!(vm.count("input.file"))) {
        writeUsage(cerr, generic);
        LOG4FIMEX(logger, Logger::FATAL, "input.file required");
        return 1;
    }

    boost::shared_ptr<CDMReader> dataReader = getCDMFileReader(vm);
    runStats(vm, dataReader);
    // TODO: do something with the dataReader

    return 0;
}

int main(int argc, char* args[])
{

    int retStatus = 0;

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

