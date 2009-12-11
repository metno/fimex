/*
 * Fimex, fiGribCut.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Dec 11, 2009
 *      Author: Heiko Klein
 */

#include "fimex/config.h"
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>
#include <fstream>
#include <iostream>
#include <cstdio>
#include <grib_api.h>
#include "fimex/Utils.h"

namespace po = boost::program_options;
namespace fs = boost::filesystem;
using namespace std;

static int debug = 0;

static void writeUsage(ostream& out, const po::options_description& options) {
    out << "usage: fiGribCut --outputFile PATH --inputFile gribFile [--inputfile gribfile] " << endl;
    out << "                 --parameter PARAM1 [--parameter PARAM2]" << endl;
    out << endl;
    out << options << endl;
}

static bool gribMatchParameters(boost::shared_ptr<grib_handle>& gh, vector<long> parameters)
{
    if (parameters.size() == 0) return true; // all parameters requested
    long param;
    GRIB_CHECK(grib_get_long(gh.get(), "paramId", &param), 0);
    return find(parameters.begin(), parameters.end(), param) != parameters.end();
}

// work on one grib_hanlde, return number of errors
static int gribCutHandle(ofstream& outStream, boost::shared_ptr<grib_handle>& gh, const vector<long>& parameters, const map<string, double>& bb)
{
    // skip parameters if not matching
    if (!gribMatchParameters(gh, parameters)) return 0;


    if (bb.size() == 4) {
        char msg[1024];
        size_t msgLength = 1024;
        GRIB_CHECK(grib_get_string(gh.get(), "typeOfGrid", msg, &msgLength), 0);
        string typeOfGrid(msg);
        if (typeOfGrid != "regular_ll") {
            cerr << "cannot only attach bounding-box to type regular_ll, got type " << typeOfGrid << endl;
            return 1;
        } else {
            // create a clone of the handle for later modifications
            gh = boost::shared_ptr<grib_handle>(grib_handle_clone(gh.get()), grib_handle_delete);
            // TODO modify bounding box

        }
    }
    // write data to file
    size_t bufferSize;
    const void* buffer;
    /* get the coded message in a buffer */
    GRIB_CHECK(grib_get_message(gh.get(),&buffer,&bufferSize),0);
    outStream.write(reinterpret_cast<const char*>(buffer), bufferSize);
    return 0;
}

// work on all files/all messages, return number of errors
static int gribCut(ofstream& outStream, const vector<string>& inputFiles, const vector<long>& parameters, const map<string, double>& bb)
{
    int errors = 0;
    for (vector<string>::const_iterator file = inputFiles.begin(); file != inputFiles.end(); ++file) {
        boost::shared_ptr<FILE> fh(fopen(file->c_str(), "r"), fclose);
        if (fh.get() == 0) {
            cerr << "cannot open file: " << *file << endl;
            ++errors;
        } else {
            // enable multi-messages
            grib_multi_support_on(0);
            while (!feof(fh.get())) {
                // read the messages of interest
                size_t pos = ftell(fh.get());
                int err = 0;
                boost::shared_ptr<grib_handle> gh(grib_handle_new_from_file(0, fh.get(), &err), grib_handle_delete);
                size_t newPos = ftell(fh.get());
                if (debug > 0) {
                    cerr << "fetching handle from file " << *file << " from pos " << pos << " to pos " << newPos << endl;
                }
                // check for errors
                if (gh.get() != 0) {
                    // something wrong with file, abbort
                    if (err != GRIB_SUCCESS) GRIB_CHECK(err,0);
                    // parse the grib handle
                    errors += gribCutHandle(outStream, gh, parameters, bb);
                }
            }
        }
    }
    return errors;
}



int
main(int argc, char* args[])
{
    /*
     * inputFile: path; repeatable = concattenation
     * outputFile: path; not-repeatable
     * parameter: grib-api parameterIds; repeatable, not required
     * boundingBox: north,east,south,west values of geographical boundingbox, i.e. 90,30,60,-30; non-repeatable, not required
     */
    po::options_description options("options");
    options.add_options()
        ("help,h", "help message")
        ("debug,d", "enable debug")
        ("version,v", "program version")
        ("outputFile,o", po::value<string>(), "outputFile")
        ("inputFile,i", po::value<vector<string> >()->composing(), "input gribFile")
        ("parameter,p", po::value<vector<long> >()->composing(), "grib-parameterID")
        ("boundingBox,b", po::value<string>(), "bounding-box, north,east,south,west")
        ;

    // read the options
    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(options).run(), vm);
    po::notify(vm);

    debug = vm.count("debug");

    if (argc == 1 || vm.count("help")) {
        writeUsage(cout, options);
        return 0;
    }
    if (vm.count("version")) {
        cout << "fiIndexGribs version " << VERSION << endl;
        return 0;
    }
    if (vm.count("inputFile") == 0) {
        cerr << "missing input file" << endl;
        writeUsage(cout, options);
        return 1;
    }
    vector<string> inputFiles = vm["inputFile"].as<vector<string> >();

    if (vm.count("outputFile") == 0) {
        cerr << "missing output file" << endl;
        writeUsage(cout, options);
        return 1;
    }
    ofstream outStream(vm["outputFile"].as<string>().c_str(), std::ios::binary|std::ios::out);

    vector<long> parameters;
    if (vm.count("parameter") > 0) {
        parameters = vm["parameter"].as<vector<long> >();
    }

    map<string, double> bb;
    if (vm.count("boundingBox") > 0) {
        vector<string> bbVec = vm["boundingBox"].as<vector<string> >();
        if (bbVec.size() < 4) {
            cerr << "boundingBox requires 4 values: north,east,south,west, got " << bb.size() << " values" << endl;
            return 1;
        }
        double north = MetNoFimex::string2type<double>(bbVec.at(0));
        double east  = MetNoFimex::string2type<double>(bbVec.at(1));
        double south = MetNoFimex::string2type<double>(bbVec.at(2));
        double west  = MetNoFimex::string2type<double>(bbVec.at(3));
        if (north > 90 || north < -90) {
            cerr << "north needs to be between -90 and 90 degree, is: "<< north << endl;
            return 1;
        }
        if (south > 90 || south < -90) {
            cerr << "south needs to be between -90 and 90 degree, is: "<< south << endl;
            return 1;
        }
        if (south > north) {
            cerr << "south must be <= north" << endl;
            return 1;
        }
        if (east > 180 || east < -180) {
            cerr << "east needs to be between -180 and 180 degree, is: "<< east << endl;
            return 1;
        }
        if (west > 180 || west < -180) {
            cerr << "west needs to be between -180 and 180 degree, is: "<< west << endl;
            return 1;
        }
        if (west > east) {
            cerr << "west must be <= east" << endl;
            return 1;
        }
        bb["north"] = north;
        bb["south"] = south;
        bb["east"] = east;
        bb["west"] = west;
    }

    int errors = gribCut(outStream, inputFiles, parameters, bb);
    if (errors > 0) {
        cerr << "found " << errors << " errors" << endl;
    }
    return errors;
}
