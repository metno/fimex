/*
 * Fimex, fiIndexGribs.cc
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
 *  Created on: Aug 14, 2009
 *      Author: Heiko Klein
 */

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"
#include "fimex/GribCDMReader.h"
#include "fimex/GribFileIndex.h"
#include "fimex/Logger.h"
#include "fimex/StringUtils.h"
#include "fimex/ThreadPool.h"

#include <boost/program_options.hpp>

#include <grib_api.h>

#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>

namespace po = boost::program_options;
using namespace std;

static void writeUsage(ostream& out, const po::options_description& options) {
    out << "usage: fiIndexGribs [ --outputDirectory DIRNAME | --appendFile GRMBL_NAME] [-c] -i gribFile" << endl;
    out << endl;
    out << options << endl;
}

void indexGrib(const std::string& input, const std::string& append, const std::string& output, vector<string> extraKeys, string config,
               vector<string> memberOptions)
{
    std::map<std::string, std::string> options;
    if (config != "") {
        using namespace MetNoFimex;
        XMLDoc_p doc = GribCDMReader::initXMLConfig(XMLInputFile(config));
        options["earthfigure"] = GribCDMReader::getConfigEarthFigure(doc);
        extraKeys.push_back(GribCDMReader::getConfigExtraKeys(doc));
    }
    if (extraKeys.size() > 0) {
        options["extraKeys"] = MetNoFimex::join(extraKeys.begin(), extraKeys.end(), ",");
    }
    std::vector<std::pair<std::string, std::regex>> members;
    if (memberOptions.size() > 0) {
        vector<pair<string, string> > memberStrings;
        vector<string> files;
        MetNoFimex::CDMFileReaderFactory::parseGribArgs(memberOptions, memberStrings, files);
        for (vector<pair<string, string> >::const_iterator memIt = memberStrings.begin(); memIt != memberStrings.end(); ++memIt) {
            members.push_back(make_pair(memIt->first, std::regex(memIt->second)));
        }
    }
    MetNoFimex::GribFileIndex gfi(input, append, members, options);

    std::ofstream outStream(output, std::ios::binary);
    outStream << gfi;
}

int main(int argc, char* args[])
{
    // only use one thread
    mifi_setNumThreads(1);

    po::options_description options("options");
    options.add_options()
        ("help,h", "help message")
        ("debug", "debug option")
        ("version", "program version")
        ("extraKey", po::value<vector<string> >()->composing(), "multiple extraKey to index")
        ("readerConfig", po::value<string>(), "cdmGribReaderConfig as used by later calls. Using the config already during indexing will make sure that extraKeys and earthFigures correspond.")
        ("outputFile,o", po::value<string>(), "output grbml file")
        ("inputFile,i", po::value<string>(), "input gribFile")
        ("input.optional", po::value<vector<string> >()->composing(), "optional arguments for grib-files as in fimex, i.e. memberRegex: , memberName: pairs")
        ("appendFile,a", po::value<string>(), "append output new index to a grbml-file")
        ;

    // read the options
    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(options).run(), vm);
    po::notify(vm);

    if (argc == 1 || vm.count("help")) {
        writeUsage(cout, options);
        return 0;
    }
    if (vm.count("debug") >= 1) {
        // TODO allow for multiple occurances and use INFO as == 1
        MetNoFimex::defaultLogLevel(MetNoFimex::Logger::DEBUG);
    } else if (vm.count("debug") > 1) {
        MetNoFimex::defaultLogLevel(MetNoFimex::Logger::DEBUG);
    }
    if (vm.count("version")) {
        cout << "fiIndexGribs version " << fimexVersion() << endl;
        return 0;
    }
    if (vm.count("inputFile") == 0) {
        cerr << "missing input file" << endl;
        writeUsage(cout, options);
        return 1;
    }
    const std::string inputFile(vm["inputFile"].as<string>());

    std::string outputFile = inputFile + ".grbml";
    if (vm.count("outputFile"))
        outputFile = vm["outputFile"].as<string>();

    vector<string> extraKeys;
    if (vm.count("extraKey")) {
        extraKeys = vm["extraKey"].as<vector<string> >();
    }
    string readerConfig("");
    if (vm.count("readerConfig")) {
        readerConfig = vm["readerConfig"].as<string>();
    }
    vector<string> members;
    if (vm.count("input.optional")) {
        members = vm["input.optional"].as<vector<string> >();
    }
    std::string appendFile;
    if (vm.count("appendFile")) {
        outputFile = appendFile = vm["appendFile"].as<string>();
    }
    indexGrib(inputFile, appendFile, outputFile, extraKeys, readerConfig, members);
    return 0;
}
