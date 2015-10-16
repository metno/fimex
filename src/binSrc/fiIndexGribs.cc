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

#include "fimex/GribFileIndex.h"
#include "fimex/GribCDMReader.h"
#include "fimex/CDMconstants.h"
#include "fimex/ThreadPool.h"
#include "fimex/Utils.h"
#include "fimex/Logger.h"
#include <grib_api.h>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/shared_ptr.hpp>
#include <stdexcept>
#include <iostream>


namespace po = boost::program_options;
namespace fs = boost::filesystem;
using namespace std;

static void writeUsage(ostream& out, const po::options_description& options) {
    out << "usage: fiIndexGribs [ --outputDirectory DIRNAME | --appendFile GRMBL_NAME] [-c] -i gribFile" << endl;
    out << endl;
    out << options << endl;
}

void
indexGrib(const fs::path& input, const fs::path& append, const fs::path& output, vector<string> extraKeys, string config, bool force)
{
    std::vector<std::pair<std::string, boost::regex> > members; // empty members, doesn't make sense for single files
    std::map<std::string, std::string> options;
    if (config != "") {
        using namespace MetNoFimex;
        boost::shared_ptr<XMLDoc> doc = GribCDMReader::initXMLConfig(XMLInputFile(config));
        options["earthfigure"] = GribCDMReader::getConfigEarthFigure(doc);
        extraKeys.push_back(GribCDMReader::getConfigExtraKeys(doc));
    }
    if (extraKeys.size() > 0) {
        options["extraKeys"] = MetNoFimex::join(extraKeys.begin(), extraKeys.end(), ",");
    }
    MetNoFimex::GribFileIndex gfi(input, append, members, force, options);
    // open stream before filter, required for closing order
    std::ofstream realOutStream;
    boost::iostreams::filtering_ostream outStream;
#if BOOST_FILESYSTEM_VERSION == 3
    std::string outputStr = output.string();
#else
    std::string outputStr = output.file_string();
#endif
    if (outputStr.find_last_of(".gz") == (outputStr.size()-1)) {
        //cerr << "using gz" << endl;
        outStream.push(boost::iostreams::gzip_compressor(boost::iostreams::zlib::default_compression));
        realOutStream.open(outputStr.c_str(), std::ios::binary|std::ios::out);
    } else {
        //cerr << output.string().find_last_of(".gz") << " " << output.string().size() << endl;
        realOutStream.open(outputStr.c_str(), std::ios::out);
    }
    outStream.push(realOutStream);

//    fs::ofstream os(output);
    outStream << gfi;
// outStream auto-close will close the file
}

int
main(int argc, char* args[])
{
    // only use one thread
    mifi_setNumThreads(1);

    po::options_description options("options");
    options.add_options()
        ("help,h", "help message")
        ("debug", "debug option")
        ("version", "program version")
        ("force,f", "force update of index-file")
        ("extraKey", po::value<vector<string> >()->composing(), "multiple extraKey to index")
        ("readerConfig", po::value<string>(), "cdmGribReaderConfig as used by later calls. Using the config already during indexing will make sure that extraKeys and earthFigures correspond.")
        ("outputDirectory,o", po::value<string>(), "output directory")
        ("inputFile,i", po::value<string>(), "input gribFile")
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

    fs::path input(vm["inputFile"].as<string>());
    fs::path fullInput = fs::system_complete(input);
    if (!fs::exists(fullInput) && !fs::is_directory(fullInput)) {
        cerr << "inputFile " << fullInput << " does not exist";
        return 1;
    }

    fs::path outDir = fullInput.branch_path().string();
    if (vm.count("outputDirectory")) {
        outDir = fs::path(vm["outputDirectory"].as<string>());
        if (!fs::is_directory(outDir)) {
            cerr << "outputDir " << outDir << " is not a directory" << endl;
            return 1;
        }
    }
    std::string filename;
#if BOOST_FILESYSTEM_VERSION == 3
    filename = fullInput.filename().string();
#else
    filename = fullInput.leaf();
#endif
    fs::path outFile = outDir / (filename + ".grbml");
    bool forceUpdate = false;
    if (vm.count("force")) forceUpdate = true;
    vector<string> extraKeys;
    if (vm.count("extraKey")) {
        extraKeys = vm["extraKey"].as<vector<string> >();
    }
    string readerConfig("");
    if (vm.count("readerConfig")) {
        readerConfig = vm["readerConfig"].as<string>();
    }
    fs::path appendFile;
    if (vm.count("appendFile")) {
        appendFile = fs::path(vm["appendFile"].as<string>());
        outFile = appendFile;
    }
    indexGrib(fullInput, appendFile, outFile, extraKeys, readerConfig, forceUpdate);
    return 0;
}
