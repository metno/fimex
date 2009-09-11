/*
 * Fimex, indexGribs.cc
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

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/shared_ptr.hpp>
#include <stdexcept>
#include "fimex/config.h"
#include "fimex/GribFileIndex.h"
#include "grib_api.h"


namespace po = boost::program_options;
namespace fs = boost::filesystem;
using namespace std;

static void writeUsage(ostream& out, const po::options_description& options) {
    out << "usage: indexGribs --outputDirectory DIRNAME --inputFile gribFile" << endl;
    out << endl;
    out << options << endl;
}

void
indexGrib(const fs::path& input, const fs::path& output, bool force)
{
    MetNoFimex::GribFileIndex gfi(input, force);
    fs::ofstream os(output);
    os << gfi;
    os.close();
}

int
main(int argc, char* args[])
{
    po::options_description options("options");
    options.add_options()
        ("help,h", "help message")
        ("version", "program version")
        ("force,f", "force update of index-file")
        ("outputDir,o", po::value<string>(), "output directory")
        ("inputFile,i", po::value<string>(), "input gribFile")
        ;

    // read the options
    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(options).run(), vm);
    po::notify(vm);

    if (argc == 1 || vm.count("help")) {
        writeUsage(cout, options);
        return 0;
    }
    if (vm.count("version")) {
        cout << "indexGribs version " << VERSION << endl;
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
    if (vm.count("outputDir")) {
        outDir = fs::path(vm["outputDir"].as<string>());
        if (!fs::is_directory(outDir)) {
            cerr << "outputDir " << outDir << " is not a directory" << endl;
            return 1;
        }
    }
    std::string filename = fullInput.leaf();
    fs::path outFile = outDir / (filename + ".grbml");

    bool forceUpdate = false;
    if (vm.count("force")) forceUpdate = true;
    indexGrib(fullInput, outFile, forceUpdate);
    return 0;
}
