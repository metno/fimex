/*
 * Fimex, fiIndexNc.cc
 *
 * (C) Copyright 2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

/**
 * @file
 * Command-line tool to build a protobuf index for a NetCDF4/HDF5 file.
 * The index can later be opened with fimex as a thread-safe replacement
 * for the original NetCDF file, avoiding libnetcdf and libhdf5 at read time.
 *
 * Usage: see fiIndexNc --help
 */

#include "NetCDFIoFactory.h"
#include "NetCDFProtobufIndexWriter.h"
#include "fimex/CDMconstants.h"
#include "fimex/FileUtils.h"
#include "fimex/Logger.h"

#include <mi_programoptions.h>

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace po = miutil::program_options;
using namespace MetNoFimex;

namespace {

Logger_p logger = getLogger("fiIndexNc");

void writeUsage(std::ostream& out, const po::option_set& options)
{
    out << "usage: fiIndexNc --input.file input.nc --output.file output." << FILETYPE_NCFP << "\n";
    out << "  Write index of 'input.nc' to 'output." << FILETYPE_NCFP << "'.\n";
    out << "usage: fiIndexNc filename.nc\n";
    out << "  Write index of 'filename.nc' to 'filename." << FILETYPE_NCFP << "'.\n";
    out << "\n";
    out << "  Builds a protobuf index for a NetCDF4/NetCDF3 file.\n";
    out << "  The index embeds the full CDM metadata plus per-variable\n";
    out << "  chunk layout, so fimex can later read data without libnetcdf/\n";
    out << "  and without any global lock.\n";
    out << "\n";
    out << "  To use this index in fimex, use the " << FILETYPE_NCFP << " extension or specify\n"
        << "  file type '" << FILETYPE_NCFP << "'. Depending on the storage location, it might\n"
        << "  also be necessary to specify 'root_path' in xml config.";
    out << "\n";
    options.help(out);
}

} // namespace

int main(int argc, char* argv[])
{
    const auto op_help = po::option("help", "print this help message");
    const auto op_version = po::option("version", "print version information");
    const auto op_input_file = po::option("input.file", "input NetCDF4 file");
    const auto op_input_src = po::option("input.src", "path to NetCDF4 file to be stored in protobuf");
    const auto op_output_file = po::option("output.file", "output index file (default: INPUT with extension replaced by ." + std::string(FILETYPE_NCFP) + ")");
    const auto op_loglvl = po::option("log-level", "log level: OFF ERROR WARN INFO DEBUG").set_default_value("WARN");

    po::option_set options;
    options << op_help << op_version << op_input_file << op_input_src << op_output_file << op_loglvl;

    std::vector<std::string> positional;
    try {
        po::value_set vm = po::parse_command_line(argc, argv, options, positional);

        if (vm.is_set(op_help)) {
            writeUsage(std::cout, options);
            return 0;
        }
        if (vm.is_set(op_version)) {
            std::cout << "fiIndexNc version " << fimexVersion() << std::endl;
            return 0;
        }

        Logger::LogLevel level = Logger::WARN;
        const std::string lv = vm.value(op_loglvl);
        if (lv == "OFF")
            level = Logger::OFF;
        else if (lv == "ERROR")
            level = Logger::ERROR;
        else if (lv == "WARN")
            level = Logger::WARN;
        else if (lv == "INFO")
            level = Logger::INFO;
        else if (lv == "DEBUG")
            level = Logger::DEBUG;
        defaultLogLevel(level);

        std::string input_src;
        if (vm.is_set(op_input_src))
            input_src = vm.value(op_input_src);
        std::string input_file;
        if (vm.is_set(op_input_file))
            input_file = vm.value(op_input_file);
        else if (positional.size() == 1)
            input_file = positional.front();
        else {
            std::cerr << "Error: exactly one input file is supported at a time.\n\n";
            writeUsage(std::cerr, options);
            return 1;
        }
        if (input_src.empty())
            input_src = input_file;

        std::string output_file;
        if (vm.is_set(op_output_file)) {
            output_file = vm.value(op_output_file);
        } else {
            output_file = replaceExtension(input_file, FILETYPE_NCFP);
        }

        LOG4FIMEX(logger, Logger::INFO, "Indexing '" << input_file << "' → '" << output_file << "'");
        NetCDFProtobufIndexWriter().write(input_file, input_src, output_file);
        LOG4FIMEX(logger, Logger::INFO, "Done.");
        return 0;

    } catch (const po::option_error& ex) {
        std::cerr << "Option error: " << ex.what() << "\n\n";
        writeUsage(std::cerr, options);
        return 1;
    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }
}
