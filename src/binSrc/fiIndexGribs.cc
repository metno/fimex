/*
 * Fimex, fiIndexGribs.cc
 *
 * (C) Copyright 2009-2026, met.no
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
 *
 *  Created on: Aug 14, 2009
 *      Author: Heiko Klein
 */

#include "GribIoFactory.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"

#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/StringUtils.h"
#include "fimex/ThreadPool.h"
#include "fimex/XMLInputFile.h"
#include "fimex/XMLUtils.h"

#include "GribCDMIndexer.h"
#include "GribFileIndex.h"
#include "GribReaderConfig.h"

#include <mi_programoptions.h>

#include <cstdint>
#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>

#include "fimex_grib_config.h"
#ifdef HAVE_PROTOBUF
#include "GribProtobufIndexWriter.h"
#endif

namespace po = miutil::program_options;
using namespace MetNoFimex;

namespace {

Logger_p logger = getLogger("fiIndexGribs");

void writeUsage(std::ostream& out, const po::option_set& options)
{
    out << "usage: fiIndexGribs -o/--outputFile GRBML_NAME [-c/--readerConfig gribreaderconfig.xml] [-i] gribFile [[-i] gribFile2 [...]]" << std::endl;
    out << "  When creating, one or more input file(s) must be specified." << std::endl;
    out << "usage: fiIndexGribs -a/--appendFile GRBML_NAME [-c/--readerConfig gribreaderconfig.xml] [-i] gribFile" << std::endl;
    out << "  When appending, exactly one input file must be specified." << std::endl;
    out << "usage: fiIndexGribs --indexFile GRBML_NAME FIIN_FILE [-c/--readerConfig gribreaderconfig.xml]" << std::endl;
    out << "  When creating an index, no input file may be specified." << std::endl;
    out << std::endl;
    options.help(out);
}

void initOptions(std::map<std::string, std::string>& options, std::vector<std::pair<std::string, std::regex>>& members, std::vector<std::string> extraKeys,
                 std::string config, std::vector<std::string> memberOptions)
{
    if (!config.empty()) {
        XMLDoc_p doc = GribReader::initXMLConfig(XMLInputFile(config));
        options["earthfigure"] = GribReader::getConfigEarthFigure(doc);
        extraKeys.push_back(GribReader::getConfigExtraKeys(doc));
    }
    if (!extraKeys.empty()) {
        options["extraKeys"] = MetNoFimex::join(extraKeys.begin(), extraKeys.end(), ",");
    }
    if (!memberOptions.empty()) {
        std::vector<std::pair<std::string, std::string>> memberStrings;
        std::vector<std::string> files;
        MetNoFimex::parseGribArgs(memberOptions, memberStrings, files);
        members = GribCDMIndexer::makeMembersRegex(memberStrings);
    }
}

std::string ensureScheme(const std::string& url)
{
    if (starts_with(url, "http://") || starts_with(url, "https://") || starts_with(url, "file://"))
        return url;
    else
        return "file:" + url;
}

struct GribIndexWriter {
    GribIndexWriter(const std::string& output, const std::string& url);
    ~GribIndexWriter();

    std::ofstream os;
};

GribIndexWriter::GribIndexWriter(const std::string& output, const std::string& url)
    : os(output, std::ios::binary)
{
    os << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << std::endl;
    os << "<gribFileIndex url=\"" << url << "\" xmlns=\"http://www.met.no/schema/fimex/gribFileIndex\">" << std::endl;
}

GribIndexWriter::~GribIndexWriter()
{
    os << "</gribFileIndex>" << std::endl;
}

void createGrbmlFromGRIB(ChunkReaderFactory_p ca, const std::vector<std::string>& inputs, const std::string& output, std::vector<std::string> extraKeys,
                         std::string config, std::vector<std::string> memberOptions)
{
    std::map<std::string, std::string> options;
    std::vector<std::pair<std::string, std::regex>> members;
    initOptions(options, members, extraKeys, config, memberOptions);

    GribIndexWriter w(output, ensureScheme(inputs.front()));
    for (const auto& input : inputs) {
        LOG4FIMEX(logger, Logger::DEBUG, "Start processing '" << input << "'");
        const GribFileIndex gfi(ca, input, "", members, options);
        for (const auto& gfm : gfi.listMessages())
            w.os << gfm;
    }
}

void appendToGrbml(ChunkReaderFactory_p ca, const std::string& input, const std::string& append, std::vector<std::string> extraKeys, std::string config,
                   std::vector<std::string> memberOptions)
{
    std::map<std::string, std::string> options;
    std::vector<std::pair<std::string, std::regex>> members;
    initOptions(options, members, extraKeys, config, memberOptions);

    LOG4FIMEX(logger, Logger::DEBUG, "Reading '" << append << "' and processing '" << input << "'");
    const GribFileIndex gfi(ca, input, append, members, options);

    LOG4FIMEX(logger, Logger::DEBUG, "Writing to '" << append << "'");
    GribIndexWriter w(append, ensureScheme(input));
    for (const auto& gfm : gfi.listMessages())
        w.os << gfm;
}

#ifdef HAVE_PROTOBUF
void createGribfpFromGrbml(ChunkReaderFactory_p ca, const std::string& grbml, const std::string& output, std::vector<std::string> extraKeys,
                           const std::string& config, const std::vector<std::string>& memberOptions)
{
    LOG4FIMEX(logger, Logger::DEBUG, "Reading grbml file '" << grbml << "' ...");
    std::map<std::string, std::string> options;
    std::vector<std::pair<std::string, std::regex>> members;
    initOptions(options, members, extraKeys, config, memberOptions);

    if (config.empty()) {
        throw std::runtime_error("grib reader config required to create FimexIndex from grbml");
    }
    XMLInputDoc configXML = createXMLInput(config);

    GribCDMIndexer grind(configXML, members, ca);
    grind.load(grbml);
    auto cdm = std::make_shared<CDM>();
    auto grib_indexed = std::make_shared<GribCDMIndexer::grib_indexed>();
    grind.build(cdm, grib_indexed);

    LOG4FIMEX(logger, Logger::DEBUG, "Writing to '" << output << "' ...");
    GribProtobufIndexWriter writer;
    writer.write(*cdm, *grib_indexed, output);
}

void createGrbfpFromGRIB(ChunkReaderFactory_p ca, const std::vector<std::string>& inputs, const std::string& output, std::vector<std::string> extraKeys,
                         const std::string& config, const std::vector<std::string>& memberOptions)
{
    std::map<std::string, std::string> options;
    std::vector<std::pair<std::string, std::regex>> members;
    initOptions(options, members, extraKeys, config, memberOptions);

    if (config.empty()) {
        throw std::runtime_error("grib reader config required to create FimexIndex from grbml");
    }
    XMLInputDoc configXML = createXMLInput(config);

    GribCDMIndexer grind(configXML, members, ca);
    grind.load(inputs);
    auto cdm = std::make_shared<CDM>();
    auto grib_indexed = std::make_shared<GribCDMIndexer::grib_indexed>();
    grind.build(cdm, grib_indexed);

    LOG4FIMEX(logger, Logger::DEBUG, "Writing to '" << output << "' ...");
    GribProtobufIndexWriter writer;
    writer.write(*cdm, *grib_indexed, output);
}
#endif

} // namespace

int main(int argc, char* args[])
{
    // only use one thread
    mifi_setNumThreads(1);

    const po::option op_help = po::option("help", "help message").set_shortkey("h").set_narg(0);
    const po::option op_debug = po::option("debug", "debug option").set_narg(0);
    const po::option op_version = po::option("version", "program version").set_narg(0);
    const po::option op_extraKey = po::option("extraKey", "multiple extraKey to index").set_composing();
    const po::option op_input_config = po::option("input.config", "cdmGribReaderConfig as used by later calls. Using the config already during indexing will make sure that extraKeys and earthFigures correspond.").add_key("readerConfig").set_shortkey("c").set_default_value("");
    const po::option op_output_file = po::option("output.file", "output index file").add_key("outputFile").set_shortkey("o");
    const po::option op_output_type = po::option("output.type", "type of index to be written: grbml or gribfp");
    const po::option op_input_file = po::option("input.file", "input file(s)").add_key("inputFile").set_shortkey("i").set_composing();
    const po::option op_input_type = po::option("input.type", "type of input: grbml (single file) or grib (GRIB message file(s))").set_default_value("");
    const po::option op_input_optional = po::option("input.optional", "optional arguments for grib-files as in fimex, i.e. memberRegex: , memberName: pairs").set_composing();
    const po::option op_append_file = po::option("appendFile", "append output new index to a grbml-file").set_shortkey("a");

    po::option_set options;
    options
        // clang-format off
        << op_help
        << op_debug
        << op_version
        << op_extraKey
        << op_input_config
        << op_output_file
        << op_output_type
        << op_input_file
        << op_input_type
        << op_input_optional
        << op_append_file
        ;
        // clang-format on

    // read the options
    po::string_v positional;
    po::value_set vm = po::parse_command_line(argc, args, options, positional);

    if (argc == 1 || vm.is_set(op_help)) {
        writeUsage(std::cout, options);
        return 0;
    }
    if (vm.is_set(op_debug)) {
        MetNoFimex::defaultLogLevel(MetNoFimex::Logger::DEBUG);
    }
    if (vm.is_set(op_version)) {
        std::cout << "fiIndexGribs version " << fimexVersion() << std::endl;
        return 0;
    }

    std::vector<std::string> inputs;
    if (vm.is_set(op_input_file))
        inputs = vm.values(op_input_file);
    inputs.insert(inputs.end(), positional.begin(), positional.end());

    std::vector<std::string> extraKeys;
    if (vm.is_set(op_extraKey)) {
        extraKeys = vm.values(op_extraKey);
    }

    std::vector<std::string> members;
    if (vm.is_set(op_input_optional)) {
        members = vm.values(op_input_optional);
    }

    const std::string& readerConfig = vm.value(op_input_config);

    const auto op_append = vm.is_set(op_append_file);
    const auto op_output = vm.is_set(op_output_file);
    if (op_append && op_output) {
        std::cerr << "Only one of"
                  << " --" << op_append_file.key() << " and --" << op_output_file.key() << " may be specified." << std::endl;
        writeUsage(std::cerr, options);
        return 1;
    }

    auto ca = createDefaultChunkReaderFactory();
    if (op_append) {
        if (vm.is_set(op_output_type) && vm.value(op_output_type) != FILETYPE_GRBML) {
            std::cerr << "Appending is only supported for " << FILETYPE_GRBML << " indices." << std::endl;
            writeUsage(std::cerr, options);
            return 1;
        }
        if (inputs.size() != 1) {
            std::cerr << "When appending, exactly one input file must be specified." << std::endl;
            writeUsage(std::cerr, options);
            return 1;
        }
        const auto& appendFile = vm.value(op_append_file);
        appendToGrbml(ca, inputs.front(), appendFile, extraKeys, readerConfig, members);
    } else {
        if (inputs.empty()) {
            std::cerr << "missing input file(s)." << std::endl;
            writeUsage(std::cerr, options);
            return 1;
        }

        std::string output_type;
        if (vm.is_set(op_output_type)) {
            output_type = vm.value(op_output_type);
        }

        std::string output_file;
        if (vm.is_set(op_output_file)) {
            output_file = vm.value(op_output_file);
            if (!vm.is_set(op_output_type)) {
                if (ends_with(output_file, FILETYPE_GRBML)) {
                    output_type = FILETYPE_GRBML;
                } else if (ends_with(output_file, FILETYPE_GRBFP)) {
                    output_type = FILETYPE_GRBFP;
                }
            }
        } else {
            output_type = FILETYPE_GRBML;
            output_file = inputs.front() + "." + output_type;
        }

        if (output_type == FILETYPE_GRBML) {
            if (vm.is_set(op_input_type) && isGribType(vm.value(op_input_type))) {
                std::cerr << "Only GRIB message input is supported for writing " << FILETYPE_GRBML << " index." << std::endl;
                return 1;
            }
            createGrbmlFromGRIB(ca, inputs, output_file, extraKeys, readerConfig, members);
#ifdef HAVE_PROTOBUF
        } else if (output_type == FILETYPE_GRBFP) {
            if (inputs.size() == 1 && ((vm.is_set(op_input_type) && vm.value(op_input_type) == FILETYPE_GRBML) || ends_with(inputs.front(), FILETYPE_GRBML))) {
                // create protobuf index from a grbml index
                createGribfpFromGrbml(ca, inputs.front(), output_file, extraKeys, readerConfig, members);
            } else if (!vm.is_set(op_input_type) || isGribType(vm.value(op_input_type))) {
                // create protobuf index from a set of GRIB messages
                createGrbfpFromGRIB(ca, inputs, output_file, extraKeys, readerConfig, members);
            } else {
                std::cerr << "Unsupported index input type '" << vm.value(op_input_type) << "' or input count for " << FILETYPE_GRBFP << " index." << std::endl;
                writeUsage(std::cerr, options);
                return 1;
            }
#endif // HAVE_PROTOBUF
        } else {
            std::cerr << "Unknown index output type '" << output_type << "'." << std::endl;
            writeUsage(std::cerr, options);
            return 1;
        }
    }
    return 0;
}
