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

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"

#include "fimex/Logger.h"
#include "fimex/StringUtils.h"
#include "fimex/ThreadPool.h"
#include "fimex/XMLInputFile.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "GribCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "GribFileIndex.h"

#include <mi_programoptions.h>

#include <grib_api.h>

#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>

namespace po = miutil::program_options;
using namespace std;
using namespace MetNoFimex;

namespace {

static Logger_p logger = getLogger("fiIndexGribs");

void writeUsage(ostream& out, const po::option_set& options)
{
    out << "usage: fiIndexGribs -o/--outputFile GRBML_NAME [-c/--readerConfig gribreaderconfig.xml] [-i] gribFile [[-i] gribFile2 [...]]" << endl;
    out << "  When creating, one or more input file(s) must be specified." << endl;
    out << "usage: fiIndexGribs -a/--appendFile GRBML_NAME [-c/--readerConfig gribreaderconfig.xml] [-i] gribFile" << endl;
    out << "  When appending, exactly one input file must be specified." << endl;
    out << endl;
    options.help(out);
}

void initOptions(std::map<std::string, std::string>& options,
                 std::vector<std::pair<std::string, std::regex>>& members,
                 vector<string> extraKeys, string config, vector<string> memberOptions)
{
    if (!config.empty()) {
        using namespace MetNoFimex;
        XMLDoc_p doc = GribCDMReader::initXMLConfig(XMLInputFile(config));
        options["earthfigure"] = GribCDMReader::getConfigEarthFigure(doc);
        extraKeys.push_back(GribCDMReader::getConfigExtraKeys(doc));
    }
    if (!extraKeys.empty()) {
        options["extraKeys"] = MetNoFimex::join(extraKeys.begin(), extraKeys.end(), ",");
    }
    if (!memberOptions.empty()) {
        vector<pair<string, string> > memberStrings;
        vector<string> files;
        MetNoFimex::parseGribArgs(memberOptions, memberStrings, files);
        for (vector<pair<string, string> >::const_iterator memIt = memberStrings.begin(); memIt != memberStrings.end(); ++memIt) {
            members.push_back(make_pair(memIt->first, std::regex(memIt->second)));
        }
    }
}

struct GribIndexWriter {
    GribIndexWriter(const string& output, const std::string& url);
    ~GribIndexWriter();

    std::ofstream os;
};

GribIndexWriter::GribIndexWriter(const std::string& output, const std::string& url)
    : os(output, std::ios::binary)
{
    os << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
    os << "<gribFileIndex url=\"" << url << "\" xmlns=\"http://www.met.no/schema/fimex/gribFileIndex\">" << endl;
}

GribIndexWriter::~GribIndexWriter()
{
    os << "</gribFileIndex>" << endl;
}

void indexGribs(const std::vector<std::string>& inputs, const std::string& output, vector<string> extraKeys, string config,
                vector<string> memberOptions)
{
    std::map<std::string, std::string> options;
    std::vector<std::pair<std::string, std::regex>> members;
    initOptions(options, members, extraKeys, config, memberOptions);

    GribIndexWriter w(output, "file:" + inputs.front());
    for (const auto& input : inputs) {
        LOG4FIMEX(logger, Logger::DEBUG, "Start processing '" << input << "'");
        const GribFileIndex gfi(input, "", members, options);
        for (const auto& gfm : gfi.listMessages())
            w.os << gfm;
    }
}

void indexGribAppend(const std::string& input, const std::string& append, vector<string> extraKeys, string config, vector<string> memberOptions)
{
    std::map<std::string, std::string> options;
    std::vector<std::pair<std::string, std::regex>> members;
    initOptions(options, members, extraKeys, config, memberOptions);

    LOG4FIMEX(logger, Logger::DEBUG, "Reading '" << append << "' and processing '" << input << "'");
    const GribFileIndex gfi(input, append, members, options);

    LOG4FIMEX(logger, Logger::DEBUG, "Writing to '" << append << "'");
    GribIndexWriter w(append, "file:" + input);
    for (const auto& gfm : gfi.listMessages())
        w.os << gfm;
}

} // namespace

int main(int argc, char* args[])
{
    // only use one thread
    mifi_setNumThreads(1);

    const po::option op_help = po::option("help", "help message").set_shortkey("h").set_narg(0);
    const po::option op_debug = po::option("debug", "debug option").set_narg(0);
    const po::option op_version = po::option("version", "program version").set_narg(0);
    const po::option op_extraKey = po::option("extraKey", "multiple extraKey to index").set_composing();
    const po::option op_readerConfig = po::option("readerConfig", "cdmGribReaderConfig as used by later calls. Using the config already during indexing will make sure that extraKeys and earthFigures correspond.").set_shortkey("c");
    const po::option op_outputFile = po::option("outputFile", "output grbml file").set_shortkey("o");
    const po::option op_inputFile = po::option("inputFile", "input gribFile").set_shortkey("i").set_composing();
    const po::option op_input_optional = po::option("input.optional", "optional arguments for grib-files as in fimex, i.e. memberRegex: , memberName: pairs").set_composing();
    const po::option op_appendFile = po::option("appendFile", "append output new index to a grbml-file").set_shortkey("a");

    po::option_set options;
    options
        << op_help
        << op_debug
        << op_version
        << op_extraKey
        << op_readerConfig
        << op_outputFile
        << op_inputFile
        << op_input_optional
        << op_appendFile
        ;

    // read the options
    po::string_v positional;
    po::value_set vm = po::parse_command_line(argc, args, options, positional);

    if (argc == 1 || vm.is_set(op_help)) {
        writeUsage(cout, options);
        return 0;
    }
    if (vm.is_set(op_debug)) {
        MetNoFimex::defaultLogLevel(MetNoFimex::Logger::DEBUG);
    }
    if (vm.is_set(op_version)) {
        cout << "fiIndexGribs version " << fimexVersion() << endl;
        return 0;
    }

    vector<string> inputs;
    if (vm.is_set(op_inputFile))
        inputs = vm.values(op_inputFile);
    inputs.insert(inputs.end(), positional.begin(), positional.end());

    std::string outputFile;
    if (vm.is_set(op_outputFile))
        outputFile = vm.value(op_outputFile);
    else if (!inputs.empty())
        outputFile = inputs.front() + ".grbml";

    vector<string> extraKeys;
    if (vm.is_set(op_extraKey)) {
        extraKeys = vm.values(op_extraKey);
    }
    string readerConfig("");
    if (vm.is_set(op_readerConfig)) {
        readerConfig = vm.value(op_readerConfig);
    }
    vector<string> members;
    if (vm.is_set(op_input_optional)) {
        members = vm.values(op_input_optional);
    }
    std::string appendFile;
    if (vm.is_set(op_appendFile)) {
        if (inputs.size() != 1) {
            cerr << "When appending, exactly one input file must be specified." << endl;
            writeUsage(cout, options);
            return 1;
        }
        outputFile = appendFile = vm.value(op_appendFile);
        indexGribAppend(inputs.front(), appendFile, extraKeys, readerConfig, members);
    } else {
        if (inputs.empty()) {
            cerr << "missing input file" << endl;
            writeUsage(cout, options);
            return 1;
        }
        indexGribs(inputs, outputFile, extraKeys, readerConfig, members);
    }
    return 0;
}
