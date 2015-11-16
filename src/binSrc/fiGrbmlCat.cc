/*
 * Fimex, fiGrbmlCat.cc
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
 *  Created on: Oct 31, 2015
 *      Author: heikok
 */

#include <string>
#include <fstream>
#include <iostream>
#include <libxml/xmlreader.h>
#include <boost/shared_ptr.hpp>
#include <boost/program_options.hpp>
#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>

using namespace std;
namespace po = boost::program_options;
namespace io = boost::iostreams;

static void writeUsage(ostream& out, const po::options_description& options) {
    out << "usage: fiGrbmlCat --outputFile=OUTFILE.grbml file1.grml [file2.grbml ...] [--inputFile=fileX.grbml] " << endl;
    out << endl;
    out << options << endl;
}


int printNode(xmlTextReaderPtr reader, ostream& os) {
    const xmlChar* name;
    const xmlChar* value;
    bool isEmpty = xmlTextReaderIsEmptyElement(reader); // needs to be read before reading attributes!

    const xmlChar* nodeName = xmlTextReaderConstName(reader);
//    cerr << "printNode: " << nodeName << endl;
    os << "<" << nodeName;

    while (xmlTextReaderMoveToNextAttribute(reader) == 1) {
        name = xmlTextReaderName(reader);
        value = xmlTextReaderValue(reader);
        os << " " << name << "=\"" << value << "\"";
    }
    if (isEmpty) {
        os << " />";
        return 1;
    } else {
        os << ">";
    }

    int ret = xmlTextReaderRead(reader);
    // walk through nodes until end-element
    while (ret == 1) {
        int type = xmlTextReaderNodeType(reader);
//        cerr << "type: " << type << " " << xmlTextReaderConstName(reader) << " " << xmlTextReaderIsEmptyElement(reader) << endl;
        switch (type) {
        case XML_READER_TYPE_ELEMENT: printNode(reader, os);break;
        case XML_READER_TYPE_END_ELEMENT: {
                os << "</" << nodeName << ">";
                return ret;
        }
        default: break;
        }
        ret = xmlTextReaderRead(reader);
    }

    return ret;
}

static bool first = true;
void grbmlExtract(const string& fileName, ostream& os)
{
    xmlTextReaderPtr reader = xmlReaderForFile(fileName.c_str(), NULL, 0);
    if (reader != NULL) {
        boost::shared_ptr<xmlTextReader> cleanupReader(reader, xmlFreeTextReader);
        const xmlChar* name;
        int ret = xmlTextReaderRead(reader);
        while (ret == 1) {
            //int depth = xmlTextReaderDepth(reader);
            int type = xmlTextReaderNodeType(reader);
            switch (type) {
            case XML_READER_TYPE_ELEMENT: {
                name = xmlTextReaderConstName(reader);
                if (name == NULL) name = reinterpret_cast<const xmlChar*>("");
                if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribFileIndex"))) {
                    if (first) {
                        string url = string(reinterpret_cast<const char*>(xmlTextReaderGetAttribute(reader, reinterpret_cast<const xmlChar*>("url"))));
                        os << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
                        os << "<gribFileIndex url=\"" << url << "\" xmlns=\"http://www.met.no/schema/fimex/gribFileIndex\">" << endl;
                        first = false;
                    }
                } else if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribMessage"))) {
                    printNode(reader, os);
                }
                break;
            }
            case XML_READER_TYPE_END_ELEMENT: {
                name = xmlTextReaderConstName(reader);
                if (name == NULL) name = reinterpret_cast<const xmlChar*>("");
                if (xmlStrEqual(name, reinterpret_cast<const xmlChar*>("gribFileIndex"))) {
                    ret = 0;
                    continue; // leave while loop
                } else {
                    cerr << "unknown node: '" << name << "'" << endl;
                }
                break;
            }
            default: break; // only element nodes of interest
            }
            ret = xmlTextReaderRead(reader);
        }
        os << "</gribFileIndex>" << endl;
        if (ret != 0) {
            cerr <<  fileName << ": failed to parse" << endl;
        }
    }
}

int
main(int argc, char* args[])
{
    po::options_description options("options");
    options.add_options()
        ("outputFile,o", po::value<string>(), "output grbml")
        ("inputFile,i", po::value<vector<string> >()->composing(), "input grbml, possibly many")
        ("compress,c", "enable gzip compression")
        ;
    po::positional_options_description posi;
    posi.add("inputFile", -1);

    // read the options
    po::variables_map vm;
    po::store(po::command_line_parser(argc, args).
              options(options).positional(posi).run(), vm);
    po::notify(vm);

    // open stream before filter, required for closing order
    ofstream realOutStream;
    io::filtering_ostream outStream;
    if (vm.count("compress") != 0) {
        // simple compression gives already small size
        outStream.push(io::gzip_compressor(io::zlib::best_speed));
    }
    if (vm["outputFile"].as<string>() != "-") {
        realOutStream.open(vm["outputFile"].as<string>().c_str(), std::ios::binary|std::ios::out);
        outStream.push(realOutStream);
    } else {
        outStream.push(cout);
    }


    if (vm.count("inputFile") > 0) {
        vector<string> files = vm["inputFile"].as<vector<string> >();
        for (size_t i = 0; i < files.size(); ++i) grbmlExtract(files[i], outStream);
    } else {
        writeUsage(cerr, options);
        return 1;
    }
    return 0;
}
