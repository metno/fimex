/*
 * Fimex, pyfimex0_NcmlCDMReader.cc
 *
 * (C) Copyright 2019, met.no
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

#include "fimex/NcmlCDMReader.h"
#include "fimex/StringUtils.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInputDoc.h"
#include "fimex/XMLInputFile.h"
#include "fimex/XMLInputString.h"

#include "pyfimex0_helpers.h"

namespace MetNoFimex {
typedef std::shared_ptr<NcmlCDMReader> NcmlCDMReader_p;
} // namespace MetNoFimex

using namespace MetNoFimex;
namespace py = pybind11;

namespace {

// FIXME this is copied from CDMFileReaderFactory.cc

XMLInputDoc createXMLInput(const XMLInput& xi)
{
    return XMLInputDoc(xi.id(), xi.getXMLDoc());
}

XMLInputDoc createXMLInput(const std::string& configXML)
{
    if (configXML.empty()) {
        return XMLInputDoc("", XMLDoc_p());
    } else if (starts_with(configXML, "<?xml ")) {
        return createXMLInput(XMLInputString(configXML));
    } else {
        return createXMLInput(XMLInputFile(configXML));
    }
}

NcmlCDMReader_p createNcmlReader1(const std::string& configXML)
{
    XMLInputDoc configDoc = createXMLInput(configXML);
    return std::make_shared<NcmlCDMReader>(configDoc);
}

NcmlCDMReader_p createNcmlReader2(CDMReader_p reader, const std::string& configXML)
{
    XMLInputDoc configDoc = createXMLInput(configXML);
    return std::make_shared<NcmlCDMReader>(reader, configDoc);
}

} // namespace

void pyfimex0_NcmlCDMReader(py::module m)
{
    py::class_<NcmlCDMReader, NcmlCDMReader_p, CDMReader>(m, "_NcmlCDMReader");

    m.def("createNcmlReader", createNcmlReader1,
          "Create a NcML reader using sources specified in the ncml file or inline xml.");
    m.def("createNcmlReader", createNcmlReader2,
          "Create a NcML reader using the specified source and the ncml file or inline xml.");
}
