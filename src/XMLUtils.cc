/*
 * Fimex
 *
 * (C) Copyright 2019-2026, met.no
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

#include "fimex/XMLUtils.h"

#include "fimex/StringUtils.h"
#include "fimex/XMLInputFile.h"
#include "fimex/XMLInputString.h"

#include <cstdlib>

namespace MetNoFimex {

std::string XmlCharPtr::to_string() const
{
    return std::string(to_cc());
}

float XmlCharPtr::to_float() const
{
    return atof(to_cc());
}

double XmlCharPtr::to_double() const
{
    return atof(to_cc());
}

long XmlCharPtr::to_long() const
{
    return atol(to_cc());
}

long long XmlCharPtr::to_longlong() const
{
    return atoll(to_cc());
}

int XmlCharPtr::cmp(const char* text) const
{
    return xmlStrcmp(p_, reinterpret_cast<const xmlChar*>(text));
}

size_t XmlCharPtr::len() const
{
    return xmlStrlen(p_);
}

XPathNodeSet::XPathNodeSet(const XMLDoc& doc, const std::string& xpath, xmlNodePtr node)
    : XPathNodeSet(doc.getXPathObject(xpath, node))
{
}

XPathNodeSet::XPathNodeSet(xmlXPathObject_p xpo)
    : xpo_(xpo)
    , nodes_(xpo_ ? xpo_->nodesetval : nullptr)
    , size_(nodes_ ? nodes_->nodeNr : 0)
{
}

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

namespace {
bool needsXmlEscape(const std::string& input)
{
    return input.find_first_of("&<>\"'") != std::string::npos;
}
} // namespace

std::string escapeXmlString(const std::string& input)
{
    if (needsXmlEscape(input)) {
        std::ostringstream output;
        escapeXmlToStream(output, input);
        return output.str();
    } else {
        return input;
    }
}

void escapeXmlToStream(std::ostream& output, const std::string& input)
{
    if (needsXmlEscape(input)) {
        for (char c : input) {
            switch (c) {
            case '&':
                output << "&amp;";
                break;
            case '<':
                output << "&lt;";
                break;
            case '>':
                output << "&gt;";
                break;
            case '"':
                output << "&quot;";
                break;
            case '\'':
                output << "&apos;";
                break;
            default:
                output.put(c); // Write the character directly to the stream
                break;
            }
        }
    } else {
        output << input;
    }
}

} // namespace MetNoFimex
