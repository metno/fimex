/*
 * Fimex
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

#include "fimex/ProtobufUtils.h"

#include "fimex/FileUtils.h"
#include "fimex/XMLUtils.h"

namespace MetNoFimex {

std::string protobufIndexRootPath(const std::string& indexFileName, const XMLInput& configXML)
{
    if (const auto doc = configXML.getXMLDoc()) {
        XPathNodeSet nodes(doc, "/cdm_fimex_index_reader_config/root_path");
        if (nodes.size() == 1) {
            return XmlCharPtr(xmlNodeGetContent(nodes[0])).to_string();
        }
    }
    return removeFilename(indexFileName);
}

} // namespace MetNoFimex
