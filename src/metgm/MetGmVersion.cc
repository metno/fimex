/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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

/**
 * Used as private/implementation class
 */

#include "MetGmVersion.h"

// Fimex
//
#include "MetGmCommentAttributeParser.h"
#include "fimex/XMLDoc.h"

// libxml2
#include <libxml/tree.h>
#include <libxml/xpath.h>

// standard
//

namespace MetNoFimex {

// static
std::shared_ptr<MetGmVersion> MetGmVersion::createMetGmVersion(mgm_version version)
{
    return std::shared_ptr<MetGmVersion>(new MetGmVersion(version)); // ctor is private, cannot use make_shared
}

// static
std::shared_ptr<MetGmVersion> MetGmVersion::createMetGmVersion(const std::unique_ptr<XMLDoc>& doc)
{
    mgm_version version = MGM_Edition1;

    if (doc.get()) {
        xmlXPathObject_p xpathObj = doc->getXPathObject("/metgm_config/meta_data/attribute");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        size_t size = (nodes) ? nodes->nodeNr : 0;
        for (size_t i = 0; i < size; ++i) {
            xmlNodePtr node = nodes->nodeTab[i];
            std::string attributeName = getXmlProp(node, "name");
            if (attributeName == VERSION) {
                std::string strVersion = getXmlProp(node, "value");
                if (strVersion.find("Edition_2") != std::string::npos) {
                    version = MGM_Edition2;
                }
            }
        }
    }
    return createMetGmVersion(version);
}

std::string MetGmVersion::getAsString() const
{
    if (version_ == MGM_Edition1)
        return std::string("STANAG 6022 Edition 1");
    else if (version_ == MGM_Edition2)
        return std::string("STANAG 6022 Edition 2");
    else
        return std::string("Unknown STANAG 6022 Edition");
}

} // namespace MetNoFimex
