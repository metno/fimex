/*
 * Fimex
 *
 * (C) Copyright 2011, met.no
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

#ifndef METGM_VERSION_H
#define METGM_VERSION_H

// METGM C library
//
#include "metgm.h"

// Fimex
//
#include "fimex/XMLDoc.h"

// libxml2
#include <libxml/tree.h>
#include <libxml/xpath.h>

// standard
//
#include <memory>
#include <string>

namespace MetNoFimex {

    #define VERSION   "metgm_version"

    class MetGmVersion {
    public:
        static std::shared_ptr<MetGmVersion> createMetGmVersion(mgm_version version)
        {
            std::shared_ptr<MetGmVersion> pVersion = std::shared_ptr<MetGmVersion>(new MetGmVersion(version));
            return pVersion;
        }

        static std::shared_ptr<MetGmVersion> createMetGmVersion(const std::unique_ptr<XMLDoc>& doc)
        {

            mgm_version version = MGM_Edition1;

            if(doc.get()) {
                xmlXPathObject_p xpathObj = doc->getXPathObject("/metgm/meta_data/attribute");
                xmlNodeSetPtr nodes = xpathObj->nodesetval;
                size_t size = (nodes) ? nodes->nodeNr : 0;
                for (size_t i = 0; i < size; ++i) {
                    xmlNodePtr node = nodes->nodeTab[i];
                    std::string attributeName = getXmlProp(node, "name");
                    if(attributeName == std::string(VERSION)) {
                        std::string strVersion = getXmlProp(node, "value");
                        if(strVersion.find("Edition_2") != std::string::npos) {
                            version = MGM_Edition2;
                        }
                    }
                }
            }
            return std::shared_ptr<MetGmVersion>(new MetGmVersion(version));
        }

        inline std::string getAsString()
        {
            if(version_ == MGM_Edition1)
                return std::string("STANAG 6022 Edition 1");
            else if(version_ == MGM_Edition2)
                return std::string("STANAG 6022 Edition 2");
            else
                return std::string("Unknown STANAG 6022 Edition");
        }

        inline bool operator == (const MetGmVersion &rh) const
        {
            return version_ == rh.version_;
        }

        inline bool operator == (const mgm_version rh_version) const
        {
            return version_ == rh_version;
        }

        inline operator mgm_version ()
        {
            return version_;
        }

        inline mgm_version as_mgm_version()
        {
            return version_;
        }

    private:
        mgm_version version_;

        explicit MetGmVersion(mgm_version version) : version_(version) { }
    };
}

#endif // METGM_VERSION_H
