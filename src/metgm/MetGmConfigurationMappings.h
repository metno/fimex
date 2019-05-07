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

#ifndef METGM_CONFIGURATIONMAPPINGS_HPP
#define METGM_CONFIGURATIONMAPPINGS_HPP

// fimex
//
#include "fimex/CDMVariable.h"


// libxml2
#include <libxml/tree.h>
#include <libxml/xpath.h>

// standard
//
#include <memory>
#include <set>
#include <string>

namespace MetNoFimex {

    struct MetGmConfigurationMappings {

        short                          p_id_;
        std::string                    cdmName_;
        std::string                    standardName_;
        std::string                    units_;
        std::string                    scaleFactor_;
        std::string                    addOffset_;
        std::string                    fillValue_;

        bool operator<(const MetGmConfigurationMappings& entry)const{return cdmName_ < entry.cdmName_;}

        MetGmConfigurationMappings(short p_id, const std::string name) : p_id_(p_id), cdmName_(name) { }
    };

    typedef std::set<MetGmConfigurationMappings> xml_configuration;

    struct MetGmConfigurationMappingsByPId
    {
        bool operator()(const MetGmConfigurationMappings& a, const MetGmConfigurationMappings& b) const { return a.p_id_ < b.p_id_; }
        bool operator()(xml_configuration::const_iterator a, xml_configuration::const_iterator b) const { return this->operator()(*a, *b); }
    };

    struct MetGmConfigurationMappingsEqPId
    {
        short p_id_;
        MetGmConfigurationMappingsEqPId(short p_id)
            : p_id_(p_id)
        {
        }
        bool operator()(const MetGmConfigurationMappings& a) const { return a.p_id_ == p_id_; }
    };

    std::vector<xml_configuration::const_iterator> sorted_by_pid(const xml_configuration& xc);

} // namespace MetNoFimex

#endif
