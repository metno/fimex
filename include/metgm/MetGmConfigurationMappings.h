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


// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/composite_key.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>

// standard
//
#include <string>

namespace MetNoFimex {

    struct MetGmConfigurationMappings {

        short                          p_id_;
        std::string                    cdmName_;
        std::string                    standardName_;
        std::string                    units_;
        boost::shared_ptr<float>       fillValue_;

        bool operator<(const MetGmConfigurationMappings& entry)const{return cdmName_ < entry.cdmName_;}

        void setFillValue(const float fillValue)
        {
            fillValue_ = boost::shared_ptr<float>(new float(fillValue));
        }

        MetGmConfigurationMappings(short p_id, const std::string name) : p_id_(p_id), cdmName_(name) { }
    };

    struct xml_pid_index {};
    struct xml_name_index {};

    typedef boost::multi_index::multi_index_container<
      MetGmConfigurationMappings,
      boost::multi_index::indexed_by<
        boost::multi_index::ordered_unique<
          boost::multi_index::identity<MetGmConfigurationMappings>
        >,
        boost::multi_index::ordered_non_unique<
          boost::multi_index::tag<xml_pid_index>,
          boost::multi_index::member<
            MetGmConfigurationMappings, short, &MetGmConfigurationMappings::p_id_
          >
        >,
        boost::multi_index::hashed_unique<
          boost::multi_index::tag<xml_name_index>,
            boost::multi_index::member<
              MetGmConfigurationMappings, std::string, &MetGmConfigurationMappings::cdmName_ >
        >
      >
    > xml_configuration;

    typedef xml_configuration::index<xml_pid_index>::type    xmlPidView;
    typedef xml_configuration::index<xml_name_index>::type   xmlNameView;
}

#endif
