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

#ifndef METGM_CDMVARIABLEPROFILE_H
#define METGM_CDMVARIABLEPROFILE_H

// internals
//
#include "MetGmTags.h"
#include "MetGmDimensionsTag.h"

//fimex
//
#include "fimex/CDMVariable.h"

// standard
//
#include <memory>
#include <set>
#include <string>

namespace MetNoFimex {

    struct MetGmCDMVariableProfile {

        MetGmCDMVariableProfile(short pid, const std::string& cdmName, std::shared_ptr<MetGmTags> tags)
            : p_id_(pid)
            , cdmName_(cdmName)
            , pTags_(tags)
        {
        }

        bool operator<(const MetGmCDMVariableProfile& profile) const {return cdmName_ < profile.cdmName_;}

        short hd() const { return pTags_->hd(); }

        short                         p_id_;
        std::string                   cdmName_;
        std::string                   standardName_;
        std::string                   units_;
        std::string                   zDimensionName_;
        std::string                   addOffset_;
        std::string                   scaleFactor_;
        std::string                   fillValue_;
        std::shared_ptr<MetGmTags> pTags_;
    };

    typedef std::set<MetGmCDMVariableProfile> cdm_configuration;

    struct MetGmCDMVariableProfileByPId
    {
        bool operator()(const MetGmCDMVariableProfile& a, const MetGmCDMVariableProfile& b) const { return a.p_id_ < b.p_id_; }
        bool operator()(cdm_configuration::const_iterator a, cdm_configuration::const_iterator b) const { return this->operator()(*a, *b); }
    };

    struct MetGmCDMVariableProfileEqPId
    {
        short p_id_;
        MetGmCDMVariableProfileEqPId(short p_id)
            : p_id_(p_id)
        {
        }
        bool operator()(const MetGmCDMVariableProfile& a) const { return a.p_id_ == p_id_; }
    };

    struct MetGmCDMVariableProfileEqName
    {
        const std::string& cdmName_;
        MetGmCDMVariableProfileEqName(const std::string& cdmName)
            : cdmName_(cdmName)
        {
        }
        bool operator()(const MetGmCDMVariableProfile& a) const { return a.cdmName_ == cdmName_; }
    };

    std::vector<cdm_configuration::iterator> sorted_by_pid(cdm_configuration& xc);

    } // namespace MetNoFimex

#endif // METGM_CDMVARIABLEPROFILE_H
