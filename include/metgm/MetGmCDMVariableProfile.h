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

//fimex
//
#include "fimex/CDMVariable.h"

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

    struct MetGmCDMVariableProfile {

        MetGmCDMVariableProfile(short pid, const CDMVariable* pVar, boost::shared_ptr<MetGmTags> tags)
            : p_id_(pid), pVariable_(pVar), pTags_(tags) {}

        const CDMVariable* variable() { return pVariable_; }

        bool operator<(const MetGmCDMVariableProfile& profile) const{
            return p_id_ < profile.p_id_;
        }

        short hd() const { return pTags_->dimTag()->asShort(); }

        short                         p_id_;
        const CDMVariable*            pVariable_;
        boost::shared_ptr<MetGmTags>  pTags_;

    };

    struct p_id_index {};
    struct hd_index   {};

//    typedef boost::multi_index::multi_index_container<
//      MetGmCDMVariableProfile,
//      boost::multi_index::indexed_by<
//          boost::multi_index::ordered_non_unique <
//              boost::multi_index::composite_key <
//                  METGM_ZProfile,
//                  boost::multi_index::member<METGM_ZProfile,short,&METGM_ZProfile::pr_>,
//                  boost::multi_index::member<METGM_ZProfile,int,&METGM_ZProfile::pid_>
//              >
//          >
//          >
//      > metgm_profile_set;

    typedef boost::multi_index::multi_index_container<
      MetGmCDMVariableProfile,
      boost::multi_index::indexed_by<
        boost::multi_index::ordered_unique<
          boost::multi_index::identity<MetGmCDMVariableProfile
          >
        >,
        boost::multi_index::hashed_non_unique<
          boost::multi_index::tag<p_id_index>,
          boost::multi_index::member<
            MetGmCDMVariableProfile, short, &MetGmCDMVariableProfile::p_id_
          >
        >,
        boost::multi_index::hashed_non_unique<
          boost::multi_index::member<
            MetGmCDMVariableProfile, const CDMVariable*, &MetGmCDMVariableProfile::pVariable_
          >
        >,
        boost::multi_index::ordered_non_unique<
          boost::multi_index::tag<hd_index>,
          boost::multi_index::const_mem_fun<
           MetGmCDMVariableProfile, short, &MetGmCDMVariableProfile::hd
          >,
          std::greater<short>
        >
      >
    > profile_multi;

    typedef profile_multi::index<p_id_index>::type pidIndex;

}

#endif // METGM_CDMVARIABLEPROFILE_H
