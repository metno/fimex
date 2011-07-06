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

#ifndef METGM_GROUP3PTR_H
#define METGM_GROUP3PTR_H

// METGM C Lib
//
#include "metgm.h"

// internals
//
#include "MetGmHandlePtr.h"

// boost
//
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    class MetGmGroup3Ptr {
    public:

        static boost::shared_ptr<MetGmGroup3Ptr> createMetGmGroup3Ptr(const boost::shared_ptr<MetGmHandlePtr> pHandle) {
            boost::shared_ptr<MetGmGroup3Ptr> gp3 = boost::shared_ptr<MetGmGroup3Ptr>(new MetGmGroup3Ptr(pHandle));
            return gp3;
        }

        inline ~MetGmGroup3Ptr() { mgm_free_group3(group3_); }

        inline bool eq(boost::shared_ptr<MetGmGroup3Ptr> &rhs) const {

            if(this == rhs.get()) return true;

            if(
                   nz() == rhs->nz()
                && nx() == rhs->nx()
                && ny() == rhs->ny()
                && nt() == rhs->nt()
                && dx() == rhs->dx()
                && dy() == rhs->dy()
                && dt() == rhs->dt()
                && cx() == rhs->cx()
                && cy() == rhs->cy()
                && pr() == rhs->pr()
                && pz() == rhs->pz()
              )
                return true;
            else
                return false;
        }

        inline bool neq(boost::shared_ptr<MetGmGroup3Ptr> &rhs) const {
            return !(eq(rhs));
        }

        inline int reset() { return mgm_reset_group3(group3_); }

        inline operator mgm_group3* () { return group3_; }
        inline operator const mgm_group3* () const { return group3_; }

        inline boost::shared_ptr<MetGmHandlePtr> mgmHandle() const { return pHandle_;}

        inline int   p_id() { return mgm_get_p_id(group3_); }
        inline const int   p_id() const { return mgm_get_p_id(group3_); }
        inline int   nz()   const { return mgm_get_nz(group3_); }
        inline int   nx()   const { return mgm_get_nx(group3_); }
        inline int   ny()   const { return mgm_get_ny(group3_); }
        inline int   nt()   const { return mgm_get_nt(group3_); }
        inline float dx()   const { return mgm_get_dx(group3_); }
        inline float dy()   const { return mgm_get_dy(group3_); }
        inline float dt()   const { return mgm_get_dt(group3_); }
        inline float cx()   const { return mgm_get_cx(group3_); }
        inline float cy()   const { return mgm_get_cy(group3_); }
        inline short pr()   const { return mgm_get_pr(group3_); }
        inline short pz()   const { return mgm_get_pz(group3_); }

        inline int set_p_id(int p_id) { return mgm_set_p_id(group3_, p_id); }
        inline int set_nz(int nz)     { return mgm_set_nz(group3_, nz); }
        inline int set_nx(int nx)     { return mgm_set_nx(group3_, nx); }
        inline int set_ny(int ny)     { return mgm_set_ny(group3_, ny); }
        inline int set_nt(int nt)     { return mgm_set_nt(group3_, nt); }
        inline int set_dx(float dx)   { return mgm_set_dx(group3_, dx); }
        inline int set_dy(float dy)   { return mgm_set_dy(group3_, dy); }
        inline int set_dt(float dt)   { return mgm_set_dt(group3_, dt); }
        inline int set_cx(float cx)   { return mgm_set_cx(group3_, cx); }
        inline int set_cy(float cy)   { return mgm_set_cy(group3_, cy); }
        inline int set_pr(short pr)   { return mgm_set_pr(group3_, pr); }
        inline int set_pz(short pz)   { return mgm_set_pz(group3_, pz); }

        void dump();

    private:
        inline MetGmGroup3Ptr(const boost::shared_ptr<MetGmHandlePtr> pHandle)
            : pHandle_(pHandle) { group3_ = mgm_new_group3(); }

        // not owning
        const boost::shared_ptr<MetGmHandlePtr> pHandle_;

        // owning
        mgm_group3* group3_;
    };


}

#endif // METGM_GROUP3PTR_H
