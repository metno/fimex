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

// internals
//
#include "MetGmHandlePtr.h"

// boost
//
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    class MetGmFileHandlePtr;
    class MetGmVersion;

    class MetGmGroup3Ptr {
    public:

        static boost::shared_ptr<MetGmGroup3Ptr> createMetGmGroup3PtrForWriting(boost::shared_ptr<MetGmHandlePtr> pHandle);
        static boost::shared_ptr<MetGmGroup3Ptr> createMetGmGroup3PtrForReading(boost::shared_ptr<MetGmHandlePtr> pHandle);

        ~MetGmGroup3Ptr();

        inline operator mgm_group3* () { return group3_; }
        inline operator const mgm_group3* () const { return group3_; }

        boost::shared_ptr<MetGmHandlePtr> mgmHandle();

        int p_id() const;
        int nz()   const;
        int nx()   const;
        int ny()   const;
        int nt()   const;
        float dx() const;
        float dy() const;
        float dt() const;
        float cx() const;
        float cy() const;
        short pr() const;
        short pz() const;

        int set_p_id(int p_id);
        int set_nz(int nz);
        int set_nx(int nx);
        int set_ny(int ny);
        int set_nt(int nt);
        int set_dx(float dx);
        int set_dy(float dy);
        int set_dt(float dt);
        int set_cx(float cx);
        int set_cy(float cy);
        int set_pr(short pr);
        int set_pz(short pz);

        int reset();

        void dump();

        bool eq(boost::shared_ptr<MetGmGroup3Ptr> &rhs) const;
        bool neq(boost::shared_ptr<MetGmGroup3Ptr> &rhs) const;

    private:
        MetGmGroup3Ptr(boost::shared_ptr<MetGmHandlePtr> pHandle);

        // not owning
        boost::shared_ptr<MetGmHandlePtr> pHandle_;

        // owning
        mgm_group3* group3_;
    };


}

#endif // METGM_GROUP3PTR_H
