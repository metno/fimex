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

// internals
//
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"

// standard
//
#include <iostream>

namespace MetNoFimex {

    boost::shared_ptr<MetGmGroup3Ptr> MetGmGroup3Ptr::createMetGmGroup3PtrForWriting(boost::shared_ptr<MetGmHandlePtr>& pHandle)
    {
        boost::shared_ptr<MetGmGroup3Ptr> gp3 = boost::shared_ptr<MetGmGroup3Ptr>(new MetGmGroup3Ptr(pHandle));
        return gp3;
    }

    boost::shared_ptr<MetGmGroup3Ptr> MetGmGroup3Ptr::createMetGmGroup3PtrForReading(boost::shared_ptr<MetGmHandlePtr>& pHandle)
    {
        boost::shared_ptr<MetGmGroup3Ptr> pg3 = boost::shared_ptr<MetGmGroup3Ptr>(new MetGmGroup3Ptr(pHandle));
        mgm_read_group3(*pg3->pHandle_->fileHandle(), *pg3->mgmHandle(), *pg3);
        return pg3;
    }

    bool MetGmGroup3Ptr::eq(boost::shared_ptr<MetGmGroup3Ptr> &rhs) const
    {
        if(this == rhs.get()) return true;

        if(        nz() == rhs->nz()
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

    bool MetGmGroup3Ptr::neq(boost::shared_ptr<MetGmGroup3Ptr> &rhs) const
    {
        return !(eq(rhs));
    }

    void MetGmGroup3Ptr::dump() {
        std::cerr << "dumping group3 [START]" << std::endl
                  << "p_id = " << p_id()      << std::endl
                  << "nz = "   << nz()        << std::endl
                  << "nx = "   << nx()        << std::endl
                  << "ny = "   << ny()        << std::endl
                  << "nt = "   << nt()        << std::endl
                  << "dx = "   << dx()        << std::endl
                  << "dy = "   << dy()        << std::endl
                  << "dt = "   << dt()        << std::endl
                  << "cx = "   << cx()        << std::endl
                  << "cy = "   << cy()        << std::endl
                  << "pr = "   << pr()        << std::endl
                  << "pz = "   << pz()        << std::endl
                  << "dumping group3 [END]"   << std::endl;
    }

}
