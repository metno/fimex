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
#include "MetGmGroup2Ptr.h"
#include "MetGmHandlePtr.h"

// METGM C API
//
#include "metgm.h"

namespace MetNoFimex {

    boost::shared_ptr<MetGmGroup2Ptr> MetGmGroup2Ptr::createMetGmGroup2PtrForReading(boost::shared_ptr<MetGmHandlePtr>& pMgmHandle)
    {
        boost::shared_ptr<MetGmGroup2Ptr> gp2 = boost::shared_ptr<MetGmGroup2Ptr>(new MetGmGroup2Ptr);

        gp2->pHandle_ = pMgmHandle;
        gp2->totalNumberOfParameters_ = mgm_get_number_of_params(*pMgmHandle);

        if(*pMgmHandle->version() == MGM_Edition2) {
            gp2->numberOfDistinctParameters_ = mgm_get_number_of_dist_params(*pMgmHandle);
            for(int index = 0; index < gp2->numberOfDistinctParameters_; ++index) {
                Group2Entry entry;

                entry.pid_ = mgm_get_param_id (*pMgmHandle, index);
                entry.repeated_ = mgm_get_ndpr (*pMgmHandle, index);
                entry.hd_ = mgm_get_hd (*pMgmHandle, index);

                gp2->entries.insert(entry);
            }
        }

        return gp2;
    }

}
