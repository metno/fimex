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
#include "MetGmTags.h"
#include "MetGmDimensionsTag.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmFileHandlePtr.h"
#include "MetGmHandlePtr.h"
#include "MetGmUtils.h"

// METGM C Library
//
#include "metgm.h"

namespace MetNoFimex {

std::shared_ptr<MetGmHandlePtr> MetGmHandlePtr::createMetGmHandleForReading(const std::string& source)
{
    std::shared_ptr<MetGmHandlePtr> pHandle = std::shared_ptr<MetGmHandlePtr>(new MetGmHandlePtr);

    pHandle->pFileHandle_ = MetGmFileHandlePtr::createMetGmFileHandlePtrForReading((source));
    if (!(pHandle->pFileHandle_.get()))
        throw CDMException(std::string("error opening metgm file handle for: ") + source);

    MGM_THROW_ON_ERROR(mgm_read_header(*pHandle->pFileHandle_, *pHandle));

    pHandle->pVersion_ = MetGmVersion::createMetGmVersion(mgm_get_version(*pHandle));

    return pHandle;
    }

    std::shared_ptr<MetGmHandlePtr> MetGmHandlePtr::createMetGmHandleForWriting(std::shared_ptr<MetGmFileHandlePtr>& pFileHandle,
                                                                                std::shared_ptr<MetGmVersion>& pVersion)
    {
        assert(pFileHandle.get());
        assert(pVersion.get());

        std::shared_ptr<MetGmHandlePtr> pHandle = std::shared_ptr<MetGmHandlePtr>(new MetGmHandlePtr);

        assert(pHandle.get());

        pHandle->pFileHandle_ = pFileHandle;
        pHandle->pVersion_    = pVersion;

        return pHandle;
    }

    MetGmHandlePtr::~MetGmHandlePtr()
    {
        MGM_THROW_ON_ERROR(mgm_free_handle(handle_));
    }
}
