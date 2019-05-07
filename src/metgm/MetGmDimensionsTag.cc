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
#include "MetGmDimensionsTag.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmFileHandlePtr.h"
#include "MetGmHandlePtr.h"
#include "MetGmUtils.h"

#include "metgm.h"

namespace MetNoFimex {

std::shared_ptr<MetGmHDTag> MetGmHDTag::createMetGmDimensionsTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable)
{

    std::shared_ptr<MetGmHDTag> tag = std::shared_ptr<MetGmHDTag>(new MetGmHDTag);

    tag->pXTag_ = MetGmHorizontalTag::createMetGmXTagForWriting(pCdmReader, pVariable);
    bool hasXAxis = tag->pXTag_.get() ? true : false;

    tag->pYTag_ = MetGmHorizontalTag::createMetGmYTagForWriting(pCdmReader, pVariable);
    bool hasYAxis = tag->pYTag_.get() ? true : false;

    tag->pZTag_ = MetGmVerticalTag::createMetGmVerticalTagForWriting(pCdmReader, pVariable);
    bool hasZAxis = tag->pZTag_.get() ? true : false;

    tag->pTTag_ = MetGmTimeTag::createMetGmTimeTagForWriting(pCdmReader, pVariable);
    bool hasTAxis = tag->pTTag_.get() ? true : false;

    if (hasZAxis && hasXAxis && hasYAxis) {
        tag->hd_ = hasTAxis ? HD_3D_T : HD_3D;
    } else if ((hasXAxis && hasYAxis) || (hasZAxis && hasYAxis) || (hasZAxis && hasXAxis)) {
        tag->hd_ = hasTAxis ? HD_2D_T : HD_2D;
    } else if ((hasYAxis) || (hasXAxis) || (hasZAxis)) {
        tag->hd_ = hasTAxis ? HD_1D_T : HD_1D;
    } else {
        tag->hd_ = hasTAxis ? HD_0D_T : HD_0D;
    }

    tag->sliceSize_ = (hasXAxis ? tag->pXTag_->nx() : 1) * (hasYAxis ? tag->pYTag_->ny() : 1) * (hasZAxis ? tag->pZTag_->nz() : 1);

    tag->totalSize_ = tag->sliceSize() * ((hasTAxis) ? tag->pTTag_->nT() : 1);

    return tag;
    }

    std::shared_ptr<MetGmHDTag> MetGmHDTag::createMetGmDimensionsTag(const std::shared_ptr<MetGmGroup1Ptr> pGp1, const std::shared_ptr<MetGmGroup3Ptr> pGp3,
                                                                     const std::shared_ptr<MetGmVerticalTag> vTag)
    {
        std::shared_ptr<MetGmHDTag> tag = std::shared_ptr<MetGmHDTag>(new MetGmHDTag);

        tag->pTTag_ = MetGmTimeTag::createMetGmTimeTagForReading(pGp1, pGp3);
        tag->pXTag_ = MetGmHorizontalTag::createMetGmXTagForReading(pGp3);
        tag->pYTag_ = MetGmHorizontalTag::createMetGmYTagForReading(pGp3);
        tag->pZTag_ = MetGmVerticalTag::createMetGmVerticalTagForReading(pGp3, vTag);

        bool hasXAxis = tag->pXTag_.get() ? true : false;
        bool hasYAxis = tag->pYTag_.get() ? true : false;
        bool hasZAxis = tag->pZTag_.get() ? true : false;
        bool hasTAxis = tag->pTTag_.get() ? true : false;

        if(hasZAxis && hasXAxis && hasYAxis) {
            tag->hd_= hasTAxis ? HD_3D_T : HD_3D;
        } else if((hasXAxis && hasYAxis) || (hasZAxis && hasYAxis) || (hasZAxis && hasXAxis)) {
            tag->hd_= hasTAxis ? HD_2D_T : HD_2D;
        } else if((hasYAxis) || (hasXAxis) || (hasZAxis)) {
            tag->hd_= hasTAxis ? HD_1D_T : HD_1D;
        } else {
            tag->hd_= hasTAxis ?  HD_0D_T : HD_0D;
        }

        tag->sliceSize_ = (hasXAxis ? tag->pXTag_->nx() : 1)
                * (hasYAxis ? tag->pYTag_->ny() : 1)
                * (hasZAxis ? tag->pZTag_->nz() : 1);

        tag->totalSize_ = tag->sliceSize() * ( (hasTAxis) ? tag->pTTag_->nT() : 1 );

        return tag;
    }
}


