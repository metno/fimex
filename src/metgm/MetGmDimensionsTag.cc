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
#include "../../include/metgm/MetGmDimensionsTag.h"
#include "../../include/metgm/MetGmGroup1Ptr.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmUtils.h"

#include "metgm.h"

namespace MetNoFimex {

    boost::shared_ptr<MetGmHDTag> MetGmHDTag::createMetGmHDTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVariable) {

        boost::shared_ptr<MetGmHDTag> tag = boost::shared_ptr<MetGmHDTag>(new MetGmHDTag);

        tag->pXTag_ = MetGmHorizontalTag::createMetGmXTag(pCdmReader, pVariable);
        bool hasXAxis = tag->pXTag_.get() ? true : false;

        tag->pYTag_ = MetGmHorizontalTag::createMetGmYTag(pCdmReader, pVariable);
        bool hasYAxis = tag->pYTag_.get() ? true : false;

        tag->pZTag_ = MetGmVerticalTag::createMetGmVerticalTagForWriting(pCdmReader, pVariable);
        bool hasZAxis = tag->pZTag_.get() ? true : false;

        tag->pTTag_ = MetGmTimeTag::createMetGmTimeTag(pCdmReader, pVariable);
        bool hasTAxis = tag->pTTag_.get() ? true : false;

//        std::cerr << " hasXAxis=" << hasXAxis << " hasYAxis=" << hasYAxis
//                  << " hasZAxis=" << hasZAxis << " hasTAxis=" << hasTAxis
//                  << std::endl;

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

    boost::shared_ptr<MetGmHDTag> MetGmHDTag::createMetGmDimensionsTag(boost::shared_ptr<MetGmGroup1Ptr>&   pGp1,
                                                                       boost::shared_ptr<MetGmGroup3Ptr>&   pGp3,
                                                                       boost::shared_ptr<MetGmVerticalTag>& vTag)
    {
        boost::shared_ptr<MetGmHDTag> tag = boost::shared_ptr<MetGmHDTag>(new MetGmHDTag);

//        std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                  << "[pid=" << pGp3->p_id() << "]"
//                  << std::endl;

        tag->pTTag_ = MetGmTimeTag::createMetGmTimeTag(pGp1, pGp3);
        tag->pXTag_ = MetGmHorizontalTag::createMetGmXTag(pGp3);
        tag->pYTag_ = MetGmHorizontalTag::createMetGmYTag(pGp3);
        tag->pZTag_ = MetGmVerticalTag::createMetGmVerticalTagForReading(pGp3, vTag);

//        std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                  << "[pid=" << pGp3->p_id() << "]"
//                  << std::endl;

        bool hasXAxis = tag->pXTag_.get() ? true : false;
        bool hasYAxis = tag->pYTag_.get() ? true : false;
        bool hasZAxis = tag->pZTag_.get() ? true : false;
        bool hasTAxis = tag->pTTag_.get() ? true : false;

//        std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                  << "[pid=" << pGp3->p_id() << "]"
//                  << " hasXAxis=" << hasXAxis << " hasYAxis=" << hasYAxis
//                  << " hasZAxis=" << hasZAxis << " hasTAxis=" << hasTAxis
//                  << std::endl;

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


