/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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
#include "MetGmHandlePtr.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup2Ptr.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmGroup5Ptr.h"
#include "MetGmVerticalTag.h"
#include "MetGmDimensionsTag.h"

// METGM C Lib
//
#include "metgm.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"

namespace MetNoFimex {

std::shared_ptr<MetGmTags> MetGmTags::createMetGmTagsForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable,
                                                                const std::shared_ptr<MetGmHandlePtr> mgmHandle, const unsigned short p_id)
{
    std::shared_ptr<MetGmTags> tags = std::shared_ptr<MetGmTags>(new MetGmTags);
    tags->pGp3_ = MetGmGroup3Ptr::createMetGmGroup3PtrForWriting(mgmHandle, p_id);
    tags->dimTag_ = MetGmHDTag::createMetGmDimensionsTagForWriting(pCdmReader, pVariable);
    tags->pGp5_ = MetGmGroup5Ptr::createMetGmGroup5PtrForWriting(pCdmReader, pVariable, tags->pGp3_);

    if (tags->pGp5_.get() && tags->dimTag_.get() && tags->pGp3_.get())
        return tags;
    else
        return std::shared_ptr<MetGmTags>();
    }

    std::shared_ptr<MetGmTags> MetGmTags::createMetGmTagsForReading(const std::shared_ptr<MetGmGroup1Ptr> pGp1, const std::shared_ptr<MetGmGroup2Ptr> pGp2,
                                                                    const std::shared_ptr<MetGmVerticalTag> vTag)
    {
        std::shared_ptr<MetGmTags> tags = std::shared_ptr<MetGmTags>(new MetGmTags(pGp1, pGp2));
        tags->pGp3_   = MetGmGroup3Ptr::createMetGmGroup3PtrForReading(pGp1->mgmHandle());
        tags->dimTag_ = MetGmHDTag::createMetGmDimensionsTag(pGp1, tags->pGp3_, vTag);
        tags->pGp5_   = MetGmGroup5Ptr::createMetGmGroup5PtrForReading(tags->pGp3_, tags->dimTag_);
        return tags;
    }

    std::shared_ptr<MetGmTags> MetGmTags::createMetGmTagsForSlicedReading(const std::shared_ptr<MetGmGroup1Ptr> pGp1,
                                                                          const std::shared_ptr<MetGmGroup2Ptr> pGp2,
                                                                          const std::shared_ptr<MetGmVerticalTag> vTag)
    {
        std::shared_ptr<MetGmTags> tags = std::shared_ptr<MetGmTags>(new MetGmTags(pGp1, pGp2));
        tags->pGp3_   = MetGmGroup3Ptr::createMetGmGroup3PtrForReading(pGp1->mgmHandle());
        tags->dimTag_ = MetGmHDTag::createMetGmDimensionsTag(pGp1, tags->pGp3_, vTag);
        tags->pGp5_   = MetGmGroup5Ptr::createMetGmGroup5PtrForSlicedReading(tags->pGp3_, tags->dimTag_);
        return tags;
    }

    std::shared_ptr<MetGmTags> MetGmTags::createMetGmTagsForSlicedWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable,
                                                                          const std::shared_ptr<MetGmHandlePtr> mgmHandle, const unsigned short p_id)
    {
        std::shared_ptr<MetGmTags> tags = std::shared_ptr<MetGmTags>(new MetGmTags);
        tags->pGp3_   = MetGmGroup3Ptr::createMetGmGroup3PtrForWriting(mgmHandle, p_id);
        tags->dimTag_ = MetGmHDTag::createMetGmDimensionsTagForWriting(pCdmReader, pVariable);
        tags->pGp5_   = MetGmGroup5Ptr::createMetGmGroup5PtrForSlicedWriting(pCdmReader, pVariable, tags->pGp3_);
        if(tags->pGp5_.get() && tags->dimTag_.get() && tags->pGp3_.get())
            return tags;
        else
            return std::shared_ptr<MetGmTags>();
    }

    unsigned short MetGmTags::p_id() const
    {
        return pGp3_->p_id();
    }
    int MetGmTags::pr() const
    {
        return pGp3_->pr();
    }
    int MetGmTags::pz() const
    {
        return pGp3_->pz();
    }
    unsigned short MetGmTags::hd() const
    {
        return dimTag_->asShort();
    }

    int MetGmTags::set_nt(int nt)   { return pGp3_->set_nt(nt); }
    int MetGmTags::set_dt(float dt) { return pGp3_->set_dt(dt); }

    int MetGmTags::set_nz(int nz)   { return pGp3_->set_nz(nz); }
    int MetGmTags::set_pz(int pz)   { return pGp3_->set_pz(pz); }
    int MetGmTags::set_pr(int pr)   { return pGp3_->set_pr(pr); }

    int MetGmTags::set_nx(int nx)   { return pGp3_->set_nx(nx); }
    int MetGmTags::set_dx(float dx) { return pGp3_->set_dx(dx); }
    int MetGmTags::set_cx(float cx) { return pGp3_->set_cx(cx); }

    int MetGmTags::set_ny(int ny)   { return pGp3_->set_ny(ny); }
    int MetGmTags::set_dy(float dy) { return pGp3_->set_dy(dy); }
    int MetGmTags::set_cy(float cy) { return pGp3_->set_cy(cy); }

    std::shared_ptr<MetGmXTag>& MetGmTags::xTag()
    {
        return dimTag_->xTag();
    }
    std::shared_ptr<MetGmYTag>& MetGmTags::yTag()
    {
        return dimTag_->yTag();
    }
    std::shared_ptr<MetGmVerticalTag>& MetGmTags::zTag()
    {
        return dimTag_->zTag();
    }
    std::shared_ptr<MetGmTimeTag>& MetGmTags::tTag()
    {
        return dimTag_->tTag();
    }

    unsigned long MetGmTags::totalDataSize()
    {
        return dimTag_->totalSize();
    }
    unsigned long MetGmTags::sliceDataSize()
    {
        return dimTag_->sliceSize();
    }

    void MetGmTags::sliceToMetGmLayout(shared_array<float>& slice)
    {
        pGp5_->sliceToMetGmLayout(slice);
    }

    shared_array<float> MetGmTags::readDataSlices(size_t pos, size_t numberOfSlices)
    {
        return pGp5_->readDataSlices(pos, numberOfSlices);
    }

    const shared_array<float>& MetGmTags::data()
    {
        return pGp5_->data();
    }
    std::string MetGmTags::units() const
    {
        return pGp5_->units();
    }
}
