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
#include "../../include/metgm/MetGmTags.h"
#include "../../include/metgm/MetGmGroup1Ptr.h"
#include "../../include/metgm/MetGmGroup2Ptr.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"
#include "../../include/metgm/MetGmGroup5Ptr.h"
#include "../../include/metgm/MetGmDimensionsTag.h"

// METGM C Lib
//
#include "metgm.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"

namespace MetNoFimex {

    boost::shared_ptr<MetGmTags> MetGmTags::createMetGmTags(boost::shared_ptr<CDMReader>& pCdmReader,
                                                            const CDMVariable* pVariable,
                                                            boost::shared_ptr<MetGmGroup3Ptr>& pg3,
                                                            const float* pFillValue)
    {
        boost::shared_ptr<MetGmTags> tags = boost::shared_ptr<MetGmTags>(new MetGmTags);
        tags->pGp3_   = pg3;
        tags->dimTag_ = MetGmHDTag::createMetGmHDTag(pCdmReader, pVariable);
        tags->pGp5_   = MetGmGroup5Ptr::createMetGmGroup5Ptr(pCdmReader, pVariable, tags->gp3(), pFillValue);

        return tags;
    }

    boost::shared_ptr<MetGmTags> MetGmTags::createMetGmTagsForReading(boost::shared_ptr<MetGmGroup1Ptr>& pGp1,
                                                                      boost::shared_ptr<MetGmGroup2Ptr>& pGp2,
                                                                      boost::shared_ptr<MetGmGroup3Ptr>& pGp3)
    {
        boost::shared_ptr<MetGmTags> tags = boost::shared_ptr<MetGmTags>(new MetGmTags);

        tags->pGp1_   = pGp1;
        tags->pGp2_   = pGp2;
        tags->pGp3_   = pGp3;
        tags->dimTag_ = MetGmHDTag::createMetGmDimensionsTag(pGp1, pGp3);
        tags->pGp5_   = MetGmGroup5Ptr::createMetGmGroup5PtrForReading(pGp3, tags->dimTag_);

        return tags;
    }
}


