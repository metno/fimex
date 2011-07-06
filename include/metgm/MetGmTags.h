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

#ifndef METGM_TAGS_H
#define METGM_TAGS_H

// METGM C lib
//
#include "metgm.h"

// implementation classes
//
#include "MetGmGroup3Ptr.h"
#include "MetGmGroup5Ptr.h"
#include "MetGmDimensionsTag.h"

// boost
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    class MetGmTags {
    public:
        static boost::shared_ptr<MetGmTags> createMetGmTags(boost::shared_ptr<CDMReader>& pCdmReader,
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

        boost::shared_ptr<MetGmHDTag>       dimTag() { return dimTag_;}
        boost::shared_ptr<MetGmGroup3Ptr>   gp3()    { return pGp3_; }
        boost::shared_ptr<MetGmGroup5Ptr>   gp5()    { return pGp5_; }

    private:
        MetGmTags() {}
        boost::shared_ptr<MetGmHDTag>       dimTag_;
        boost::shared_ptr<MetGmGroup3Ptr>   pGp3_;
        boost::shared_ptr<MetGmGroup5Ptr>   pGp5_;
    };
}

#endif // METGM_TAGS_H
