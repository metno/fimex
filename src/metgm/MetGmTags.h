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

#ifndef METGM_TAGS_H
#define METGM_TAGS_H

#include "fimex/CDMReaderDecl.h"
#include "fimex/SharedArray.h"

// standard
#include <memory>
#include <string>

namespace MetNoFimex {

    class CDMVariable;
    class MetGmHDTag;
    class MetGmHandlePtr;
    class MetGmGroup1Ptr;
    class MetGmGroup2Ptr;
    class MetGmGroup3Ptr;
    class MetGmGroup5Ptr;
    class MetGmXTag;
    class MetGmYTag;
    class MetGmTimeTag;
    class MetGmVerticalTag;

    class MetGmTags {
    public:
        static std::shared_ptr<MetGmTags> createMetGmTagsForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable,
                                                                    const std::shared_ptr<MetGmHandlePtr> mgmHandle, const unsigned short p_id,
                                                                    const std::string fillValue = std::string(), const std::string addOffset = std::string(),
                                                                    const std::string scaleFactor = std::string());

        static std::shared_ptr<MetGmTags> createMetGmTagsForReading(const std::shared_ptr<MetGmGroup1Ptr> pGp1, const std::shared_ptr<MetGmGroup2Ptr> pGp2,
                                                                    const std::shared_ptr<MetGmVerticalTag> vTag);

        static std::shared_ptr<MetGmTags> createMetGmTagsForSlicedReading(const std::shared_ptr<MetGmGroup1Ptr> pGp1,
                                                                          const std::shared_ptr<MetGmGroup2Ptr> pGp2,
                                                                          const std::shared_ptr<MetGmVerticalTag> vTag);

        static std::shared_ptr<MetGmTags> createMetGmTagsForSlicedWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable,
                                                                          const std::shared_ptr<MetGmHandlePtr> mgmHandle, const unsigned short p_id);

        const std::shared_ptr<MetGmGroup3Ptr>& gp3() { return pGp3_; }
        //        const std::shared_ptr<MetGmHDTag>&     dimTag() { return dimTag_;}

        const unsigned short p_id()  const;
        const int            pr()    const;
        const int            pz()    const;
        const unsigned short hd()    const;
        const std::string    units() const;

        int set_nt(int nt);
        int set_dt(float dt);

        int set_nz(int nz);
        int set_pz(int pz);
        int set_pr(int pr);

        int set_nx(int nx);
        int set_dx(float dx);
        int set_cx(float cx);

        int set_ny(int ny);
        int set_dy(float dy);
        int set_cy(float cy);

        const unsigned long totalDataSize();
        const unsigned long sliceDataSize();
        const shared_array<float>& data();

        std::shared_ptr<MetGmXTag>& xTag();
        std::shared_ptr<MetGmYTag>& yTag();
        std::shared_ptr<MetGmVerticalTag>& zTag();
        std::shared_ptr<MetGmTimeTag>& tTag();

        void sliceToMetGmLayout(shared_array<float>& slice);

        shared_array<float> readDataSlices(size_t pos, size_t numberOfSlices);

    private:
        MetGmTags() { }

        MetGmTags(const std::shared_ptr<MetGmGroup1Ptr> pg1, const std::shared_ptr<MetGmGroup2Ptr> pg2)
            : pGp1_(pg1)
            , pGp2_(pg2)
        {
        }

        const std::shared_ptr<MetGmGroup1Ptr> pGp1_;
        const std::shared_ptr<MetGmGroup2Ptr> pGp2_;

        std::shared_ptr<MetGmGroup3Ptr> pGp3_;
        std::shared_ptr<MetGmGroup5Ptr> pGp5_;
        std::shared_ptr<MetGmHDTag> dimTag_;
    };
}

#endif // METGM_TAGS_H
