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

#ifndef METGM_DIMENSIONALTAG_H
#define METGM_DIMENSIONALTAG_H

#include "MetGmTimeTag.h"
#include "MetGmVerticalTag.h"
#include "MetGmHorizontalTag.h"

#include "metgm.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDMDimension.h"

#include <cstdio>
#include <string>

namespace MetNoFimex {

    class MetGmHDTag {
    public:

        enum MetGmHD {
            HD_3D_T = 1,     // volume + multiple time steps (highest dimensionality)
            HD_3D   = 2,     // volume (single time step volume)
            HD_2D_T = 3,     // horizontal or vertical slice (meteorological cross section) + multiple time steps
            HD_2D   = 4,     // horizontal or vertical slice (single time step )
            HD_1D_T = 5,     // linear profile + multiple time steps
            HD_1D   = 6,     // linear profile “METCM equivalent” (single time step profile)
            HD_0D_T = 7,     // single grid point + multiple time steps
            HD_0D   = 8      // single grid point (single time step) (lowest dimensionality)
        };

        inline static boost::shared_ptr<MetGmHDTag> createMetGmHDTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVariable) {

            boost::shared_ptr<MetGmHDTag> tag = boost::shared_ptr<MetGmHDTag>(new MetGmHDTag);

            tag->pXTag_ = MetGmHorizontalTag::createMetGmXTag(pCdmReader, pVariable);
            bool hasXAxis = tag->pXTag_.get() ? true : false;

            tag->pYTag_ = MetGmHorizontalTag::createMetGmYTag(pCdmReader, pVariable);
            bool hasYAxis = tag->pYTag_.get() ? true : false;

            tag->pZTag_ = MetGmVerticalTag::createMetGmVerticalTag(pCdmReader, pVariable);
            bool hasZAxis = tag->pZTag_.get() ? true : false;

            tag->pTTag_ = MetGmTimeTag::createMetGmTimeTag(pCdmReader, pVariable);
            bool hasTAxis = tag->pTTag_.get() ? true : false;

            std::cerr << " hasXAxis=" << hasXAxis << " hasYAxis=" << hasYAxis
                      << " hasZAxis=" << hasZAxis << " hasTAxis=" << hasTAxis
                      << std::endl;

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

        std::string asString() {
            switch (hd_)
            {
                case HD_3D_T: return "HD_3D_T";
                case HD_3D:   return "HD_3D";
                case HD_2D_T: return "HD_2D_T";
                case HD_2D:   return "HD_2D";
                case HD_1D_T: return "HD_1D_T";
                case HD_1D:   return "HD_1D";
                case HD_0D_T: return "HD_0D_T";
                case HD_0D:   return "HD_0D";
            }
        }

        short asShort() { return static_cast<short>(hd_); }

        size_t sliceSize() const {return sliceSize_; }
        size_t totalSize() const {return totalSize_; }

        size_t xSize() const { return pXTag_.get() ? pXTag_->nx() : 0; }
        size_t ySize() const { return pYTag_.get() ? pYTag_->ny() : 0; }
        size_t zSize() const { return pZTag_.get() ? pZTag_->nz() : 0; }
        size_t tSize() const { return pTTag_.get() ? pTTag_->nT() : 0; }

        boost::shared_ptr<MetGmXTag>        xTag() { return pXTag_; }
        boost::shared_ptr<MetGmYTag>        yTag() { return pYTag_; }
        boost::shared_ptr<MetGmVerticalTag> zTag() { return pZTag_; }
        boost::shared_ptr<MetGmTimeTag>     tTag() { return pTTag_; }
    private:

        inline MetGmHDTag() : hd_(MetGmHDTag::HD_0D), sliceSize_(1), totalSize_() { }

        boost::shared_ptr<MetGmXTag> pXTag_;
        boost::shared_ptr<MetGmYTag> pYTag_;
        boost::shared_ptr<MetGmTimeTag> pTTag_;
        boost::shared_ptr<MetGmVerticalTag> pZTag_;

        MetGmHD hd_;

        size_t sliceSize_;
        size_t totalSize_;
    };
}

#endif // METGM_DIMENSIONALTAG_H
