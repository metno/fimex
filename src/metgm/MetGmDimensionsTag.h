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

    class MetGmGroup1Ptr;

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

        static boost::shared_ptr<MetGmHDTag> createMetGmDimensionsTagForWriting(const CDMReader_p pCdmReader,
                                                                                const CDMVariable* pVariable);

        static boost::shared_ptr<MetGmHDTag> createMetGmDimensionsTag(const boost::shared_ptr<MetGmGroup1Ptr>   pGp1,
                                                                      const boost::shared_ptr<MetGmGroup3Ptr>   pGp3,
                                                                      const boost::shared_ptr<MetGmVerticalTag> vTag);

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
                default:      return "Undefined_HD";
            }
            return "Undefined_HD";
        }

        MetGmHD hd()    { return hd_; }
        short asShort() { return static_cast<short>(hd_); }

        size_t sliceSize() const {return sliceSize_; }
        size_t totalSize() const {return totalSize_; }

        size_t xSize() const { return pXTag_.get() ? pXTag_->nx() : 0; }
        size_t ySize() const { return pYTag_.get() ? pYTag_->ny() : 0; }
        size_t zSize() const { return pZTag_.get() ? pZTag_->nz() : 0; }
        size_t tSize() const { return pTTag_.get() ? pTTag_->nT() : 0; }

        boost::shared_ptr<MetGmXTag>&        xTag() { return pXTag_; }
        boost::shared_ptr<MetGmYTag>&        yTag() { return pYTag_; }
        boost::shared_ptr<MetGmVerticalTag>& zTag() { return pZTag_; }
        boost::shared_ptr<MetGmTimeTag>&     tTag() { return pTTag_; }

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
