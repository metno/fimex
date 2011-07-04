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

        inline static boost::shared_ptr<MetGmHDTag> createMetGmHDTag(boost::shared_ptr<CDMReader> pCdmReader, const CDMVariable* pVar) {

            boost::shared_ptr<MetGmHDTag> tag = boost::shared_ptr<MetGmHDTag>(new MetGmHDTag);
            tag->pCdmReader_ = pCdmReader;
            tag->pVar_ = pVar;

            const CDM& cdmRef = pCdmReader->getCDM();
            std::string varName = tag->pVar_->getName();

            bool hasTAxis = !( cdmRef.getTimeAxis(varName).empty() );
            tag->pTDim_ = hasTAxis ? &cdmRef.getDimension(cdmRef.getTimeAxis(varName)): 0;

            bool hasXAxis = !( cdmRef.getHorizontalXAxis(varName).empty() );
            tag->pXDim_ = hasXAxis ? &cdmRef.getDimension(cdmRef.getHorizontalXAxis(varName)) : 0;

            bool hasYAxis = !( cdmRef.getHorizontalYAxis(varName).empty() );
            tag->pYDim_ = hasYAxis ? &cdmRef.getDimension(cdmRef.getHorizontalYAxis(varName)) : 0;

            bool hasZAxis = !( cdmRef.getVerticalAxis(varName).empty() );
            tag->pZDim_ = hasZAxis ? &cdmRef.getDimension(cdmRef.getVerticalAxis(varName)) : 0;

            if(hasZAxis && hasXAxis && hasYAxis) {
                tag->hd_= hasTAxis ? HD_3D_T : HD_3D;
            } else if((hasZAxis && hasXAxis && hasYAxis) || (hasZAxis && !hasXAxis && hasYAxis) || (hasZAxis && hasXAxis && !hasYAxis)) {
                tag->hd_= hasTAxis ? HD_2D_T : HD_2D;
            } else if((!hasZAxis && !hasXAxis && hasYAxis) || (!hasZAxis && hasXAxis && !hasYAxis) || (hasZAxis && !hasXAxis && !hasYAxis)) {
                tag->hd_= hasTAxis ? HD_1D_T : HD_1D;
            } else {
                tag->hd_= hasTAxis ?  HD_0D_T : HD_0D;
            }

            tag->sliceSize_ = (tag->pXDim_ ? tag->pXDim_->getLength() : 1)
                            * (tag->pYDim_ ? tag->pYDim_->getLength() : 1)
                            * (tag->pZDim_ ? tag->pZDim_->getLength() : 1);

            tag->totalSize_ = tag->sliceSize() * ( (hasTAxis) ? tag->pTDim_->getLength() : 1 );

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

        size_t xSize() const { return pXDim_ ? pXDim_->getLength() : 0; }
        size_t ySize() const { return pYDim_ ? pYDim_->getLength() : 0; }
        size_t zSize() const { return pZDim_ ? pZDim_->getLength() : 0; }
        size_t tSize() const { return pTDim_ ? pTDim_->getLength() : 0; }

    private:

        inline MetGmHDTag() : pVar_(0), pTDim_(0), pXDim_(0), pYDim_(0), pZDim_(0), hd_(MetGmHDTag::HD_0D), sliceSize_(1), totalSize_() { }

        /**
          * not owner, just holding reference
          */
        const CDMVariable*  pVar_;
        const CDMDimension* pTDim_;
        const CDMDimension* pXDim_;
        const CDMDimension* pYDim_;
        const CDMDimension* pZDim_;

        MetGmHD hd_;

        size_t sliceSize_;
        size_t totalSize_;
    };
}

#endif // METGM_DIMENSIONALTAG_H
