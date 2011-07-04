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

#ifndef METGM_GROUP5PTR_H
#define METGM_GROUP5PTR_H

// metgm
//
#include "metgm.h"

// internals
//
#include "MetGmGroup3Ptr.h"
#include "MetGmDimensionsTag.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"

// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>

namespace MetNoFimex {

    class MetGmGroup5Ptr {
    public:
        static boost::shared_ptr<MetGmGroup5Ptr> createMetGmGroup5Ptr(const boost::shared_ptr<CDMReader> pCdmReader,
                                                                      const CDMVariable* pVariable,
                                                                      const boost::shared_ptr<MetGmGroup3Ptr> gp3,
                                                                      const float* pFillValue = 0);

        inline operator float* () { return data_.get(); }
        inline operator const float* () const { return data_.get(); }

    private:

        void changeFillValue();
        void transpozeData();

        MetGmGroup5Ptr(const boost::shared_ptr<MetGmGroup3Ptr> gp3, float fillValue = 9999.0f) : pGp3_(gp3), fillValue_(fillValue) { }

        // not owning
        const boost::shared_ptr<MetGmGroup3Ptr> pGp3_;

        // owning
        boost::shared_ptr<MetGmHDTag> hdTag_;
        boost::shared_array<float> data_;
        float fillValue_;
    };

    void MetGmGroup5Ptr::changeFillValue() {
        for(size_t index = 0; index < hdTag_->totalSize(); ++index) {
            if(fillValue_ != MIFI_UNDEFINED_F && fillValue_ == data_[index]) {
                data_[index] = 9999.0;
            } else if(isnan(data_[index])) {
                data_[index] = 9999.0;
            }
        }
    }

    void MetGmGroup5Ptr::transpozeData()
    {
        if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
            return;

        boost::shared_array<float> dataT(new float[hdTag_->totalSize()]);

        float* slice = data_.get();
        float* sliceT = dataT.get();

        for(size_t sIndex = 0; sIndex < hdTag_->tSize(); ++sIndex) {

            slice = data_.get() + sIndex * hdTag_->sliceSize();
            sliceT = dataT.get() + sIndex * hdTag_->sliceSize();

            for(size_t z_index = 0; z_index < hdTag_->zSize(); ++z_index) {

                for(size_t y_index = 0; y_index < hdTag_->ySize(); ++y_index) {
                    for(size_t x_index = 0; x_index < hdTag_->xSize(); ++x_index) {
                        sliceT[z_index + x_index * hdTag_->zSize() + y_index * (hdTag_->zSize() * hdTag_->xSize())] =
                                slice[z_index * (hdTag_->ySize() * hdTag_->xSize()) + y_index * hdTag_->xSize() + x_index];
                    } // x_index

                } // y_index

            } // z_index

        } // sliceIndex

        data_.swap(dataT);
    }

    boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5Ptr(const boost::shared_ptr<CDMReader> pCdmReader,
                                                                           const CDMVariable* pVariable,
                                                                           const boost::shared_ptr<MetGmGroup3Ptr> pg3,
                                                                           const float* pFillValue)
    {
        boost::shared_ptr<MetGmGroup5Ptr> gp5 =
                boost::shared_ptr<MetGmGroup5Ptr>(new MetGmGroup5Ptr(pg3));

        gp5->hdTag_ = MetGmHDTag::createMetGmHDTag(pCdmReader, pVariable);

        switch(gp5->hdTag_->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_3D_T:
                {
                    const std::string mgmUnits = mgm_get_param_unit(gp5->pGp3_->p_id(), *(gp5->pGp3_->mgmHandle()));

                    gp5->data_  = (pCdmReader->getScaledDataInUnit(pVariable->getName(), mgmUnits.c_str()))->asFloat();

                    gp5->fillValue_ = pFillValue ? *pFillValue : pCdmReader->getCDM().getFillValue(pVariable->getName());

                    gp5->changeFillValue();

                    gp5->transpozeData();
                }
                break;
            case MetGmHDTag::HD_0D:
            case MetGmHDTag::HD_0D_T:
            case MetGmHDTag::HD_1D:
            case MetGmHDTag::HD_1D_T:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D:
            default:
                throw CDMException(std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :") + gp5->hdTag_->asString());
        }

        return gp5;
    }
}

#endif // METGM_GROUP5PTR_H
