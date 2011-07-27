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
#include "../../include/metgm/MetGmUtils.h"
#include "../../include/metgm/MetGmGroup5Ptr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmVersion.h"

namespace MetNoFimex {

void MetGmGroup5Ptr::changeFillValue() {
    for(size_t index = 0; index < hdTag_->totalSize(); ++index) {
        if(fillValue_ != MIFI_UNDEFINED_F && fillValue_ == data_[index]) {
            data_[index] = 9999.0;
        } else if(isnan(data_[index])) {
            data_[index] = 9999.0;
        }
    }
}

void MetGmGroup5Ptr::toMetGmLayout()
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

void MetGmGroup5Ptr::toFimexLayout()
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

                    sliceT[z_index * (hdTag_->ySize() * hdTag_->xSize()) + y_index * hdTag_->xSize() + x_index] =
                            slice[z_index + x_index * hdTag_->zSize() + y_index * (hdTag_->zSize() * hdTag_->xSize())];
                } // x_index

            } // y_index

        } // z_index

    } // sliceIndex

    data_.swap(dataT);
}

boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForWriting(boost::shared_ptr<CDMReader>& pCdmReader,
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
                std::string mgmUnits;

                std::cerr << __FUNCTION__ << " @ " << __LINE__ << " for " << pVariable->getName() << std::endl;
                if(gp5->pGp3_->p_id() == 7) {
                    /**
                      * METGM is tricky here as it can accomodate both m and hPa units
                      * depending if it is geopotential_height or pressure but the
                      * mgm_get_param_unit is, for some reason, always returning 'm'
                      *
                      * it should honor the pr settings
                      */
                    if(gp5->hdTag_->zTag()->pr() == 2) {
                        /**
                          * If the value specified for pr is 2,
                          * and the values for pid=7 (if reported)
                          * are heights given in meters above MSL
                          */
                        mgmUnits = "m";
                        std::cerr << __FUNCTION__ << " @ " << __LINE__ << " m " << pVariable->getName() << std::endl;
                    } else {
                        mgmUnits = "hPa";
                        std::cerr << __FUNCTION__ << " @ " << __LINE__ << " hPa " << pVariable->getName() << std::endl;
                    }
                } else {
                    mgmUnits = std::string(mgm_get_param_unit(gp5->pGp3_->p_id(), *(gp5->pGp3_->mgmHandle())));
                }

                gp5->data_  = (pCdmReader->getScaledDataInUnit(pVariable->getName(), mgmUnits.c_str()))->asFloat();

                gp5->fillValue_ = pFillValue ? *pFillValue : pCdmReader->getCDM().getFillValue(pVariable->getName());

                gp5->changeFillValue();

                gp5->toMetGmLayout();

                std::cerr << __FUNCTION__ << " @ " << __LINE__ << " for " << pVariable->getName() << std::endl;
            }
            break;
        case MetGmHDTag::HD_0D:
        case MetGmHDTag::HD_0D_T:
        case MetGmHDTag::HD_1D:
        case MetGmHDTag::HD_1D_T:
        case MetGmHDTag::HD_2D_T:
        case MetGmHDTag::HD_3D:
        default:
        throw CDMException(std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :") + gp5->hdTag_->asString() + " for " + pVariable->getName());
    }

    return gp5;
}

    boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForReading(boost::shared_ptr<MetGmGroup3Ptr>& gp3,
                                                                                     boost::shared_ptr<MetGmHDTag>&     hdTag)
    {
        boost::shared_ptr<MetGmGroup5Ptr> gp5 =
                boost::shared_ptr<MetGmGroup5Ptr>(new MetGmGroup5Ptr(gp3));

        gp5->hdTag_ = hdTag;

        switch(gp5->hdTag_->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_3D_T:
                {
                    gp5->data_.reset(new float[gp5->hdTag_->totalSize()]);

                    MGM_THROW_ON_ERROR(mgm_read_group5(*gp3->mgmHandle()->fileHandle(), *gp3->mgmHandle(), gp5->data_.get()))
                    MGM_THROW_ON_ERROR(mgm_param_is_convertible(gp5->pGp3_->p_id(), *gp3->mgmHandle()->version()))

                    // from METGM to Fimex layout
                    gp5->toFimexLayout();
                }
                break;
            case MetGmHDTag::HD_0D:
            case MetGmHDTag::HD_0D_T:
            case MetGmHDTag::HD_1D:
            case MetGmHDTag::HD_1D_T:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D:
            default:
                throw CDMException(  std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :")
                                   + gp5->hdTag_->asString()
                                   + " for p_id="
                                   + boost::lexical_cast<std::string>(gp5->pGp3_->p_id()));
        }
        return gp5;
    }

    void MetGmGroup5Ptr::dumpFimexLayout() {
        std::cerr << "dumping group5 in Fimex layout [START]" << std::endl;
        for(size_t sIndex = 0; sIndex < hdTag_->tSize(); ++sIndex) {

            float* slice = data_.get() + sIndex * hdTag_->sliceSize();

            for(size_t z_index = 0; z_index < hdTag_->zSize(); ++z_index) {

                for(size_t y_index = 0; y_index < hdTag_->ySize(); ++y_index) {

                    for(size_t x_index = 0; x_index < hdTag_->xSize(); ++x_index) {

                        std::cerr << "[pid=" << pGp3_->p_id()  << "]"
                                  << "[T=" << sIndex  << "]"
                                  << "[X=" << x_index << "]"
                                  << "[Y=" << y_index << "]"
                                  << "[Z=" << z_index << "]"
                                  << "{" << slice[x_index + y_index * hdTag_->xSize() + z_index * hdTag_->xSize() * hdTag_->ySize()] << "}"
                                  << "    ";
                    } // x_index

                    std::cerr << std::endl << "end of Y" << std::endl;

                } // y_index

                std::cerr << std::endl << "end of Z" << std::endl;

            } // z_index

            std::cerr << std::endl << "end of T" << std::endl;

        } // sliceIndex
        std::cerr << "dumping group5 in Fimex layout"   << std::endl;
    }
}
