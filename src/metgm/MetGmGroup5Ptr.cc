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
#include "../../include/metgm/MetGmDimensionsTag.h"
#include "../../include/metgm/MetGmVersion.h"

namespace MetNoFimex {

MetGmGroup5Ptr::MetGmGroup5Ptr(const boost::shared_ptr<MetGmGroup3Ptr>& gp3,
                               const boost::shared_ptr<MetGmHDTag>&     hdTag,
                               const std::vector<float>&                vdata,
                               const float&                             fillValue)
        : pGp3_(gp3),
          hdTag_(hdTag),
          vData_(vdata.begin(), vdata.end()),
          fillValue_(fillValue)
    {

    }

void MetGmGroup5Ptr::changeFillValue() {
    for(size_t index = 0; index < hdTag_->totalSize(); ++index) {
        if(fillValue_ != MIFI_UNDEFINED_F && fillValue_ == vData_.at(index)) {
            vData_.at(index) = 9999.0;
        } else if(isnan(vData_.at(index))) {
            vData_.at(index) = 9999.0;
        }
    }
}

void MetGmGroup5Ptr::toMetGmLayout()
{
    if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
        return;

    boost::shared_array<float> data(dataAsFloat());
    boost::shared_array<float> dataT(new float[hdTag_->totalSize()]);

    float* slice = data.get();
    float* sliceT = dataT.get();

    for(size_t sIndex = 0; sIndex < hdTag_->tSize(); ++sIndex) {

        slice = data.get() + sIndex * hdTag_->sliceSize();
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

    vData_.clear();
    for(size_t index = 0; index < hdTag_->totalSize(); ++index)
        vData_.push_back(data[index]);
}

void MetGmGroup5Ptr::toFimexLayout()
{
    if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
        return;

    boost::shared_array<float> data(dataAsFloat());
    boost::shared_array<float> dataT(new float[hdTag_->totalSize()]);

    float* slice = data.get();
    float* sliceT = dataT.get();

    for(size_t sIndex = 0; sIndex < hdTag_->tSize(); ++sIndex) {

        slice = data.get() + sIndex * hdTag_->sliceSize();
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

    vData_.clear();
    for(size_t index = 0; index < hdTag_->totalSize(); ++index)
        vData_.push_back(data[index]);
}

boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForWriting(boost::shared_ptr<CDMReader>& pCdmReader,
                                                                                 const CDMVariable* pVariable,
                                                                                 const boost::shared_ptr<MetGmGroup3Ptr>& pg3,
                                                                                 const float* pFillValue)
{
//    boost::shared_ptr<MetGmGroup5Ptr> gp5 =
//            boost::shared_ptr<MetGmGroup5Ptr>(new MetGmGroup5Ptr(pg3));

    boost::shared_ptr<MetGmHDTag> hdtag = MetGmHDTag::createMetGmHDTag(pCdmReader, pVariable);
    float fillValue = 9999.0f;

    switch(hdtag->asShort()) {
        case MetGmHDTag::HD_2D:
        case MetGmHDTag::HD_3D_T:
            {
                std::string mgmUnits;

//                std::cerr << __FUNCTION__ << " @ " << __LINE__ << " for " << pVariable->getName() << std::endl;
                if(pg3->p_id() == 7) {
                    /**
                      * METGM is tricky here as it can accomodate both m and hPa units
                      * depending if it is geopotential_height or pressure but the
                      * mgm_get_param_unit is, for some reason, always returning 'm'
                      *
                      * it should honor the pr settings
                      */
                    if(hdtag->zTag()->pr() == 2) {
                        /**
                          * If the value specified for pr is 2,
                          * and the values for pid=7 (if reported)
                          * are heights given in meters above MSL
                          */
                        mgmUnits = "m";
//                        std::cerr << __FUNCTION__ << " @ " << __LINE__ << " m " << pVariable->getName() << std::endl;
                    } else {
                        mgmUnits = "hPa";
//                        std::cerr << __FUNCTION__ << " @ " << __LINE__ << " hPa " << pVariable->getName()  << std::endl;
                    }
                } else {
                    mgmUnits = std::string(mgm_get_param_unit(pg3->p_id(), *(pg3->mgmHandle())));
                }

                boost::shared_array<float> data  = (pCdmReader->getScaledDataInUnit(pVariable->getName(), mgmUnits.c_str()))->asFloat();

                fillValue = pFillValue ? *pFillValue : pCdmReader->getCDM().getFillValue(pVariable->getName());

                std::vector<float> vdata;

                for(size_t index = 0; index < hdtag->totalSize(); ++index)
                    vdata.push_back(data[index]);

                boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(pg3, hdtag, vdata, fillValue));

                gp5->changeFillValue();

                gp5->toMetGmLayout();

//                std::cerr << __FUNCTION__ << " @ " << __LINE__ << " for " << pVariable->getName() << std::endl;

                return gp5;
            }
            break;
        case MetGmHDTag::HD_0D:
        case MetGmHDTag::HD_0D_T:
        case MetGmHDTag::HD_1D:
        case MetGmHDTag::HD_1D_T:
        case MetGmHDTag::HD_2D_T:
        case MetGmHDTag::HD_3D:
        default:
        throw CDMException(std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :") + hdtag->asString() + " for " + pVariable->getName());
    }

    std::vector<float> empty;
    return boost::shared_ptr<MetGmGroup5Ptr>(new MetGmGroup5Ptr(pg3, hdtag, empty, fillValue));
}

    boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForReading(boost::shared_ptr<MetGmGroup3Ptr>& gp3,
                                                                                     boost::shared_ptr<MetGmHDTag>&     hdTag)
    {
//        std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                  << "[pid=" << gp3->p_id() << "]"
//                  << " dimensionality " << hdTag->asString()
//                  << std::endl;

        switch(hdTag->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D_T:
                {
                   int sizeToAlloc = hdTag->totalSize();

//                    std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                              << "[pid=" << gp3->p_id() << "]"
//                              << " before allocation of size " << hdTag->totalSize()
//                              << std::endl;

                    boost::shared_array<float> data(new float[sizeToAlloc]);

//                    std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                              << "[pid=" << gp3->p_id() << "]"
//                              << " after allocation of size " << hdTag->totalSize()
//                              << std::endl;

                    MGM_THROW_ON_ERROR(mgm_read_group5(*gp3->mgmHandle()->fileHandle(), *gp3->mgmHandle(), data.get()))
                    MGM_THROW_ON_ERROR(mgm_param_is_convertible(gp3->p_id(), *gp3->mgmHandle()->version()))

                    std::vector<float> vdata;
                    for(size_t index = 0; index < sizeToAlloc; ++index)
                        vdata.push_back(data[index]);
                    boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(gp3, hdTag, vdata));

                    // from METGM to Fimex layout
                    gp5->toFimexLayout();

                    return gp5;
                }
                break;
            case MetGmHDTag::HD_0D:
            case MetGmHDTag::HD_0D_T:
            case MetGmHDTag::HD_1D:
            case MetGmHDTag::HD_1D_T:
            case MetGmHDTag::HD_3D:
            default:
                throw CDMException(  std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :")
                                   + hdTag->asString()
                                   + " for p_id="
                                   + boost::lexical_cast<std::string>(gp3->p_id()));
        }

        std::vector<float> empty;
        return  boost::shared_ptr<MetGmGroup5Ptr> (new MetGmGroup5Ptr(gp3, hdTag, empty));
    }

    void MetGmGroup5Ptr::dumpFimexLayout() {

        boost::shared_array<float> data(dataAsFloat());

        std::cerr << "dumping group5 in Fimex layout [START]" << std::endl;
        for(size_t sIndex = 0; sIndex < hdTag_->tSize(); ++sIndex) {

            float* slice = data.get() + sIndex * hdTag_->sliceSize();

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

    boost::shared_array<float> MetGmGroup5Ptr::dataAsFloat()
    {
        boost::shared_array<float> data(new float[vData_.size()]);

        for(size_t index = 0; index < vData_.size(); ++index)
            data[index] = vData_.at(index);

        return data;
    }

}
