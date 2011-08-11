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
#include "MetGmUtils.h"
#include "MetGmGroup5Ptr.h"
#include "MetGmFileHandlePtr.h"
#include "MetGmHandlePtr.h"
#include "MetGmDimensionsTag.h"
#include "MetGmVersion.h"

// fimex
//
#include "fimex/DataTypeChanger.h"

// boost
//
#include <boost/progress.hpp>

namespace MetNoFimex {

MetGmGroup5Ptr::MetGmGroup5Ptr(const boost::shared_ptr<MetGmGroup3Ptr> gp3,
                               const boost::shared_ptr<MetGmHDTag>     hdTag,
                               const boost::shared_array<float>        data,
                               const std::string                       fillValue)
        : pGp3_(gp3),
          hdTag_(hdTag),
          data_(data),
          fillValue_(fillValue)
    {

    }

//void MetGmGroup5Ptr::changeFillValue() {
//    float fillToChange = boost::lexical_cast<float>(fillValue_);
//    for(size_t index = 0; index < hdTag_->totalSize(); ++index) {
//        if(!fillValue_.empty() && data_[index] == fillToChange) {
//            data_[index] = 9999.0;
//        } else if(isnan(data_[index])) {
//            data_[index] = 9999.0;
//        }
//    }
//}

void MetGmGroup5Ptr::toMetGmLayout()
{
//    MetGmProfilingTimer timer;

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

//    MGM_MESSAGE_POINT(timer.elapsedToString())
}

void MetGmGroup5Ptr::toMetGmLayout(const float oldFill, const float oldScale, const float oldOffset,
                                   const float newFill, const float newScale, const float newOffset,
                                   const float unitsScale, const float unitsOffset)
{
    if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
        return;

    float offset = oldOffset * oldOffset + unitsOffset;
    float scale  = oldScale * unitsScale;

    boost::shared_array<float> dataT(new float[hdTag_->totalSize()]);

    float* slice = data_.get();
    float* sliceT = dataT.get();

    for(size_t sIndex = 0; sIndex < hdTag_->tSize(); ++sIndex) {

        slice = data_.get() + sIndex * hdTag_->sliceSize();
        sliceT = dataT.get() + sIndex * hdTag_->sliceSize();

        for(size_t z_index = 0; z_index < hdTag_->zSize(); ++z_index) {

            for(size_t y_index = 0; y_index < hdTag_->ySize(); ++y_index) {
                for(size_t x_index = 0; x_index < hdTag_->xSize(); ++x_index) {
                    float value = slice[z_index * (hdTag_->ySize() * hdTag_->xSize()) + y_index * hdTag_->xSize() + x_index];
                    if (value == oldFill || isinf(static_cast<double>(value))) {
                        value = newFill;
                    } else {
                        value = (value * scale + offset) * newScale + newOffset;
                    }
                    sliceT[z_index + x_index * hdTag_->zSize() + y_index * (hdTag_->zSize() * hdTag_->xSize())] = value;
                } // x_index

            } // y_index

        } // z_index

    } // sliceIndex

    data_.swap(dataT);
}

void MetGmGroup5Ptr::toFimexLayout()
{
//    MetGmProfilingTimer timer;

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

//    MGM_MESSAGE_POINT(timer.elapsedToString())

}

boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForWriting(const boost::shared_ptr<CDMReader> pCdmReader,
                                                                                 const CDMVariable* pVariable,
                                                                                 const boost::shared_ptr<MetGmGroup3Ptr> pg3,
                                                                                 const std::string& fillValue,
                                                                                 const std::string& addOffset,
                                                                                 const std::string& scaleFactor)
{
    boost::shared_ptr<MetGmHDTag> hdtag =
            MetGmHDTag::createMetGmDimensionsTagForWriting(pCdmReader, pVariable);

    switch(hdtag->asShort()) {
        case MetGmHDTag::HD_2D:
        case MetGmHDTag::HD_3D_T:
            {
                std::string mgmUnits;

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
//                        MGM_CHECK_POINT()
                        mgmUnits = "m";
                    } else {
//                        MGM_CHECK_POINT()
                        mgmUnits = "hPa";
                    }
                } else {
                    mgmUnits = std::string(mgm_get_param_unit(pg3->p_id(), *(pg3->mgmHandle())));
                }

//                MetGmProfilingTimer timer;

                const CDM& cdmRef = pCdmReader->getCDM();
                const std::string varName = pVariable->getName();

                float oldScale  = 1.0;
                float oldOffset = 0.0;
                float oldFill   = MIFI_UNDEFINED_F;

                CDMAttribute attr;
                if (cdmRef.getAttribute(varName, "scale_factor", attr)) {
                    oldScale = attr.getData()->asConstFloat()[0];
                }
                if (cdmRef.getAttribute(varName, "add_offset", attr)) {
                    oldOffset = attr.getData()->asConstFloat()[0];
                }
                if(cdmRef.getAttribute(varName, "_FillValue", attr)) {
                    oldFill = attr.getData()->asDouble()[0];
                }

                double unitsScale = 1.0;
                double unitsOffset = 0.0;

                Units unitsConvertor;
//                MGM_CHECK_POINT()
                if(unitsConvertor.areConvertible(cdmRef.getUnits(varName), mgmUnits)) {
                    unitsConvertor.convert(cdmRef.getUnits(varName), mgmUnits , unitsScale, unitsOffset);
                } else {
                    std::string msg(" can't convert from ");
                    msg.append(cdmRef.getUnits(varName)).append(" to ").append(mgmUnits).append(" for variable ").append(varName).append(" -- excluding");
                    MGM_MESSAGE_POINT(msg)
                    boost::shared_array<float> empty;
                    return boost::shared_ptr<MetGmGroup5Ptr> (new MetGmGroup5Ptr(pg3, hdtag, empty));
                }
//              MGM_CHECK_POINT()

                float newFill = fillValue.empty() ? 9999.0f : boost::lexical_cast<float>(fillValue);
                float newScale  = scaleFactor.empty() ? 1.0 : boost::lexical_cast<float>(scaleFactor);
                float newOffset = addOffset.empty()   ? 0.0 : boost::lexical_cast<float>(addOffset);

                boost::shared_ptr<Data> raw_data  = pCdmReader->getData(varName);

                boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(pg3, hdtag, raw_data->asConstFloat()));

                gp5->toMetGmLayout(oldFill, oldScale, oldOffset, newFill, newScale, newOffset, unitsScale, unitsOffset);

//                MGM_MESSAGE_POINT(timer.elapsedToString().append(" for kb = ").append(boost::lexical_cast<std::string>(raw_data->size() * raw_data->bytes_for_one() / 1024)))

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

    return boost::shared_ptr<MetGmGroup5Ptr>(new MetGmGroup5Ptr(pg3, hdtag, boost::shared_array<float>(0), fillValue));
}

    boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForReading(const boost::shared_ptr<MetGmGroup3Ptr> gp3,
                                                                                     const boost::shared_ptr<MetGmHDTag>     hdTag)
    {
        switch(hdTag->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D_T:
                {
//                    MetGmProfilingTimer timer;

                    boost::shared_array<float> data(new float[hdTag->totalSize()]);

                    MGM_THROW_ON_ERROR(mgm_read_group5(*gp3->mgmHandle()->fileHandle(), *gp3->mgmHandle(), data.get()))

//                    MGM_MESSAGE_POINT(timer.elapsedToString().append("for kb = ").append(boost::lexical_cast<std::string>(hdTag->totalSize() * sizeof(float) / 1024)))

                    MGM_THROW_ON_ERROR(mgm_param_is_convertible(gp3->p_id(), *gp3->mgmHandle()->version()))

                    boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(gp3, hdTag, data));

                    // from METGM to Fimex layout
                    gp5->toFimexLayout();

//                    MGM_MESSAGE_POINT(timer.elapsedToString().append("for kb = ").append(boost::lexical_cast<std::string>(hdTag->totalSize() * sizeof(float) / 1024)))

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
                                   + " for p_id ="
                                   + boost::lexical_cast<std::string>(gp3->p_id()));
        }

        return  boost::shared_ptr<MetGmGroup5Ptr> (new MetGmGroup5Ptr(gp3, hdTag, boost::shared_array<float>(0)));
    }

    void MetGmGroup5Ptr::dumpFimexLayout()
    {
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
