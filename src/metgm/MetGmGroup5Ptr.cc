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

// standard
#include <cstdio>
#include <cmath>

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


/*
 * transforming ND index to 1D index:
 *
 * (x, y)         -> x + (nx * y)
 * (y, x)         -> y + (ny * x)
 *
 * (x, y, z)      -> x + (nx * y) + (nx * ny) * z
 * (z, x, y)      -> z + (nz * x) + (nz * nx) * y
 *
 * (x, y, z, t)   -> x + (nx * y) + (nx * ny) * z + (nx * ny * nz) * t
 * (z, x, y, t)   -> z + (nz * x) + (nz * nx) * y + (nx * ny * nz) * t
 */


/*
 * (z, x, y, t) = (x, y, z, t)
 * (z, x, y, t)   -> z + nz * x + Nzx * y + Nxyz * t -> z + zLeap + zxLeap + xyzLeap
 * (x, y, z, t)   -> x + nx * y + Nxy * z + Nxyz * t -> x + xLeap + xyLeap + xyzLeap
 *
 *
 *     reindex data - all slices
 *     Fimex               METGM
 * (x, y, z, slice) -> (z, x, y, slice)
 */
void MetGmGroup5Ptr::toMetGmLayout()
{
    if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
        return;

    boost::shared_array<float> dataT(new float[hdTag_->totalSize()]);

    float* pos = data_.get();
    float* posT = dataT.get();

    size_t nz = hdTag_->zSize();
    size_t ny = hdTag_->ySize();
    size_t nx = hdTag_->xSize();
    size_t nt = hdTag_->tSize();

    size_t Nxy  = nx * ny;
    size_t Nzx  = nz * nx;
    size_t Nxyz = Nxy * nz;


    size_t xyzLeap = -Nxyz;

    for(size_t t = 0; t < nt; ++t) {

        xyzLeap += Nxyz;

        size_t xLeap  = -nx;
        size_t zxLeap = -Nzx;

        for(size_t y = 0; y < ny; ++y) {

            xLeap += nx;
            zxLeap += Nzx;

            size_t zLeapF = -nz;

            for(size_t x = 0; x < nx; ++x) {

                zLeapF += nz;

                size_t xyLeapF = -Nxy;
                size_t xyLeapB = nz * Nxy;
                // from front and back
                //   at the same time
                // to meet in the middle
                size_t midz = nz / 2 + nz % 2;
                for(size_t z = 0; z < midz; ++z) {

                    xyLeapF += Nxy;
                    xyLeapB -= Nxy;

                    float valueFz = *(pos + x + xLeap + xyLeapF + xyzLeap);
                    *(posT + z + zLeapF + zxLeap + xyzLeap) = valueFz;
                    float valueBz = *(pos + x + xLeap + xyLeapB + xyzLeap);
                    *(posT + (nz - 1 - z) + zLeapF + zxLeap + xyzLeap) = valueBz;

                } // z
            } // x
        } // y
    } // slice

    data_.swap(dataT);
}

/*
 *     reindex data - all slices
 *     METGM               Fimex
 * (z, x, y, slice) -> (x, y, z, slice)
 */
void MetGmGroup5Ptr::toFimexLayout()
{
    if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
        return;

    boost::shared_array<float> dataT(new float[hdTag_->totalSize()]);

    float* pos = data_.get();
    float* posT = dataT.get();

    size_t nz = hdTag_->zSize();
    size_t ny = hdTag_->ySize();
    size_t nx = hdTag_->xSize();
    size_t nt = hdTag_->tSize();

    size_t Nxy  = nx * ny;
    size_t Nzx  = nz * nx;
    size_t Nxyz = Nxy * nz;


    size_t xyzLeap = -Nxyz;

    for(size_t t = 0; t < nt; ++t) {

        xyzLeap += Nxyz;

        size_t xLeap  = -nx;
        size_t zxLeap = -Nzx;

        for(size_t y = 0; y < ny; ++y) {

            xLeap += nx;
            zxLeap += Nzx;

            size_t xyLeap = -Nxy;

            for(size_t z = 0; z < nz; ++z) {

                xyLeap += Nxy;

                size_t zLeapF = -nz;
                size_t zLeapB = nx * nz;

                // from front and back
                //   at the same time
                // to meet in the middle
                size_t midx = nx / 2 + nx % 2;
                for(size_t x = 0; x < midx; ++x) {

                    zLeapF += nz;
                    zLeapB -= nz;

                    float valueFx = *(pos + z + zLeapF + zxLeap + xyzLeap);
                    *(posT + x + xLeap + xyLeap + xyzLeap) = valueFx;
                    float valueBx = *(pos + z + zLeapB + zxLeap + xyzLeap);
                    *(posT + (nx - 1 - x) + xLeap + xyLeap + xyzLeap) = valueBx;
                } //x
            } // z
        } // y
    } // slice

    data_.swap(dataT);
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
                        mgmUnits = "m";
                    } else {
                        mgmUnits = "hPa";
                    }
                } else {
                    mgmUnits = std::string(mgm_get_param_unit(pg3->p_id(), *(pg3->mgmHandle())));
                }

                const CDM& cdmRef = pCdmReader->getCDM();
                const std::string varName = pVariable->getName();

                Units unitsConvertor;
                if(! unitsConvertor.areConvertible(cdmRef.getUnits(varName), mgmUnits)) {
                    std::string msg(" can't convert from ");
                    msg.append(cdmRef.getUnits(varName)).append(" to ").append(mgmUnits).append(" for variable ").append(varName).append(" -- excluding");
                    MGM_MESSAGE_POINT(msg)
                    boost::shared_array<float> empty;
                    return boost::shared_ptr<MetGmGroup5Ptr> (new MetGmGroup5Ptr(pg3, hdtag, empty));
                }

                boost::shared_ptr<Data> raw_data  = pCdmReader->getScaledDataInUnit(varName, mgmUnits);

                boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(pg3, hdtag, raw_data->asFloat()));

                /*
                 *     reindex data - all slices
                 *     Fimex               METGM
                 * (x, y, z, slice) -> (z, x, y, slice)
                 */
                gp5->toMetGmLayout();

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
                    boost::shared_array<float> data(new float[hdTag->totalSize()]);

                    boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(gp3, hdTag, data));

                    gp5->sOffset_ = ftell(gp3->mgmHandle()->fileHandle()->handle());
                    MGM_THROW_ON_ERROR(mgm_read_group5(*gp3->mgmHandle()->fileHandle(), *gp3->mgmHandle(), data.get()))
                    gp5->eOffset_ = ftell(gp3->mgmHandle()->fileHandle()->handle());

                    MGM_THROW_ON_ERROR(mgm_param_is_convertible(gp3->p_id(), *gp3->mgmHandle()->version()))

                    /*
                     *           reindex data
                     *     METGM               Fimex
                     * (z, x, y, slice) -> (x, y, z, slice)
                     */
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
                                   + " for p_id ="
                                   + boost::lexical_cast<std::string>(gp3->p_id()));
        }

        return  boost::shared_ptr<MetGmGroup5Ptr> (new MetGmGroup5Ptr(gp3, hdTag, boost::shared_array<float>(0)));
    }

    boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForSlicedReading(const boost::shared_ptr<MetGmGroup3Ptr> gp3,
                                                                                           const boost::shared_ptr<MetGmHDTag>     hdTag)
    {
        switch(hdTag->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D_T:
                {
                    boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(gp3, hdTag, boost::shared_array<float>(0)));

                    /**
                      * must skip data to keep METGM C API lib satisifed
                      */

                    gp5->sOffset_ = ftell(gp3->mgmHandle()->fileHandle()->handle());
                    MGM_THROW_ON_ERROR(mgm_skip_group5(*gp3->mgmHandle()->fileHandle(), *gp3->mgmHandle()))
                    gp5->eOffset_ = ftell(gp3->mgmHandle()->fileHandle()->handle());

                    return  gp5;
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

    /*
     *  reindex data - several slices
     *     METGM               Fimex
     * (z, x, y, slice) -> (x, y, z, slice)
     */
    void MetGmGroup5Ptr::slicesToFimexLayout(boost::shared_array<float>& slices, size_t numberOfSlices)
    {
        if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
            return;

        boost::shared_array<float> dataT(new float[hdTag_->sliceSize() * numberOfSlices]);

        float* pos = slices.get();
        float* posT = dataT.get();

        size_t nz = hdTag_->zSize();
        size_t ny = hdTag_->ySize();
        size_t nx = hdTag_->xSize();

        size_t Nxy  = nx * ny;
        size_t Nzx  = nz * nx;
        size_t Nxyz = Nxy * nz;

        size_t xyzLeap = -Nxyz;

        for(size_t slice_index = 0; slice_index < numberOfSlices; ++slice_index) {

            xyzLeap += Nxyz;

            size_t xLeap  = -nx;
            size_t zxLeap = -Nzx;

            for(size_t y = 0; y < ny; ++y) {

                xLeap += nx;
                zxLeap += Nzx;

                size_t xyLeap = -Nxy;

                for(size_t z = 0; z < nz; ++z) {

                    xyLeap += Nxy;

                    size_t zLeapF = -nz;
                    size_t zLeapB = nx * nz;

                    size_t midx = nx / 2 + nx % 2;
                    for(size_t x = 0; x < midx; ++x) {

                        zLeapF += nz;
                        zLeapB -= nz;

                        float valueFx = *(pos + z + zLeapF + zxLeap + xyzLeap);
                        *(posT + x + xLeap + xyLeap + xyzLeap) = valueFx;

                        float valueBx = *(pos + z + zLeapB + zxLeap + xyzLeap);
                        *(posT + (nx - 1 - x) + xLeap + xyLeap + xyzLeap) = valueBx;
                    } // x
                } // z
            } // y
        } // slice

        slices.swap(dataT);
    }

    boost::shared_array<float> MetGmGroup5Ptr::readDataSlices(size_t pos, size_t numberOfSlices)
    {
        assert( (pos + numberOfSlices - 1) >= 1 && (pos + numberOfSlices - 1) <= hdTag_->tSize());

        switch(hdTag_->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_2D_T:
            case MetGmHDTag::HD_3D_T:
                {
                    FILE* fh = fopen(pGp3_->mgmHandle()->fileHandle()->fileName().c_str(), "rb");;
                    if(!fh) {
                        return boost::shared_array<float>(0);
                    }

                    mgm_handle* mh = mgm_new_handle();
                    if(!mh) {
                        fclose(fh);
                        return boost::shared_array<float>(0);
                    }

                    mgm_group3* gp3 = mgm_new_group3();
                    if(!gp3) {
                        mgm_free_handle(mh);
                        fclose(fh);
                        return boost::shared_array<float>(0);
                    }

                    int call_result = MGM_OK;

                    int n = 0;
                    int np = 0;
                    int ndp = 0;

                    call_result = mgm_read_header(fh, mh);

                    if(call_result != MGM_OK) {
                        mgm_free_group3(gp3);
                        mgm_free_handle(mh);
                        fclose(fh);
                        return boost::shared_array<float>(0);
                    }

                    np = mgm_get_number_of_params(mh);
                    ndp = mgm_get_number_of_dist_params(mh);

                    for (n = 0; n < np; n++)
                    {
                        call_result = mgm_read_group3(fh, mh, gp3);

                        if(call_result != MGM_OK) {
                            mgm_free_group3(gp3);
                            mgm_free_handle(mh);
                            fclose(fh);
                            return boost::shared_array<float>(0);
                        }

                        if (mgm_get_pz(gp3) > 0) {
                            call_result = mgm_skip_group4(fh, mh);
                            if(call_result != MGM_OK) {
                                mgm_free_group3(gp3);
                                mgm_free_handle(mh);
                                fclose(fh);
                                return boost::shared_array<float>(0);
                            }

                        }
                        long cOffset = ftell(fh);
                        if(cOffset < sOffset_) {
                            // skip group5 data
                            int nt = mgm_get_nt(gp3);
                            for(int slice_index = 1; slice_index <= nt; ++slice_index)
                            {
                                size_t cSlicePos = 0;
                                call_result = mgm_skip_group5_slice(fh, mh, &cSlicePos);
                                if(call_result != MGM_OK) {
                                    mgm_free_group3(gp3);
                                    mgm_free_handle(mh);
                                    fclose(fh);
                                    return boost::shared_array<float>(0);
                                }
                            }
                        } else if(cOffset > sOffset_) {
                            // something wrong
                            mgm_free_group3(gp3);
                            mgm_free_handle(mh);
                            fclose(fh);
                            return boost::shared_array<float>(0);
                        } else {
                            boost::shared_array<float> data(new float[hdTag_->sliceSize() * numberOfSlices]);

                            size_t slices_read = 0;

                            for(size_t slice_index = 1; slice_index <= hdTag_->tSize(); ++slice_index)
                            {
                                size_t cSlicePos = -1;
                                call_result = mgm_read_group5_slice(fh, mh, data.get() + (slices_read * hdTag_->sliceSize()), &cSlicePos);
                                if(call_result != MGM_OK) {
                                    mgm_free_group3(gp3);
                                    mgm_free_handle(mh);
                                    fclose(fh);
                                    return boost::shared_array<float>(0);
                                }
                                if(cSlicePos >= pos && cSlicePos < pos + numberOfSlices -1) {
                                    ++slices_read;
                                    continue;
                                } else if(cSlicePos == pos + numberOfSlices -1) {
                                    mgm_free_group3(gp3);
                                    mgm_free_handle(mh);
                                    fclose(fh);

                                    MGM_THROW_ON_ERROR(mgm_param_is_convertible(pGp3_->p_id(), *pGp3_->mgmHandle()->version()))

                                    /*
                                     * reindex data - slice
                                     *   METGM      Fimex
                                     * (z, x, y -> (x, y, z)
                                     */
                                    slicesToFimexLayout(data, numberOfSlices);

                                    return data;
                                }
                            }
                        }
                    }

                    mgm_free_group3(gp3);
                    mgm_free_handle(mh);
                    fclose(fh);
                    return  boost::shared_array<float>(0);
                }
                break;
            case MetGmHDTag::HD_0D:
            case MetGmHDTag::HD_0D_T:
            case MetGmHDTag::HD_1D:
            case MetGmHDTag::HD_1D_T:
            case MetGmHDTag::HD_3D:
            default:
                throw CDMException(  std::string(__FUNCTION__) + std::string(": dimensionality not supported yet :")
                                   + hdTag_->asString()
                                   + " for p_id ="
                                   + boost::lexical_cast<std::string>(pGp3_->p_id()));
        }

        return  boost::shared_array<float>(0);
    }

    boost::shared_ptr<MetGmGroup5Ptr> MetGmGroup5Ptr::createMetGmGroup5PtrForSlicedWriting(const boost::shared_ptr<CDMReader> pCdmReader,
                                                                                           const CDMVariable* pVariable,
                                                                                           const boost::shared_ptr<MetGmGroup3Ptr> pg3)
    {
        boost::shared_ptr<MetGmHDTag> hdtag =
                MetGmHDTag::createMetGmDimensionsTagForWriting(pCdmReader, pVariable);

        std::string mgmUnits;

        switch(hdtag->asShort()) {
            case MetGmHDTag::HD_2D:
            case MetGmHDTag::HD_3D_T:
                {
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
                        } else {
                            mgmUnits = "hPa";
                        }
                    } else {
                        mgmUnits = std::string(mgm_get_param_unit(pg3->p_id(), *(pg3->mgmHandle())));
                    }

                    const CDM& cdmRef = pCdmReader->getCDM();
                    const std::string varName = pVariable->getName();

                    Units unitsConvertor;
                    if(! unitsConvertor.areConvertible(cdmRef.getUnits(varName), mgmUnits)) {
                        std::string msg(" can't convert from ");
                        msg.append(cdmRef.getUnits(varName)).append(" to ").append(mgmUnits).append(" for variable ").append(varName).append(" -- excluding");
                        MGM_MESSAGE_POINT(msg)
//                        boost::shared_array<float> empty;
//                        boost::shared_ptr<MetGmGroup5Ptr> mgm5group(new MetGmGroup5Ptr(pg3, hdtag, empty));
//                        return mgm5group;
                          return boost::shared_ptr<MetGmGroup5Ptr>();
                    }

                    boost::shared_ptr<MetGmGroup5Ptr> gp5(new MetGmGroup5Ptr(pg3, hdtag, boost::shared_array<float>(0)));
                    gp5->units_ = mgmUnits;

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

        return boost::shared_ptr<MetGmGroup5Ptr>(new MetGmGroup5Ptr(pg3, hdtag, boost::shared_array<float>(0)));
    }

    /*
     * reindex data - one slice
     *   Fimex      METGM
     * (x, y, z) -> (z, x, y)
     */
    void MetGmGroup5Ptr::sliceToMetGmLayout(boost::shared_array<float>& slice)
    {
        if(hdTag_->asShort() !=  MetGmHDTag::HD_3D_T)
            return;

        boost::shared_array<float> dataT(new float[hdTag_->sliceSize()]);

        float* pos = slice.get();
        float* posT = dataT.get();

        size_t nz = hdTag_->zSize();
        size_t ny = hdTag_->ySize();
        size_t nx = hdTag_->xSize();

        size_t Nxy  = nx * ny;
        size_t Nzx  = nz * nx;

        size_t xLeap  = -nx;
        size_t zxLeap = -Nzx;

        for(size_t y = 0; y < ny; ++y) {

            xLeap += nx;
            zxLeap += Nzx;

            size_t zLeapF = -nz;

            for(size_t x = 0; x < nx; ++x) {

                zLeapF += nz;

                size_t xyLeapF = -Nxy;
                size_t xyLeapB = nz * Nxy;

                size_t midz = nz / 2 + nz % 2;
                for(size_t z = 0; z < midz; ++z) {

                    xyLeapF += Nxy;
                    xyLeapB -= Nxy;

                    float valueFz = *(pos + x + xLeap + xyLeapF);
                    *(posT + z + zLeapF + zxLeap) = valueFz;
                    float valueBz = *(pos + x + xLeap + xyLeapB);
                    *(posT + (nz - 1 - z) + zLeapF + zxLeap) = valueBz;
                } // z

            } // x

        } // y

        slice.swap(dataT);
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
