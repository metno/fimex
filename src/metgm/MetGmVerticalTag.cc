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
#include "MetGmVerticalTag.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmFileHandlePtr.h"
#include "MetGmHandlePtr.h"
#include "MetGmUtils.h"


// fimex
//
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDMException.h"
#include "fimex/coordSys/CoordinateSystem.h"

// boost
//
#include <boost/bind.hpp>
#include <boost/algorithm/string.hpp>

namespace MetNoFimex {

std::shared_ptr<MetGmVerticalTag> MetGmVerticalTag::createMetGmVerticalTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable)
{
    std::shared_ptr<MetGmVerticalTag> VTag;

    const CoordinateSystem_cp_v coordSys = listCoordinateSystems(pCdmReader);

    CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordSys, pVariable->getName());
    if (cs.get()) {
        if(cs->isSimpleSpatialGridded()) {

            CoordinateAxis_cp zAxis = cs->getGeoZAxis();

            if(!zAxis.get()) {
                return std::shared_ptr<MetGmVerticalTag>();
            }

            VTag = std::shared_ptr<MetGmVerticalTag>(new MetGmVerticalTag());

            DataPtr data;

            data = pCdmReader->getData(zAxis->getName());

            /* do something with the data */
            if(zAxis->getAxisType() == CoordinateAxis::Pressure) {
                VTag->pr_ = 2;
                data = pCdmReader->getScaledDataInUnit(zAxis->getName(), "hPa");
            } else if (zAxis->getAxisType() == CoordinateAxis::Height) {
                VTag->pr_ = 1; // as default
                if(boost::algorithm::ends_with(pVariable->getName(), "_MSL")) {
                    VTag->pr_ = 0;
                }
                data = pCdmReader->getScaledDataInUnit(zAxis->getName(), "m");
            }
            VTag->pz_ = 1;
            VTag->nz_= data->size();
            VTag->extractVerticalPoints(data);
        }
    } else {
        /* vertical coordinate not found */
//            VTag->nz_ = 1;
//            VTag->pr_ = 0;
//            VTag->pz_ = 1;
//            VTag->points_ = boost::shared_array<float>(new float[1]);
//            VTag->points_[0] = 0;
    }

    return VTag;
}

std::shared_ptr<MetGmVerticalTag> MetGmVerticalTag::createMetGmVerticalTagForReading(std::shared_ptr<MetGmGroup3Ptr> pGp3,
                                                                                     std::shared_ptr<MetGmVerticalTag> prevTag)
{
    std::shared_ptr<MetGmVerticalTag> VTag = std::shared_ptr<MetGmVerticalTag>(new MetGmVerticalTag);

    if (pGp3->pz() == 0) {
        /**
         * same Z data as in previous parameter do deep copy
         */
        if (prevTag.get() == 0 || prevTag->nz() == 0 || prevTag->points().get() == 0)
            throw CDMException("we have pz = 0 and previous vTag = 0");

        VTag->nz_ = pGp3->nz();
        VTag->pz_ = pGp3->pz();
        VTag->pr_ = pGp3->pr();

        // but take the data from prev
        VTag->points().reset(new float[VTag->nz()]);

        memcpy(VTag->points().get(), prevTag->points().get(), prevTag->nz() * sizeof(float));

        return VTag; // return copy
    }

    if (pGp3->p_id() == 0) {

        VTag->nz_ = 1;
        VTag->pr_ = 0;
        VTag->pz_ = 1;

        VTag->points_.reset(new float[VTag->nz()]);
        VTag->points_[0] = 0.0;

        MGM_THROW_ON_ERROR(mgm_skip_group4(*pGp3->mgmHandle()->fileHandle(), *pGp3->mgmHandle()))
        return VTag;
    }

    VTag->nz_ = pGp3->nz();
    VTag->pz_ = pGp3->pz();
    VTag->pr_ = pGp3->pr();

    VTag->points_.reset(new float[VTag->nz()]);

    MGM_THROW_ON_ERROR(mgm_read_group4(*pGp3->mgmHandle()->fileHandle(), *pGp3->mgmHandle(), VTag->points_.get()))

    return VTag;
    }

    bool MetGmVerticalTag::hasNegativePoints() {
        std::less_equal<float> leq;
        return std::find_if(&points_[0], &points_[nz_], boost::bind( leq, _1, 0 ) ) != &points_[nz_];
    }

    void MetGmVerticalTag::extractVerticalPoints(const DataPtr& data)
    {
        points_ = data->asFloat();
    }

    void MetGmVerticalTag::dump() {
        std::cerr << "dumping Z profile [START]" << std::endl;
        for(size_t index = 0; index < nz_; ++index) {
            std::cerr << "[" << index << "] = " << points_[index] << std::endl;
        }
        std::cerr << "dumping Z profile [END]"   << std::endl;
    }
}
