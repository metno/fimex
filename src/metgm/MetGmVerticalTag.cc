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
#include "../../include/metgm/MetGmVerticalTag.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"
#include "../../include/metgm/MetGmFileHandlePtr.h"
#include "../../include/metgm/MetGmHandlePtr.h"
#include "../../include/metgm/MetGmUtils.h"


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

boost::shared_ptr<MetGmVerticalTag> MetGmVerticalTag::createMetGmVerticalTagForWriting(boost::shared_ptr<CDMReader>& pCdmReader,
                                                                                       const CDMVariable* pVariable)
{
    if(!pVariable)
        throw CDMException("pVar is null createMetGmVerticalTag");

    if(!pCdmReader.get())
        throw CDMException("createMetGmXTag: pCdmReader is null");

    boost::shared_ptr<MetGmVerticalTag> VTag;

    const CDM& cdmRef = pCdmReader->getCDM();

    std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);

    std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(pVariable->getName()));
    if (varSysIt != coordSys.end()) {
        if((*varSysIt)->isSimpleSpatialGridded()) {

            CoordinateSystem::ConstAxisPtr zAxis = (*varSysIt)->getGeoZAxis();

            if(!zAxis.get()) {
//                std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                          << " z axis NOT existing for " << pVariable->getName() << std::endl;
                return boost::shared_ptr<MetGmVerticalTag>();
            } else {
//                std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                          << " z axis IS existing for " << pVariable->getName() << std::endl;
            }

            VTag = boost::shared_ptr<MetGmVerticalTag>(new MetGmVerticalTag());

            boost::shared_ptr<Data> data;

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

    boost::shared_ptr<MetGmVerticalTag> MetGmVerticalTag::createMetGmVerticalTagForReading(boost::shared_ptr<MetGmGroup3Ptr>   pGp3,
                                                                                           boost::shared_ptr<MetGmVerticalTag> prevTag)
    {
        boost::shared_ptr<MetGmVerticalTag> VTag = boost::shared_ptr<MetGmVerticalTag>(new MetGmVerticalTag);

        if(pGp3->pz() == 0) {
            /**
              * same Z data as in previous parameter do deep copy
              */
            if(prevTag.get() == 0 || prevTag->nz() == 0 || prevTag->points().get() == 0)
                throw CDMException("we have pz = 0 and previous vTag = 0");

            VTag->nz_ = pGp3->nz();
            VTag->pz_ = pGp3->pz();
            VTag->pr_ = pGp3->pr();

            // but take the data from prev
            VTag->points().reset(new float[VTag->nz()]);

            memcpy(VTag->points().get(), prevTag->points().get(), prevTag->nz() * sizeof(float));

            return VTag; // return copy
        }

        if(pGp3->p_id() == 0) {

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

    void MetGmVerticalTag::extractVerticalPoints(const boost::shared_ptr<Data>& data)
    {
        points_ = data->asConstFloat();
    }

    void MetGmVerticalTag::dump() {
        std::cerr << "dumping Z profile [START]" << std::endl;
        for(size_t index = 0; index < nz_; ++index) {
            std::cerr << "[" << index << "] = " << points_[index] << std::endl;
        }
        std::cerr << "dumping Z profile [END]"   << std::endl;
    }
}
