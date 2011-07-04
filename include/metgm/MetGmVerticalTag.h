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

#ifndef METGM_VERTICALTAG_H
#define METGM_VERTICALTAG_H

// fimex
//
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDMDimension.h"
#include "fimex/CDMAttribute.h"
#include "fimex/CDMException.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"

// boost
//
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

// standard
//
#include <ctime>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>

namespace MetNoFimex {

    class MetGmVerticalTag {
    public:

        static boost::shared_ptr<MetGmVerticalTag> createMetGmVerticalTag(const boost::shared_ptr<CDMReader> pCdmReader, const CDMVariable* pVar);

        inline unsigned int               nz()       { return nz_; }
        inline unsigned int               pr()       { return pr_;}
        inline unsigned int               pz()       { return pz_; }
        inline boost::shared_array<float> points()   { return points_; }

    protected:

        inline MetGmVerticalTag() : nz_(0), pr_(0), pz_(0) { }

        inline bool hasNegativePoints() {
            std::less_equal<float> leq;
            return std::find_if(&points_[0], &points_[nz_], boost::bind( leq, _1, 0 ) ) != &points_[nz_];
        }

        inline void extractVerticalPoints(const boost::shared_ptr<Data>& data)
        {
            points_ = data->asConstFloat();

            if(hasNegativePoints())
                throw CDMException("negative values on the vertical axis not supported");
        }

        unsigned int                nz_;
        unsigned int                pr_;
        unsigned int                pz_;
        boost::shared_array<float>  points_;
    };

    boost::shared_ptr<MetGmVerticalTag> MetGmVerticalTag::createMetGmVerticalTag(const boost::shared_ptr<CDMReader> pCdmReader, const CDMVariable* pVar)
    {
        if(!pVar)
            throw CDMException("pVar is null createMetGmVerticalTag");

        if(!pCdmReader.get())
            throw CDMException("createMetGmXTag: pCdmReader is null");

        boost::shared_ptr<MetGmVerticalTag> VTag =
                boost::shared_ptr<MetGmVerticalTag>();

        const CDM& cdmRef = pCdmReader->getCDM();

        std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);

        std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(pVar->getName()));
        if (varSysIt != coordSys.end()) {
            if((*varSysIt)->isSimpleSpatialGridded()) {
                CoordinateSystem::ConstAxisPtr zAxis = (*varSysIt)->getGeoZAxis();

                boost::shared_ptr<Data> data;

                data = pCdmReader->getData(zAxis->getName());

                /* do something with the data */
                if(zAxis->getAxisType() == CoordinateAxis::Pressure) {
                    VTag->pr_ = 2;
                    data = pCdmReader->getScaledDataInUnit(zAxis->getName(), "hPa");
                } else if (zAxis->getAxisType() == CoordinateAxis::Height) {
                    VTag->pr_ = 1;
                    data = pCdmReader->getScaledDataInUnit(zAxis->getName(), "m");
                } else {
                    throw CDMException("pr  = 0 not supported yet");
                }
                VTag->pz_ = 1;
                VTag->nz_= data->size();
                VTag->extractVerticalPoints(data);
            }
        } else {
            /* vertical coordinate not found */
            VTag->nz_ = 1;
            VTag->pr_ = 0;
            VTag->pz_ = 1;
            VTag->points_ = boost::shared_array<float>(new float[1]);
            VTag->points_[0] = 0;
        }

        return VTag;
    }

}

//            /**
//              * 1. try to find metgm_pr CDMAttribute
//              * 2. else try parsing the name for _GND or _MSL
//              * 3. if MSL sent exists from before, then check for units to distinguish _GND or _MSL
//              */
//            CDMAttribute metgmPrAttribute;
//            if(cdmRef.getAttribute(pVar->getName(), "metgm_pr", metgmPrAttribute)) {
//                short pr = boost::lexical_cast<short>(metgmPrAttribute.getStringValue());
//                MGM_THROW_ON_ERROR(gp3->set_pr(pr))
//            } else if(pVar->getName().find("_MSL") != std::string::npos) {
//                MGM_THROW_ON_ERROR(gp3->set_pr(0))
//            } else if(pVar->getName().find("_GND") != std::string::npos) {
//                MGM_THROW_ON_ERROR(gp3->set_pr(1))
//            } else {
//                // check unit for the dimension
//                CDMAttribute metgmUnitsAttribute;
//                if(cdmRef.getAttribute(zDimension->getName(), "units", metgmUnitsAttribute)) {
//                    std::string unitsName = metgmUnitsAttribute.getStringValue();
//                    if(unitsName.find("Pa") != std::string::npos) {
//                        MGM_THROW_ON_ERROR(gp3->set_pr(2))
//                    } else if(unitsName.find("m") != std::string::npos) {
//                        if(pid2CdmVariablesMMap_.find(0) != pid2CdmVariablesMMap_.end()) {
//                            // we have MSL pr = 1 (we are dealing with GND type)
//                            MGM_THROW_ON_ERROR(gp3->set_pr(1))
//                        } else {
//                            // no MSL in CDM model (pr = 0 if units not Pa)
//                            MGM_THROW_ON_ERROR(gp3->set_pr(0))
//                        }
//                    } else {
//                        assert(0); // some todo here
//                    }
//                } else  {
//                    assert(0); // some todo here
//                }
//            }
#endif // METGM_VERTICALTAG_H
