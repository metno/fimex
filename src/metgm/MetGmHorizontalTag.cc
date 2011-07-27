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
#include "../../include/metgm/MetGmHorizontalTag.h"
#include "../../include/metgm/MetGmGroup3Ptr.h"

// fimex
//
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex {

    boost::shared_ptr<MetGmXTag> MetGmHorizontalTag::createMetGmXTag(boost::shared_ptr<CDMReader>& pCdmReader)
    {
        if(!pCdmReader.get())
            throw CDMException("createMetGmXTag: pCdmReader is null");

        const CDM& cdmRef = pCdmReader->getCDM();

        std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);
        if(coordSys.size() != 1)
            throw CDMException("found more than one coor sys");

        CoordinateSystem::ConstAxisPtr xAxis = coordSys.at(0)->getGeoXAxis();

        boost::shared_ptr<Data> xData;

        if(xAxis->getAxisType() != CoordinateAxis::Lon)
            throw CDMException("can't find longitude axis -- check for projection");

        xData = pCdmReader->getScaledData(xAxis->getName());

        boost::shared_ptr<MetGmXTag> XTag
                = boost::shared_ptr<MetGmXTag>(new MetGmXTag());

        XTag->numberOfPoints_ = xData->size();

        XTag->extractHorizontalPoints(xData);

        XTag->center_ = (XTag->horizontalPoints_.at(XTag->horizontalPoints_.size() - 1) + XTag->horizontalPoints_.at(0)) / 2.0;

        XTag->distance_ = XTag->horizontalPoints_.at(1) - XTag->horizontalPoints_.at(0);

        if(XTag->distance_ < 0)
            throw CDMException("metgm is not supporting negative distances on longitude axis");

        return XTag;
    }

    boost::shared_ptr<MetGmYTag> MetGmHorizontalTag::createMetGmYTag(boost::shared_ptr<CDMReader>& pCdmReader)
    {
        if(!pCdmReader.get())
            throw CDMException("createMetGmXTag: pCdmReader is null");

        const CDM& cdmRef = pCdmReader->getCDM();

        std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);
        if(coordSys.size() != 1)
            throw CDMException("found more than one coor sys");

        CoordinateSystem::ConstAxisPtr yAxis = coordSys.at(0)->getGeoYAxis();

        if(yAxis->getAxisType() != CoordinateAxis::Lat)
            throw CDMException("can't find longitude axis -- check for projection");

        boost::shared_ptr<Data> yData;
        if(yAxis->hasData()) {
            yData = yAxis->getData();
        } else {
            yData = pCdmReader->getData(yAxis->getName());
        }

        boost::shared_ptr<MetGmYTag> YTag
                = boost::shared_ptr<MetGmYTag>(new MetGmYTag);


        YTag->numberOfPoints_ = yData->size();

        YTag->extractHorizontalPoints(yData);

        YTag->center_ = (YTag->horizontalPoints_.at(YTag->horizontalPoints_.size() - 1) + YTag->horizontalPoints_.at(0)) / 2.0;

        YTag->distance_ = YTag->horizontalPoints_.at(1) - YTag->horizontalPoints_.at(0);

        if(YTag->distance_ < 0)
            throw CDMException("metgm is not supporting negative distances on longitude axis");

        return YTag;
    }

    boost::shared_ptr<MetGmXTag> MetGmHorizontalTag::createMetGmXTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVariable)
    {
        if(!pVariable)
            throw CDMException("pVar is null");

        if(!pCdmReader.get())
            throw CDMException("pCdmReader is null");

        boost::shared_ptr<MetGmXTag> XTag;

        const CDM& cdmRef = pCdmReader->getCDM();

        std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);

        std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(pVariable->getName()));
        if (varSysIt != coordSys.end()) {
            if((*varSysIt)->isSimpleSpatialGridded()) {

                CoordinateSystem::ConstAxisPtr xAxis = (*varSysIt)->getGeoXAxis();

                if(!xAxis.get()) {
//                    std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                              << " x axis NOT existing for " << pVariable->getName() << std::endl;
                    return boost::shared_ptr<MetGmXTag>();
                } else {
//                    std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                              << " x axis IS existing for " << pVariable->getName() << std::endl;
                }

                XTag = boost::shared_ptr<MetGmXTag>(new MetGmXTag);

                boost::shared_ptr<Data> data = pCdmReader->getScaledDataInUnit(xAxis->getName(), "degree");

                XTag->numberOfPoints_ = data->size();

                XTag->extractHorizontalPoints(data);

                XTag->center_ = (XTag->horizontalPoints_.at(XTag->horizontalPoints_.size() - 1) + XTag->horizontalPoints_.at(0)) / 2.0;

                XTag->distance_ = XTag->horizontalPoints_.at(1) - XTag->horizontalPoints_.at(0);

                if(XTag->distance_ < 0)
                    throw CDMException("metgm is not supporting negative distances on longitude axis");
            }
        } else {
        }

        return XTag;
    }

    boost::shared_ptr<MetGmYTag> MetGmHorizontalTag::createMetGmYTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVariable)
    {
        if(!pVariable)
            throw CDMException("pVar is null");

        if(!pCdmReader.get())
            throw CDMException("pCdmReader is null");

        boost::shared_ptr<MetGmYTag> YTag;

        const CDM& cdmRef = pCdmReader->getCDM();

        std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);

        std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(pVariable->getName()));
        if (varSysIt != coordSys.end()) {
            if((*varSysIt)->isSimpleSpatialGridded()) {

                CoordinateSystem::ConstAxisPtr yAxis = (*varSysIt)->getGeoYAxis();

                if(!yAxis.get()) {
//                    std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                              << " y axis NOT existing for " << pVariable->getName() << std::endl;
                    return boost::shared_ptr<MetGmYTag>();
                } else {
//                    std::cerr << __FILE__ << " @ " << __FUNCTION__ << " @ " << __LINE__ << " : "
//                              << " y axis IS existing for " << pVariable->getName() << std::endl;
                }

                YTag = boost::shared_ptr<MetGmYTag>(new MetGmYTag);

                boost::shared_ptr<Data> data = pCdmReader->getScaledDataInUnit(yAxis->getName(), "degree");

                YTag->numberOfPoints_ = data->size();

                YTag->extractHorizontalPoints(data);

                YTag->center_ = (YTag->horizontalPoints_.at(YTag->horizontalPoints_.size() - 1) + YTag->horizontalPoints_.at(0)) / 2.0;

                YTag->distance_ = YTag->horizontalPoints_.at(1) - YTag->horizontalPoints_.at(0);

                if(YTag->distance_ < 0)
                    throw CDMException("metgm is not supporting negative distances on latitude axis");
            }
        } else {
        }

        return YTag;
    }

    boost::shared_ptr<MetGmXTag> MetGmHorizontalTag::createMetGmXTag(boost::shared_ptr<MetGmGroup3Ptr>& pg3)
    {
        boost::shared_ptr<MetGmXTag> XTag = boost::shared_ptr<MetGmXTag>(new MetGmXTag);

        XTag->center_         = pg3->cx();
        XTag->distance_       = pg3->dx();
        XTag->numberOfPoints_ = pg3->nx();

        double x0 = XTag->cx() - (XTag->nx() - 1) * XTag->dx() / 2.0;

        for (int index = 0; index < XTag->nx(); ++index) {
            XTag->horizontalPoints_.push_back(x0 + index * XTag->dx());
        }

        return XTag;
    }

    boost::shared_ptr<MetGmYTag> MetGmHorizontalTag::createMetGmYTag(boost::shared_ptr<MetGmGroup3Ptr>& pg3)
    {
        boost::shared_ptr<MetGmYTag> YTag = boost::shared_ptr<MetGmYTag>(new MetGmYTag);

        YTag->center_         = pg3->cy();
        YTag->distance_       = pg3->dy();
        YTag->numberOfPoints_ = pg3->ny();

        double y0 = YTag->cy() - (YTag->ny() - 1) * YTag->dy() / 2.0;

        for(size_t index = 0; index < YTag->ny(); ++index) {
            YTag->horizontalPoints_.push_back(y0 + index * YTag->dy());
        }

        return YTag;
    }
}

