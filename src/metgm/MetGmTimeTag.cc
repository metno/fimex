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
#include "../../include/metgm/MetGmTimeTag.h"

// fimex
//
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex {

boost::shared_ptr<MetGmTimeTag> MetGmTimeTag::createMetGmTimeTag(boost::shared_ptr<CDMReader>& pCdmReader)
{
    if(!pCdmReader.get())
        throw CDMException("createMetGmTimeTag: pCdmReader is null");

    const CDM& cdmRef = pCdmReader->getCDM();

    if( !(cdmRef.hasDimension("time") && cdmRef.hasVariable("time")) )
        throw CDMException("createMetGmTimeTag: cdm model doesn't have the 'time'");

    boost::shared_ptr<MetGmTimeTag> TTag =
            boost::shared_ptr<MetGmTimeTag>(new MetGmTimeTag());

    TTag->init(pCdmReader);

    return TTag;
}

boost::shared_ptr<MetGmTimeTag> MetGmTimeTag::createMetGmTimeTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVariable)
{
    if(!pVariable)
        throw CDMException("pVar is null");

    if(!pCdmReader.get())
        throw CDMException("pCdmReader is null");

    boost::shared_ptr<MetGmTimeTag> TTag;

    const CDM& cdmRef = pCdmReader->getCDM();

    std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys = listCoordinateSystems(cdmRef);

    std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(pVariable->getName()));
    if (varSysIt != coordSys.end()) {
        if((*varSysIt)->isSimpleSpatialGridded()) {

            TTag = boost::shared_ptr<MetGmTimeTag>(new MetGmTimeTag);

            CoordinateSystem::ConstAxisPtr tAxis = (*varSysIt)->getTimeAxis();

            assert(tAxis->getAxisType() == CoordinateAxis::Time);

            boost::shared_ptr<Data> data = pCdmReader->getData(tAxis->getName());

            TTag->nT_ = data->size();

            const CDMAttribute& tUnitAttribute(cdmRef.getAttribute("time", std::string("units")));
            const std::string t_unit = tUnitAttribute.getStringValue();
            const TimeUnit kilde_tu(t_unit);

            const boost::shared_array<double> tArray = data->asConstDouble();

            for(size_t index = 0; index < data->size(); ++index) {
                time_t t = kilde_tu.unitTime2epochSeconds(tArray[index]);
                TTag->timePoints_.push_back(t);
            }

            if(TTag->timePoints_.size() <= 1)
                throw CDMException("time axis has one point can't determine dt needed for MetGm");

            if(TTag->hasNegativeTimePoints())
                throw CDMException("negative values on the time axis not supported");

            if(TTag->hasNonEquidistantTimePoints())
                throw CDMException("time points at time axis are not equidistant [use extractor to split file on boundaries]");


            TTag->dT_ = TTag->timePoints_.at(1) - TTag->timePoints_.at(0);

            TTag->extractStartDateTime();

            TTag->extractAnalysisDateTime(pCdmReader);
        }
    } else {
    }

    return TTag;
}

}

