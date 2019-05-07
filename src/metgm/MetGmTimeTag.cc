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
#include "MetGmTimeTag.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup3Ptr.h"

// fimex
//
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/coordSys/CoordinateSystem.h"

// boost
//
#include <boost/shared_array.hpp>

// standard
//
#include <memory>
#include <numeric>

namespace MetNoFimex {

std::shared_ptr<MetGmTimeTag> MetGmTimeTag::createMetGmTimeTagGlobal(const CDMReader_p pCdmReader)
{
    if(!pCdmReader.get())
        throw CDMException("createMetGmTimeTag: pCdmReader is null");

    const CDM& cdmRef = pCdmReader->getCDM();

    if( !(cdmRef.hasDimension("time") && cdmRef.hasVariable("time")) )
        throw CDMException("createMetGmTimeTag: cdm model doesn't have the 'time'");

    std::shared_ptr<MetGmTimeTag> TTag = std::shared_ptr<MetGmTimeTag>(new MetGmTimeTag());

    TTag->init(pCdmReader);

    return TTag;
}

std::shared_ptr<MetGmTimeTag> MetGmTimeTag::createMetGmTimeTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable)
{
    std::shared_ptr<MetGmTimeTag> TTag;

    const CoordinateSystem_cp_v coordSys = listCoordinateSystems(pCdmReader);
    const CDM& cdmRef = pCdmReader->getCDM();

    CoordinateSystem_cp cs = findCompleteCoordinateSystemFor(coordSys, pVariable->getName());
    if (cs.get()) {
        if(cs->isSimpleSpatialGridded()) {

            CoordinateAxis_cp tAxis = cs->getTimeAxis();

            if(!tAxis.get()) {
                return std::shared_ptr<MetGmTimeTag>();
            }

            TTag = std::shared_ptr<MetGmTimeTag>(new MetGmTimeTag);

            if(tAxis->getAxisType() != CoordinateAxis::Time)
                throw CDMException("time axis not found");

            DataPtr data = pCdmReader->getData(tAxis->getName());

            TTag->nT_ = data->size();

            const CDMAttribute& tUnitAttribute(cdmRef.getAttribute("time", std::string("units")));
            const std::string t_unit = tUnitAttribute.getStringValue();
            const TimeUnit kilde_tu(t_unit);

            const boost::shared_array<double> tArray = data->asDouble();

            for(size_t index = 0; index < data->size(); ++index) {
                time_t t = kilde_tu.unitTime2epochSeconds(tArray[index]);
                TTag->points_.push_back(t);
            }

            TTag->extractStartDateTime();

            TTag->extractAnalysisDateTime(pCdmReader);

            if(TTag->hasNegativeTimePoints())
                throw CDMException("negative values on the time axis not supported");

            if(TTag->hasNonEquidistantTimePoints())
                throw CDMException("time points at time axis are not equidistant [use extractor to split file on boundaries]");

            if(TTag->points_.size() <= 1) {
                TTag->dT_ = 3600;
            } else {
                TTag->dT_ = TTag->points_.at(1) - TTag->points_.at(0);
            }
        }
    } else {
    }

    return TTag;
}

std::shared_ptr<MetGmTimeTag> MetGmTimeTag::createMetGmTimeTagForReading(const std::shared_ptr<MetGmGroup1Ptr> pGroup1,
                                                                         const std::shared_ptr<MetGmGroup3Ptr> pGroup3)
{
    std::shared_ptr<MetGmTimeTag> TTag = std::shared_ptr<MetGmTimeTag>(new MetGmTimeTag);
    if(pGroup3->p_id() == 0) {
        TTag->dT_ = pGroup3->nt() * pGroup3->dt(); // valid for total duration
        TTag->nT_ = 1;
        TTag->analysis_t = pGroup1->analysisTime();
        TTag->start_t = pGroup1->startTime();
        TTag->points_.push_back(TTag->start_t + TTag->nT_ * TTag->dT_);
        TTag->pointsAsTimePoints_.push_back(make_time_from_timet(TTag->points_.at(0)));
        TTag->pointsAsDouble_.push_back(TTag->points_.at(0));
    } else {
        TTag->dT_ = pGroup3->dt();
        TTag->nT_ = pGroup3->nt();
        TTag->analysis_t = pGroup1->analysisTime();
        TTag->start_t = pGroup1->startTime();
        for(size_t step = 0; step < TTag->nT_; ++step) {
            TTag->points_.push_back(TTag->start_t + step * TTag->dT_);
            TTag->pointsAsTimePoints_.push_back(make_time_from_timet(TTag->points_.at(step)));
            TTag->pointsAsDouble_.push_back(TTag->points_.at(step));
        }
    }
    return TTag;
}

    void MetGmTimeTag::extractAnalysisDateTime(const CDMReader_p pCdmReader)
    {
        const CDM& cdmRef = pCdmReader->getCDM();
        CDMAttribute metgmAnalysisDateTimeAttribute;
        FimexTime analysisTime;
        std::vector<std::string> refVarNames = cdmRef.findVariables("standard_name", "forecast_reference_time");
        if(!refVarNames.empty()) { // we have to honour if there is forecast time in CDM model we get
            analysisTime = getUniqueForecastReferenceTimeFT(pCdmReader);
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), ANALYSIS_DATE_TIME, metgmAnalysisDateTimeAttribute)) {
            analysisTime = string2FimexTime(metgmAnalysisDateTimeAttribute.getStringValue());
        } else {
            analysisTime = make_fimextime_utc_now();
        }

        const TimeUnit tu("seconds since 1970-01-01 00:00:00");
        double unit_time = tu.fimexTime2unitTime(analysisTime);
        analysis_t = tu.unitTime2epochSeconds(unit_time);
    }

    bool MetGmTimeTag::hasNegativeTimePoints()
    {
        return std::find_if(points_.begin(), points_.end(), [](float f) { return f < 0; }) != points_.end();
    }

    bool MetGmTimeTag::hasNonEquidistantTimePoints()
    {
        if(nT() <= 1)
            return false;

        std::deque<double> adjDiff(points_.size());
        std::adjacent_difference(points_.begin(), points_.end(), adjDiff.begin());
        adjDiff.pop_front(); // remove first element

        std::deque<double>::iterator it = std::unique_copy(adjDiff.begin(), adjDiff.end(), adjDiff.begin());
        std::sort(adjDiff.begin(), it);

        adjDiff.resize(it - adjDiff.begin());

        return adjDiff.size() != 1;
    }

    void MetGmTimeTag::extractTimePoints(const CDMReader_p pCdmReader)
    {
        const CDM& cdmRef = pCdmReader->getCDM();
        const CDMVariable& tVar = cdmRef.getVariable("time");

        DataPtr tData = pCdmReader->getData(tVar.getName());

        const CDMAttribute& tUnitAttribute(cdmRef.getAttribute("time", std::string("units")));
        const std::string t_unit = tUnitAttribute.getStringValue();
        const TimeUnit kilde_tu(t_unit);

        const boost::shared_array<double> tArray = tData->asDouble();

        for(size_t index = 0; index < tData->size(); ++index) {
            time_t t = kilde_tu.unitTime2epochSeconds(tArray[index]);
            points_.push_back(t);
        }

        if(hasNegativeTimePoints())
            throw CDMException("negative values on the time axis not supported");

        if(hasNonEquidistantTimePoints())
            throw CDMException("time points at time axis are not equidistant [use extractor to split file on boundaries]");
    }

    void MetGmTimeTag::extractStartDateTime()
    {
        start_t = points_.at(0);
    }

    void MetGmTimeTag::init(const CDMReader_p pCdmReader)
    {
        const CDM& cdmRef = pCdmReader->getCDM();
        const CDMDimension& tDim = cdmRef.getDimension("time");

        if(!tDim.isUnlimited())
            throw CDMException("in given cdm model 'time'' is not unlimited dimension");

        nT_ = tDim.getLength();

        extractTimePoints(pCdmReader);

        if(points_.size() <= 1)
            dT_ = 3600;
        else
            dT_ = points_.at(1) - points_.at(0);

        extractStartDateTime();

        extractAnalysisDateTime(pCdmReader);
    }

}

