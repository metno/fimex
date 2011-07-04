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

#ifndef METGM_TIMETAG_H
#define METGM_TIMETAG_H

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

#define ANALYSIS_DATE_TIME "metgm_analysis_date_time"

    class MetGmTimeTag {
    public:

        static boost::shared_ptr<MetGmTimeTag> createMetGmTimeTag(boost::shared_ptr<CDMReader>& pCdmReader);
        static boost::shared_ptr<MetGmTimeTag> createMetGmTimeTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVariable);

        inline unsigned int        nT()           { return nT_; }
        inline time_t              dT()           { return dT_; }
        inline time_t              analysisTime() { return analysis_t; }
        inline time_t              startTime()    { return start_t; }
        inline std::vector<time_t> timePoints()   { return timePoints_; }

    private:

        inline MetGmTimeTag() : analysis_t(0), start_t(0), dT_(0), nT_(0) { }

        inline void extractAnalysisDateTime(boost::shared_ptr<CDMReader>& pCdmReader)
        {
            const CDM cdmRef = pCdmReader->getCDM();
            CDMAttribute metgmAnalysisDateTimeAttribute;
            boost::posix_time::ptime analysisTime;
            std::vector<std::string> refVarNames = cdmRef.findVariables("standard_name", "forecast_reference_time");
            if(!refVarNames.empty()) { // we have to honour if there is forecast time in CDM model we get
                analysisTime = getUniqueForecastReferenceTime(pCdmReader);
            } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), ANALYSIS_DATE_TIME, metgmAnalysisDateTimeAttribute)) {
                analysisTime = boost::posix_time::from_iso_string(metgmAnalysisDateTimeAttribute.getStringValue());
            } else {
                analysisTime = boost::posix_time::second_clock::universal_time();
            }

            const TimeUnit tu("seconds since 1970-01-01 00:00:00");
            double unit_time = tu.posixTime2unitTime(analysisTime);
            analysis_t = tu.unitTime2epochSeconds(unit_time);
        }

        inline bool hasNegativeTimePoints() {
            std::less_equal<float> leq;
            return std::find_if(timePoints_.begin(), timePoints_.end(), boost::bind( leq, _1, 0 ) ) != timePoints_.end();
        }

        inline bool hasNonEquidistantTimePoints() {
            std::deque<double> adjDiff(timePoints_.size());
            std::adjacent_difference(timePoints_.begin(), timePoints_.end(), adjDiff.begin());
            adjDiff.pop_front(); // remove first element

            std::deque<double>::iterator it = std::unique_copy(adjDiff.begin(), adjDiff.end(), adjDiff.begin());
            std::sort(adjDiff.begin(), it);

            adjDiff.resize(it - adjDiff.begin());

            return adjDiff.size() != 1;
        }

        inline void extractTimePoints(boost::shared_ptr<CDMReader>& pCdmReader)
        {
            const CDM& cdmRef = pCdmReader->getCDM();
            const CDMVariable& tVar = cdmRef.getVariable("time");

            boost::shared_ptr<Data> tData = pCdmReader->getData(tVar.getName());

            const CDMAttribute& tUnitAttribute(cdmRef.getAttribute("time", std::string("units")));
            const std::string t_unit = tUnitAttribute.getStringValue();
            const TimeUnit kilde_tu(t_unit);

            const boost::shared_array<double> tArray = tData->asConstDouble();

            for(size_t index = 0; index < tData->size(); ++index) {
                time_t t = kilde_tu.unitTime2epochSeconds(tArray[index]);
                timePoints_.push_back(t);
            }

            if(timePoints_.size() <= 1)
                throw CDMException("time axis has one point can't determine dt needed for MetGm");

            if(hasNegativeTimePoints())
                throw CDMException("negative values on the time axis not supported");

            if(hasNonEquidistantTimePoints())
                throw CDMException("time points at time axis are not equidistant [use extractor to split file on boundaries]");
        }

        inline void extractStartDateTime()
        {
            start_t = timePoints_.at(0);
        }

        inline void init(boost::shared_ptr<CDMReader>& pCdmReader)
        {
            const CDM& cdmRef = pCdmReader->getCDM();
            const CDMDimension& tDim = cdmRef.getDimension("time");

            if(!tDim.isUnlimited())
                throw CDMException("in given cdm model 'time'' is not unlimited dimension");

            nT_ = tDim.getLength();

            extractTimePoints(pCdmReader);

            dT_ = timePoints_.at(1) - timePoints_.at(0);

            extractStartDateTime();

            extractAnalysisDateTime(pCdmReader);
        }

        time_t              analysis_t;
        time_t              start_t;
        time_t              dT_;
        unsigned int        nT_;
        std::vector<time_t> timePoints_;
    };
}

#endif // METGM_TIMETAG_H
