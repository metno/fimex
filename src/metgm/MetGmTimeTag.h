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

#include "fimex/CDMReaderDecl.h"

// boost
#include <boost/date_time/posix_time/posix_time.hpp>
#include <memory>

// standard
#include <vector>

namespace MetNoFimex {

#define ANALYSIS_DATE_TIME "metgm_analysis_date_time"

    class CDMVariable;
    class MetGmGroup1Ptr;
    class MetGmGroup3Ptr;

    class MetGmTimeTag {
    public:
        static std::shared_ptr<MetGmTimeTag> createMetGmTimeTagGlobal(const CDMReader_p pCdmReader);

        static std::shared_ptr<MetGmTimeTag> createMetGmTimeTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVariable);

        static std::shared_ptr<MetGmTimeTag> createMetGmTimeTagForReading(const std::shared_ptr<MetGmGroup1Ptr> pGroup1,
                                                                          const std::shared_ptr<MetGmGroup3Ptr> pGroup3);

        inline unsigned int        nT()           { return nT_; }
        inline time_t              dT()           { return dT_; }
        inline time_t              analysisTime() { return analysis_t; }
        inline time_t              startTime()    { return start_t; }
        inline std::vector<time_t>&                   pointsAsSystemTime() { return points_; }
        inline std::vector<double>&                   pointsAsDouble()     { return pointsAsDouble_; }
        inline std::vector<boost::posix_time::ptime>& pointsAsBoostPosix() { return pointsAsBoostPosix_; }

    private:

        inline MetGmTimeTag() : analysis_t(0), start_t(0), dT_(0), nT_(0) { }

        void extractAnalysisDateTime(const CDMReader_p pCdmReader);

        bool hasNegativeTimePoints();

        bool hasNonEquidistantTimePoints();

        void extractTimePoints(const CDMReader_p pCdmReader);

        void extractStartDateTime();

        void init(const CDMReader_p pCdmReader);

        time_t              analysis_t;
        time_t              start_t;
        time_t              dT_;
        unsigned int        nT_;
        std::vector<time_t> points_;
        std::vector<double> pointsAsDouble_;
        std::vector<boost::posix_time::ptime> pointsAsBoostPosix_;
    };
}

#endif // METGM_TIMETAG_H
