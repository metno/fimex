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

#ifndef METGM_GROUP1PTR_H
#define METGM_GROUP1PTR_H

#include "fimex/CDMReaderDecl.h"
#include "fimex/TimeUtils.h"

#include <memory>
#include <string>

    namespace MetNoFimex {

        class CDMReader;
        class CDMVariable;
        class MetGmTimeTag;
        class MetGmCommentAttributeParser;
        class MetGmFileHandlePtr;
        class MetGmHandlePtr;
        class MetGmVersion;

        class MetGmGroup1Ptr {
        public:
            static std::shared_ptr<MetGmGroup1Ptr> createMetGmGroup1PtrForReading(const std::shared_ptr<MetGmHandlePtr> pMgmHandle);
            static std::shared_ptr<MetGmGroup1Ptr> createMetGmGroup1PtrForWriting(const CDMReader_p pCdmReader);

            std::string dataTypeAsString();
            unsigned int dataType()     { return dataType_; }
            std::string freeText()      { return freeText_; }
            std::string modelType()     { return modelType_; }
            std::string productNation() { return productNation_; }
            time_t      startTime()     { return start_t; }
            time_t      analysisTime()  { return analysis_t; }
            time_point startTimeAsTimePoint() { return make_time_from_timet(start_t); }
            time_point analysisTimeAsTimePoint() { return make_time_from_timet(analysis_t); }
            std::string startTimeAsIsoString() { return make_time_string(startTimeAsTimePoint()); }
            std::string analysisTimeAsIsoString() { return make_time_string(analysisTimeAsTimePoint()); }
            std::string startTimeAsIsoExtendedString() { return make_time_string_extended(startTimeAsTimePoint()); }
            std::string analysisTimeAsIsoExtendedString() { return make_time_string_extended(analysisTimeAsTimePoint()); }
            const std::shared_ptr<MetGmHandlePtr>& mgmHandle() { return this->pHandle_; }

        private:

            MetGmGroup1Ptr() { }
            MetGmGroup1Ptr(const std::shared_ptr<MetGmHandlePtr> pMgmHandle)
                : pHandle_(pMgmHandle)
            {
            }

            std::shared_ptr<MetGmCommentAttributeParser> parser_;
            std::shared_ptr<MetGmTimeTag> tTag_;
            const std::shared_ptr<MetGmHandlePtr> pHandle_;

            std::string  freeText_;
            std::string  modelType_;
            std::string  productNation_;
            unsigned int dataType_;
            time_t       analysis_t;
            time_t       start_t;
        };

    }

    #endif // METGM_GROUP1PTR_H
