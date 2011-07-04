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

#ifndef METGM_GROUP1PTR_H
#define METGM_GROUP1PTR_H

// internals
//
#include "MetGmTimeTag.h"
#include "MetGmCommentAttributeParser.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"

// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

namespace MetNoFimex {

    class MetGmGroup1Ptr {
    public:
        static boost::shared_ptr<MetGmGroup1Ptr> createMetGmGroup1Ptr(boost::shared_ptr<CDMReader>& pCdmReader);

        std::string freeText()      { return freeText_; }
        time_t      startTime()     { return tTag_->startTime(); }
        time_t      analysisTime()  { return analysis_t; }
        std::string modelType()     { return modelType_; }
        std::string productNation() { return productNation_; }
        unsigned int dataType()     { return dataType_; }
    private:

        MetGmGroup1Ptr() { }

        boost::shared_ptr<MetGmCommentAttributeParser> parser_;
        boost::shared_ptr<MetGmTimeTag>                tTag_;

        std::string  freeText_;
        std::string  modelType_;
        std::string  productNation_;
        unsigned int dataType_;
        time_t       analysis_t;
    };

    boost::shared_ptr<MetGmGroup1Ptr> MetGmGroup1Ptr::createMetGmGroup1Ptr(boost::shared_ptr<CDMReader>& pCdmReader)
    {
        boost::shared_ptr<MetGmGroup1Ptr> gp1 =
                boost::shared_ptr<MetGmGroup1Ptr>(new MetGmGroup1Ptr);

        const CDM& cdmRef = pCdmReader->getCDM();

        gp1->tTag_ = MetGmTimeTag::createMetGmTimeTag(pCdmReader);

        gp1->parser_ = MetGmCommentAttributeParser::createMetGmCommentAttributeParser(pCdmReader);

        // set analysis date time
        if(!gp1->parser_->analysisDateTime().empty()) {
            TimeUnit tu("seconds since 1970-01-01 00:00:00");
            boost::posix_time::ptime analysis_posix = boost::posix_time::from_iso_string(gp1->parser_->analysisDateTime());
            double unit_time = tu.posixTime2unitTime(analysis_posix);
            gp1->analysis_t = tu.unitTime2epochSeconds(unit_time);
        } else {
            gp1->analysis_t = gp1->tTag_->analysisTime();
        }

        CDMAttribute metgmFreeTextAttribute;
        gp1->freeText_= std::string("comment---------------------------------");
        if(!gp1->parser_->freeText().empty()) {
            /* value from xml */
            gp1->freeText_ = gp1->parser_->freeText();
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), FREE_TEXT, metgmFreeTextAttribute)) {
            /* value from cdm model */
            gp1->freeText_ = metgmFreeTextAttribute.getStringValue();
        }

        CDMAttribute metgmDataTypeAttribute;
        gp1->dataType_ = 4;
        if(!gp1->parser_->dataType().empty()) {
            /* value from xml */
            gp1->dataType_ = boost::lexical_cast<unsigned int>(gp1->parser_->dataType().c_str());
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), DATA_TYPE, metgmDataTypeAttribute)) {
            /* value from cdm model */
            gp1->dataType_ = boost::lexical_cast<unsigned int>(metgmDataTypeAttribute.getStringValue().c_str());
        }

        CDMAttribute metgmModelTypeAttribute;
        gp1->modelType_= std::string("----------------");
        if(!gp1->parser_->modelType().empty()) {
            /* value from xml */
            gp1->modelType_ = gp1->parser_->modelType();
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), MODEL_TYPE, metgmModelTypeAttribute)) {
            /* value from cdm model */
            gp1->modelType_ = metgmModelTypeAttribute.getStringValue();
        }

        CDMAttribute metgmProductNationAttribute;
        gp1->productNation_ = "NOR";
        if(!gp1->parser_->productNation().empty()) {
            /* value from xml */
            gp1->productNation_ = gp1->parser_->productNation();
        } else if(cdmRef.getAttribute(cdmRef.globalAttributeNS(), PRODUCTION_NATION, metgmProductNationAttribute)) {
            /* value from cdm model */
            gp1->productNation_ = metgmProductNationAttribute.getStringValue();
        }

        return gp1;
    }
}

#endif // METGM_GROUP1PTR_H
