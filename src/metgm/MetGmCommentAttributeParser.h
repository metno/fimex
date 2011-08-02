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

#ifndef METGM_COMMENTATTRIBUTEPARSER_HPP
#define METGM_COMMENTATTRIBUTEPARSER_HPP

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMAttribute.h"

// libxml2
#include <libxml/tree.h>
#include <libxml/xpath.h>

// boost
//
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    #define FREE_TEXT "metgm_free_text"
    #define VERSION   "metgm_version"
    #define ANALYSIS_DATE_TIME "metgm_analysis_date_time"
    #define START_DATE_TIME "metgm_start_date_time"
    #define DATA_TYPE "metgm_data_type"
    #define MODEL_TYPE "metgm_model_type"
    #define PRODUCTION_NATION "metgm_production_nation"

    class MetGmCommentAttributeParser {
    public:
        static boost::shared_ptr<MetGmCommentAttributeParser> createMetGmCommentAttributeParser(const boost::shared_ptr<CDMReader>& pCdmReader);

        std::string freeText()         {return freeText_; }
        std::string version()          {return version_; }
        std::string dataType()         {return dataType_; }
        std::string modelType()        {return modelType_; }
        std::string productNation()    {return productNation_; }
        std::string analysisDateTime() {return analysisDateTime_; }
        std::string startDateTime()    {return startDateTime_; }

    private:
        MetGmCommentAttributeParser() { }

        std::string freeText_;
        std::string version_;
        std::string dataType_;
        std::string modelType_;
        std::string productNation_;
        std::string analysisDateTime_;
        std::string startDateTime_;
    };
}

#endif
