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

// boost
//
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    class CDMReader;
    class CDMVariable;
    class MetGmTimeTag;
    class MetGmCommentAttributeParser;

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

}

#endif // METGM_GROUP1PTR_H
