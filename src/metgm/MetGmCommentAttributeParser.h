/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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
#include "fimex/CDMReaderDecl.h"

#include <memory>

namespace MetNoFimex {

extern const std::string FREE_TEXT;
extern const std::string VERSION;
extern const std::string ANALYSIS_DATE_TIME;
extern const std::string START_DATE_TIME;
extern const std::string DATA_TYPE;
extern const std::string MODEL_TYPE;
extern const std::string PRODUCTION_NATION;

class MetGmCommentAttributeParser
{
public:
    static std::shared_ptr<MetGmCommentAttributeParser> createMetGmCommentAttributeParser(const CDMReader_p& pCdmReader);

    const std::string& freeText() const { return freeText_; }
    const std::string& version() const { return version_; }
    const std::string& dataType() const { return dataType_; }
    const std::string& modelType() const { return modelType_; }
    const std::string& productNation() const { return productNation_; }
    const std::string& analysisDateTime() const { return analysisDateTime_; }
    const std::string& startDateTime() const { return startDateTime_; }

private:
    MetGmCommentAttributeParser() {}

    std::string freeText_;
    std::string version_;
    std::string dataType_;
    std::string modelType_;
    std::string productNation_;
    std::string analysisDateTime_;
    std::string startDateTime_;
};

} // namespace MetNoFimex

#endif
