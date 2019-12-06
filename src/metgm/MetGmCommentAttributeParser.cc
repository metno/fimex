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

// internals
//
#include "MetGmCommentAttributeParser.h"

// fimex
//
#include "fimex/CDM.h"
#include "fimex/CDMAttribute.h"
#include "fimex/CDMException.h"
#include "fimex/CDMReader.h"
#include "fimex/Logger.h"
#include "fimex/XMLDoc.h"

// libxml2
//
#include <libxml/tree.h>
#include <libxml/xpath.h>

namespace MetNoFimex {

static Logger_p logger = getLogger("fimex.MetGmCDMWriter.MetGmCommentAttribute");

const std::string FREE_TEXT = "metgm_free_text";
const std::string VERSION = "metgm_version";
const std::string ANALYSIS_DATE_TIME = "metgm_analysis_date_time";
const std::string START_DATE_TIME = "metgm_start_date_time";
const std::string DATA_TYPE = "metgm_data_type";
const std::string MODEL_TYPE = "metgm_model_type";
const std::string PRODUCTION_NATION = "metgm_production_nation";

std::shared_ptr<MetGmCommentAttributeParser> MetGmCommentAttributeParser::createMetGmCommentAttributeParser(const CDMReader_p& pCdmReader)
{
    std::shared_ptr<MetGmCommentAttributeParser> parser = std::shared_ptr<MetGmCommentAttributeParser>(new MetGmCommentAttributeParser);

    CDMAttribute metgmMetaData; // encoded within comment
    if(pCdmReader->getCDM().getAttribute(pCdmReader->getCDM().globalAttributeNS(), "comment", metgmMetaData)) {
        try {
            std::string commentStr = metgmMetaData.getStringValue();
            if (! (commentStr.size() > 0 && commentStr.substr(0,1) == "<")) throw CDMException("comment not xml");
            XMLDoc_p doc = XMLDoc::fromString(metgmMetaData.getStringValue());

            if(doc.get() != 0) {
                xmlXPathObject_p xpathObj = doc->getXPathObject("/meta_data/attribute");
                xmlNodeSetPtr nodes = xpathObj->nodesetval;
                size_t size = (nodes) ? nodes->nodeNr : 0;
                for (size_t i = 0; i < size; ++i) {
                    xmlNodePtr node = nodes->nodeTab[i];
                    std::string attributeName = getXmlProp(node, "name");
                    if (attributeName == FREE_TEXT) {
                        parser->freeText_ = getXmlProp(node, "value");
                    } else if (attributeName == VERSION) {
                        parser->version_ = getXmlProp(node, "value");
                    } else if (attributeName == DATA_TYPE) {
                        parser->dataType_ = getXmlProp(node, "value");
                    } else if (attributeName == MODEL_TYPE) {
                        parser->modelType_ = getXmlProp(node, "value");
                    } else if (attributeName == PRODUCTION_NATION) {
                        parser->productNation_ = getXmlProp(node, "value");
                    } else if (attributeName == ANALYSIS_DATE_TIME) {
                        parser->analysisDateTime_ = getXmlProp(node, "value");
                    } else if (attributeName == START_DATE_TIME) {
                        parser->startDateTime_ = getXmlProp(node, "value");
                    }
                }
            }
        } catch (CDMException& exception) {
            // ignore
            LOG4FIMEX(logger, Logger::DEBUG, "failed parsing global attribute 'comment' as xml: '" << metgmMetaData.getStringValue() << "'");
        }
    }

    return parser;
}

} // namespace MetNoFimex
