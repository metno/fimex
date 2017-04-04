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
#include "MetGmCommentAttributeParser.h"

// fimex
//
#include "fimex/CDM.h"
#include "fimex/XMLDoc.h"
#include "fimex/Logger.h"

// libxml2
//
#include <libxml/tree.h>
#include <libxml/xpath.h>


// standard
//
#include <iostream>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.MetGmCDMWriter.MetGmCommentAttribute");

boost::shared_ptr<MetGmCommentAttributeParser> MetGmCommentAttributeParser::createMetGmCommentAttributeParser(const boost::shared_ptr<CDMReader>& pCdmReader) {
    boost::shared_ptr<MetGmCommentAttributeParser> parser =
            boost::shared_ptr<MetGmCommentAttributeParser>(new MetGmCommentAttributeParser);

    CDMAttribute metgmMetaData; // encoded within comment
    if(pCdmReader->getCDM().getAttribute(pCdmReader->getCDM().globalAttributeNS(), "comment", metgmMetaData)) {
        try {
            std::string commentStr = metgmMetaData.getStringValue();
            if (! (commentStr.size() > 0 && commentStr.substr(0,1) == "<")) throw CDMException("comment not xml");
            boost::shared_ptr<XMLDoc> doc = XMLDoc::fromString(metgmMetaData.getStringValue());

            if(doc.get() != 0) {
                XPathObjPtr xpathObj = doc->getXPathObject("/meta_data/attribute");
                xmlNodeSetPtr nodes = xpathObj->nodesetval;
                size_t size = (nodes) ? nodes->nodeNr : 0;
                for (size_t i = 0; i < size; ++i) {
                    xmlNodePtr node = nodes->nodeTab[i];
                    std::string attributeName = getXmlProp(node, "name");
                    if(attributeName == std::string(FREE_TEXT)) {
                        parser->freeText_ = getXmlProp(node, "value");
                    } else if(attributeName == std::string(VERSION)) {
                        parser->version_ = getXmlProp(node, "value");
                    } else if(attributeName == std::string(DATA_TYPE)) {
                        parser->dataType_ = getXmlProp(node, "value");
                    }  else if(attributeName == std::string(MODEL_TYPE)) {
                        parser->modelType_ = getXmlProp(node, "value");
                    } else if(attributeName == std::string(PRODUCTION_NATION)) {
                        parser->productNation_ = getXmlProp(node, "value");
                    } else if(attributeName == std::string(ANALYSIS_DATE_TIME)) {
                        parser->analysisDateTime_ = getXmlProp(node, "value");
                    } else if(attributeName == std::string(START_DATE_TIME)) {
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

}
