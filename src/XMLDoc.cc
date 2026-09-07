/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/XMLDoc.h"

#include "fimex/CDMException.h"
#include "fimex/ChunkReaderXmlInputCtx.h"

#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

namespace MetNoFimex {

namespace {
std::string make_string(xmlChar* xc)
{
    std::string retVal;
    if (xc) {
        retVal = std::string(reinterpret_cast<char*>(xc));
        xmlFree(xc);
    }
    return retVal;
}

int doXmlInitParser()
{
    xmlInitParser();
    LIBXML_TEST_VERSION
    return 1;
}

void init()
{
    static int done = doXmlInitParser();
    (void)done;
}

xmlDoc* xmlDocFromFile(const std::string& filename)
{
    init();
    return xmlReadFile(filename.c_str(), NULL, XML_PARSE_HUGE);
}

xmlDoc* xmlDocFromChunkReader(ChunkReader_p cr, const std::string& url)
{
    init();
    ChunkReaderXmlInputCtx crctx(cr);
    return xmlReadIO(readChunkReaderXmlInputCtx, closeChunkReaderXmlInputCtx, &crctx, url.c_str(), NULL, XML_PARSE_HUGE);
}

xmlDoc* checkXmlDoc(xmlDoc* doc, const std::string& url)
{
    if (!doc) {
        throw CDMException("cannot read xml from '" + url + "'");
    }
    return doc;
}

} // namespace

XMLDoc::XMLDoc(const std::string& filename)
    : XMLDoc(xmlDocFromFile(filename), filename)
{
}

XMLDoc::XMLDoc(ChunkReader_p cr, const std::string& url)
    : XMLDoc(xmlDocFromChunkReader(cr, url), url)
{
}

XMLDoc::XMLDoc(xmlDoc* pdoc, const std::string& url)
    : doc(checkXmlDoc(pdoc, url))
    , xpathCtx(0)
{
    // apply the XInclude process
    if (xmlXIncludeProcess(doc) < 0) {
        throw CDMException("XInclude processing failed for '" + url + "'");
    }

    xpathCtx = xmlXPathNewContext(pdoc);
    if (!xpathCtx) {
        throw CDMException("unable to generate xpath context for '" + url + "'");
    }
}

std::string XMLDoc::toString(const xmlNodePtr node)
{
    std::unique_ptr<xmlDoc, decltype(&xmlFreeDoc)> ndoc(xmlNewDoc((const xmlChar*)"1.0"), &xmlFreeDoc);
    xmlNodePtr retNode = xmlDocCopyNode(node, ndoc.get(), 1); // points to new memory
    xmlDocSetRootElement(ndoc.get(), retNode);  // transfer ownership
    xmlChar* str;
    int size;
    xmlDocDumpMemory(ndoc.get(), &str, &size);
    return make_string(str);
}

XMLDoc_p XMLDoc::fromFile(const std::string& filename)
{
    return std::make_shared<XMLDoc>(filename);
}

XMLDoc_p XMLDoc::fromString(const std::string& buffer, const std::string& url)
{
    // cannot use make_shared here as XMLDoc ctor is private
    return XMLDoc_p(new XMLDoc(xmlReadMemory(buffer.c_str(), buffer.length(), url.c_str(), NULL, 0), url));
}

void XMLDoc::registerNamespace(const std::string& prefix, const std::string& href)
{
    /* do register namespace */
    if(xmlXPathRegisterNs(xpathCtx, reinterpret_cast<const xmlChar *>(prefix.c_str()), reinterpret_cast<const xmlChar *>(href.c_str())) != 0) {
        throw CDMException("unable to register NS with prefix "+prefix + " and href " + href);
    }
}

XMLDoc::~XMLDoc()
{
    if (doc) {
        xmlFreeDoc(doc);
    }
    if (xpathCtx) {
        xmlXPathFreeContext(xpathCtx);
    }
}

xmlXPathObject_p XMLDoc::getXPathObject(const std::string& xpath, xmlNodePtr node) const
{
    if (node == 0) {
        xpathCtx->node = xmlDocGetRootElement(doc);
    } else {
        xpathCtx->node = node;
    }
    xmlXPathObject_p xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpath.c_str()), xpathCtx), xmlXPathFreeObject);
    if (xpathObj.get() == 0) {
        throw CDMException("unable to parse xpath: " + xpath);
    }
    return xpathObj;
}

/**
 * a memory-safe form of xmlGetProp
 *
 * @return a string of the attribute, "" if attribute doesn't exist
 */
std::string getXmlProp(const xmlNodePtr node, const char* attrName)
{
    return make_string(xmlGetProp(node, reinterpret_cast<const xmlChar*>(attrName)));
}

std::string getXmlName(const xmlNodePtr node)
{
    return std::string(reinterpret_cast<const char*>(node->name));
}

std::string getXmlContent(const xmlNodePtr node)
{
    std::string retVal;
    if (node)
        retVal = make_string(xmlNodeGetContent(node));
    return retVal;
}

} // namespace MetNoFimex
