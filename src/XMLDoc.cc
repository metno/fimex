/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "fimex/XMLDoc.h"

#include "fimex/CDMException.h"

#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

namespace MetNoFimex
{

XMLDoc::XMLDoc(const std::string& filename)
    :doc(0), xpathCtx(0)
{
    init();
    xmlDoc* pdoc = xmlReadFile(filename.c_str(), NULL, 0);
    if (pdoc == 0) throw CDMException("cannot read xml-file: "+filename);
    setDoc(pdoc);
    setXPathCtx(pdoc);
}

XMLDoc::XMLDoc()
    : doc(0), xpathCtx(0)
{
    init();
}

void XMLDoc::init()
{
    xmlInitParser();
    LIBXML_TEST_VERSION
}

void XMLDoc::setDoc(xmlDoc* pdoc)
{
    doc = pdoc;
    if (doc == 0) {
        cleanup();
        throw CDMException("unable to parse content");
    }
    // apply the XInclude process
    if (xmlXIncludeProcess(doc) < 0) {
        cleanup();
        throw CDMException("XInclude processing failed");
    }
}

void XMLDoc::setXPathCtx(xmlDoc* pdoc)
{
    xpathCtx = xmlXPathNewContext(pdoc);
    if (xpathCtx == 0) {
        cleanup();
        throw CDMException("unable to generate xpath context");
    }
}

std::string XMLDoc::toString(const xmlNodePtr node)
{
    xmlDocPtr ndoc = xmlNewDoc((const xmlChar*)"1.0");
    xmlNodePtr retNode = xmlCopyNodeList(node);
    xmlDocSetRootElement(ndoc, retNode);
    xmlSetTreeDoc(retNode, doc);
    xmlChar* str;
    int size;
    xmlDocDumpMemory(ndoc, &str, &size);
    std::string retVal(reinterpret_cast<const char *>(str));
    xmlFree(str);
    xmlFreeDoc(ndoc);
    return retVal;
}

XMLDoc_p XMLDoc::fromFile(const std::string& filename)
{
    return XMLDoc_p(new XMLDoc(filename));
}

XMLDoc_p XMLDoc::fromString(const std::string& buffer, const std::string& url)
{
    XMLDoc_p pdoc(new XMLDoc());
    xmlDoc* pxmldoc = xmlReadMemory(buffer.c_str(), buffer.length(), url.c_str(), NULL, 0);
    pdoc->setDoc(pxmldoc);
    pdoc->setXPathCtx(pxmldoc);
    return pdoc;
}

void XMLDoc::registerNamespace(const std::string& prefix, const std::string& href)
{
    /* do register namespace */
    if(xmlXPathRegisterNs(xpathCtx, reinterpret_cast<const xmlChar *>(prefix.c_str()), reinterpret_cast<const xmlChar *>(href.c_str())) != 0) {
        throw CDMException("unable to register NS with prefix "+prefix + " and href " + href);
    }
}

void XMLDoc::cleanup()
{
    if (doc != 0) {
        xmlFreeDoc(doc);
    }
    if (xpathCtx != 0) {
        xmlXPathFreeContext(xpathCtx);
    }
    xmlCleanupParser();

}

XMLDoc::~XMLDoc()
{
    cleanup();
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
 * a memory-save form of xmlGetProp
 *
 * @return a string of the attribute, "" if attribute doesn't exist
 */
std::string getXmlProp(const xmlNodePtr node, const std::string& attrName) {
    boost::shared_ptr<xmlChar> xChar(xmlGetProp(node, reinterpret_cast<const xmlChar *>(attrName.c_str())), xmlFree);
    std::string retVal;
    if (xChar.get() != 0) {
        retVal = std::string(reinterpret_cast<char *>(xChar.get()));
    }
    return retVal;
}

std::string getXmlName(const xmlNodePtr node) {
    return std::string (reinterpret_cast<const char *>(node->name));
}
std::string getXmlContent(const xmlNodePtr node)
{
    if (node == 0) return "";
    boost::shared_ptr<xmlChar> xChar(xmlNodeGetContent(node), xmlFree);
    std::string retVal;
    if (xChar.get() != 0) {
        retVal = std::string(reinterpret_cast<char *>(xChar.get()));
    }
    return retVal;
}


}
