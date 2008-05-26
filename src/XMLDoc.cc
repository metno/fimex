#include "XMLDoc.h"
#include <libxml/xinclude.h>


namespace MetNoFimex
{

XMLDoc::XMLDoc(const std::string& filename) throw(CDMException)
{
	xmlInitParser();
    LIBXML_TEST_VERSION
    doc = xmlReadFile(filename.c_str(), NULL, 0);
    if (doc == 0) {
    	cleanup();
    	throw CDMException("unable to parse config file: " + filename);
    }
    // apply the XInclude process
    if (xmlXIncludeProcess(doc) < 0) {
    	cleanup();
    	throw CDMException("XInclude processing failed: " + filename);
    }
	xpathCtx = xmlXPathNewContext(doc);
	if (xpathCtx == 0) {
		cleanup();
		throw CDMException("unable to generate xpath context for " + filename);
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

XPathObjPtr XMLDoc::getXPathObject(const std::string& xpath) const throw(CDMException)
{
	XPathObjPtr xpathObj(xmlXPathEvalExpression(reinterpret_cast<const xmlChar*>(xpath.c_str()), xpathCtx), xmlXPathFreeObject);
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


}
