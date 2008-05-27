#ifndef XMLDOC_H_
#define XMLDOC_H_
#include <boost/utility.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include "CDMException.h"
#include "XMLDoc.h"

namespace MetNoFimex
{

typedef boost::shared_ptr<xmlXPathObject> XPathObjPtr;

/**
 * a tiny wrapper around libxml dom and xpath reader with xml::include 
 */
class XMLDoc : boost::noncopyable
{
public:
	XMLDoc(const std::string& filename) throw(CDMException);
	virtual ~XMLDoc();
	XPathObjPtr getXPathObject(const std::string& xpath) const throw(CDMException);

private:
	xmlDoc* doc;
	xmlXPathContext* xpathCtx;
	void cleanup();
};

/**
 * a memory-save form of xmlGetProp
 * 
 * @return a string of the attribute, "" if attribute doesn't exist
 */
std::string getXmlProp(const xmlNodePtr node, const std::string& attrName);

/**
 * a memory-save form of xmlGetProp
 * 
 * @return a string of the attribute, "" if attribute doesn't exist
 */
std::string getXmlName(const xmlNodePtr node);


}

#endif /*XMLDOC_H_*/
