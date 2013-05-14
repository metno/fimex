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

#ifndef XMLDOC_H_
#define XMLDOC_H_
#include <boost/utility.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include "fimex/CDMException.h"

//forward decl;
struct _xmlDoc;
struct _xmlXPathContext;
struct _xmlXPathObject;
struct _xmlNode;
typedef struct _xmlDoc xmlDoc;
typedef struct _xmlNode xmlNode;
typedef struct _xmlXPathContext xmlXPathContext;
typedef xmlNode* xmlNodePtr;
typedef struct _xmlXPathObject xmlXPathObject;

namespace MetNoFimex
{

typedef boost::shared_ptr<xmlXPathObject> XPathObjPtr;

/**
 * a tiny wrapper around libxml dom and xpath reader with xml::include
 */
class XMLDoc : boost::noncopyable
{
public:
	/**
	 * initialization of libxml and the xml config file
	 *
	 * @param filename xml input-file
	 * @throw CDMException if problems with libxml or problems with input-file
	 */
	explicit XMLDoc(const std::string& filename);
	virtual ~XMLDoc();
	/**
	 * get a ptr to the node defined by xpath
	 *
	 * @param xpath xpath string for the node
	 * @return an xpathobj, which is != 0, but might have 0 elements, i.e. nodesetval == 0 or nodesetval->nodeNr == 0
	 * @throw CDMException if xpath is not parsable
	 */
	XPathObjPtr getXPathObject(const std::string& xpath, xmlNodePtr node = 0) const;
	/**
	 * @brief register a namespace for later xpath
	 *
	 * register a namespace with a prefix for later xpath retrievals
	 * @param prefix short name for namespace
	 * @param uri full namespace name
	 */
	void registerNamespace(const std::string& prefix, const std::string& uri);

	/**
	 *
	 * make a string representation of a node as full xml-document
	 *
	 * @param a node to print
	 * @return string
	 */
	std::string toString(const xmlNodePtr node);

    static boost::shared_ptr<XMLDoc> fromFile(const std::string& filename);
    static boost::shared_ptr<XMLDoc> fromString(const std::string& buffer, const std::string& url = "");
    static boost::shared_ptr<XMLDoc> fromURL(const std::string& url);

private:
        /**
          * helper functions
          */
        explicit XMLDoc();
        void init();
        void setDoc(xmlDoc* pdoc);
        void setXPathCtx(xmlDoc* pdoc);
        /**
          * some helper functions
          */
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
 * a memory-save form of xmlGetName
 *
 * @return a string of the attribute, "" if attribute doesn't exist
 */
std::string getXmlName(const xmlNodePtr node);
/**
 * @brief get all text-contents of the node or underlying nodes
 *
 * @param node the xmlNodePtr or xmlNodePtr as list
 * @return string with text-content, or ""
 * @throws CDMException
 */
std::string getXmlContent(const xmlNodePtr node);


}

#endif /*XMLDOC_H_*/
