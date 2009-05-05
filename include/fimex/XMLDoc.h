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
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include "fimex/CDMException.h"
#include "fimex/XMLDoc.h"

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
	XMLDoc(const std::string& filename) throw(CDMException);
	virtual ~XMLDoc();
	/**
	 * get a ptr to the node defined by xpath
	 *
	 * @param xpath xpath string for the node
	 * @return an xpathobj, which is != 0, but might have 0 elements, i.e. nodesetval == 0 or nodesetval->nodeNr == 0
	 * @throw CDMException if xpath is not parsable
	 */
	XPathObjPtr getXPathObject(const std::string& xpath) const throw(CDMException);
	/**
	 * @brief register a namespace for later xpath
	 *
	 * register a namespace with a prefix for later xpath retrievals
	 * @param prefix short name for namespace
	 * @param uri full namespace name
	 */
	void registerNamespace(const std::string& prefix, const std::string& uri) throw(CDMException);

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
 * a memory-save form of xmlGetName
 *
 * @return a string of the attribute, "" if attribute doesn't exist
 */
std::string getXmlName(const xmlNodePtr node);


}

#endif /*XMLDOC_H_*/
