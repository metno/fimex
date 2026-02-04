/*
 * Fimex, CDM_XMLConfigHelper.h
 *
 * (C) Copyright 2009-2026, met.no
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
 *
 *  Created on: Sep 16, 2009
 *      Author: Heiko Klein
 */

#ifndef CDM_XMLCONFIGHELPER_H_
#define CDM_XMLCONFIGHELPER_H_

#include <map>
#include <memory>
#include <vector>

// forward decl, originally in <libxml/tree.h>
typedef struct _xmlNode xmlNode;
typedef xmlNode* xmlNodePtr;

namespace MetNoFimex {

// forward decl.
class ReplaceStringObject;
class CDMAttribute;
class XMLDoc;

/**
 * some function to help reading xml-configuration files containing
 * cdm information like attribute - name/value pairs
 *
 * known usages are FeltCDMReader(2) and GribCDMReader
 */

/**
 * read all `<attribute .../>` subnodes of this node and add them to attributes, replace values by templateReplacements as needed
 */
void fillAttributeListFromXMLNode(std::vector<CDMAttribute>& attributes, const xmlNodePtr node,
                                  const std::map<std::string, std::shared_ptr<ReplaceStringObject>>& templateReplacements);

/**
 * read a xml-node retrieved by the xpathString and extract the nodes attributes and all `<attributes>` sub-elements with name, value and type
 *
 * @param doc the document to read from
 * @param xpathString the string leading to the node
 * @param xmlAttributes returns all attributes of the first node matched
 * @param varAttributes returns all `<attribute .../>` sub elements of this node
 * @param templateReplacements the CDMAttribute values may containt templates (%VAR%) wich are replaced by these values
 * @return number of nodes matched (only the first has been read)
 */
int readXPathNodeWithCDMAttributes(const XMLDoc& doc, const std::string& xpathString, std::map<std::string, std::string>& xmlAttributes,
                                   std::vector<CDMAttribute>& varAttributes,
                                   const std::map<std::string, std::shared_ptr<ReplaceStringObject>>& templateReplacements);

} // namespace MetNoFimex

#endif /* CDM_XMLCONFIGHELPER_H_ */
