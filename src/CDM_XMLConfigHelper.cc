/*
 * Fimex, CDM_XMLConfigHelper.cc
 *
 * (C) Copyright 2009-2022, met.no
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
 *
 *  Created on: Sep 16, 2009
 *      Author: Heiko Klein
 */

#include "fimex/CDM_XMLConfigHelper.h"

#include "fimex/CDMAttribute.h"
#include "fimex/CDMException.h"
#include "fimex/CDMVariable.h"
#include "fimex/ReplaceStringObject.h"
#include "fimex/StringUtils.h"
#include "fimex/XMLUtils.h"

#include <regex>

namespace MetNoFimex {

using namespace std;

static string replaceTemplateAttribute(string value, const map<string, std::shared_ptr<ReplaceStringObject>> templateReplacements)
{
    for (auto& tr : templateReplacements) {
        stringstream outString;
        std::regex rgx("%" + tr.first + "(\\(([^,]*),?(.*)?\\))?" + "%");
        string::const_iterator  begin = value.begin(), end = value.end();
        std::match_results<string::const_iterator> matches;
        while (std::regex_search(begin, end, matches, rgx)) {
            outString << string(begin, matches[0].first);
            auto& rso = tr.second;
            string var(matches[1].first, matches[1].second);
            if (matches.size() > 2) {
                string match2(matches[2].first, matches[2].second);
                std::vector<std::string> options;
                if (matches.size() > 3) {
                    string match3(matches[3].first, matches[3].second);
                    options = tokenize(match3, ",");
                }
                // values within the inner brackets
                rso->setFormatStringAndOptions(match2, options);
            }
            rso->put(outString);
            begin = matches[0].second;
        }
        // add the last non-matching bit
        outString << string(begin, end);
        value = outString.str();
    }
    return value;
}

void fillAttributeListFromXMLNode(vector<CDMAttribute>& attributes, const xmlNodePtr node,
                                  const std::map<std::string, std::shared_ptr<ReplaceStringObject>>& templateReplacements)
{
    if (!node)
        return;
    if ((node->type == XML_ELEMENT_NODE) &&
        (string("attribute") == reinterpret_cast<const char *>(node->name))) {
            string name = getXmlProp(node, "name");
            string value = getXmlProp(node, "value");
            string type = getXmlProp(node, "type");

            value = replaceTemplateAttribute(value, templateReplacements);
            attributes.push_back(CDMAttribute(name,type,value));
    }
    fillAttributeListFromXMLNode(attributes, node->next, templateReplacements);
}

int readXPathNodeWithCDMAttributes(const XMLDoc& doc, const string& xpathString, std::map<string, string>& xmlAttributes,
                                   std::vector<CDMAttribute>& varAttributes, const map<string, std::shared_ptr<ReplaceStringObject>>& templateReplacements)
{
    XPathNodeSet nodes(doc, xpathString);
    if (nodes.size() == 0) {
        return 0;
    }

    // only parsing node[0]
    auto node = nodes[0];
    if (node->type != XML_ELEMENT_NODE) {
        throw CDMException("xpath does not point to XML_ELEMENT_NODE: " + xpathString);
    }
    xmlAttrPtr attr = node->properties;
    while (attr != 0) {
        string name(reinterpret_cast<const char *>(attr->name));
        string value(reinterpret_cast<const char *>(attr->children->content));
        xmlAttributes[name] = value;
        attr = attr->next;
    }
    fillAttributeListFromXMLNode(varAttributes, node->children, templateReplacements);
    return nodes.size();
}

} // end of MetNoFimex
