/*
 * Fimex
 *
 * (C) Copyright 2019-2026, met.no
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

#ifndef XMLUTILS_H_
#define XMLUTILS_H_

#include "fimex/XMLDoc.h"
#include "fimex/XMLInput.h"
#include "fimex/XMLInputDoc.h"

#include <string>

#include <libxml/xmlreader.h>
#include <libxml/xpath.h>

namespace MetNoFimex {

class XmlCharPtr {
public:
  XmlCharPtr(xmlChar* p) : p_(p) {}
  ~XmlCharPtr() { if (p_) xmlFree(p_); }

  const char* to_cc() const { return reinterpret_cast<const char*>(p_); }

  std::string to_string() const;
  float to_float() const;
  double to_double() const;
  long to_long() const;
  long long to_longlong() const;

  int cmp(const char* text) const;
  size_t len() const;

  bool operator==(const char* text) const { return cmp(text) == 0; }

private:
  xmlChar* p_;
};

class XPathNodeSet {
public:
    class iterator {
    public:
        iterator(const XPathNodeSet& p, int i) : parent(p), index(i) {}

        iterator& operator++() { ++index; return *this; }
        iterator operator++(int) { iterator it(*this); ++index; return it; }

        xmlNodePtr operator*() { return parent.at(index); }

        bool operator==(const iterator& other) const { return &parent == &other.parent && index == other.index; }
        bool operator!=(const iterator& other) const { return !(*this == other); }

    private:
        const XPathNodeSet& parent;
        int index;
    };

public:
    XPathNodeSet(const XMLDoc& doc, const std::string& xpath, xmlNodePtr node = nullptr);
    XPathNodeSet(XMLDoc_p doc, const std::string& xpath, xmlNodePtr node = nullptr)
        : XPathNodeSet(*doc, xpath, node) { }
    XPathNodeSet(std::unique_ptr<XMLDoc>& doc, const std::string& xpath, xmlNodePtr node = nullptr)
        : XPathNodeSet(*doc, xpath, node) { }
    XPathNodeSet(xmlXPathObject_p xpo);

    iterator begin() const { return iterator(*this, 0); }
    iterator end() const { return iterator(*this, size_); }

    int size() const { return size_; }
    xmlNodePtr at(int index) const { if (index >= 0 && index < size_) return nodes_->nodeTab[index]; else return nullptr; }
    xmlNodePtr operator[](int index) const { return at(index); }

private:
    xmlXPathObject_p xpo_;
    xmlNodeSetPtr nodes_;
    int size_;
};

XMLInputDoc createXMLInput(const XMLInput& xi);
XMLInputDoc createXMLInput(const std::string& configXML);

std::string escapeXmlString(const std::string& input);
void escapeXmlToStream(std::ostream& output, const std::string& input);

/// Helper for ostream operator
class EscapeXmlOp
{
public:
    explicit EscapeXmlOp(const std::string& str)
        : input(str)
    {
    }

    friend std::ostream& operator<<(std::ostream& os, const EscapeXmlOp& esc)
    {
        escapeXmlToStream(os, esc.input);
        return os;
    }

private:
    const std::string& input;
};

inline EscapeXmlOp escapeXml(const std::string& str)
{
    return EscapeXmlOp(str);
}

} // namespace MetNoFimex

#endif /* XMLUTILS_H_ */
