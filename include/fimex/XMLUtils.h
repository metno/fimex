/*
 * Fimex
 *
 * (C) Copyright 2019-2022, met.no
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

#ifndef XMLUTILS_H_
#define XMLUTILS_H_

#include <string>
#include <libxml/xmlreader.h>

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

} // namespace MetNoFimex

#endif /* XMLUTILS_H_ */
