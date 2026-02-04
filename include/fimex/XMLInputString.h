/*
 * Fimex, XMLInput.h
 *
 * (C) Copyright 2011-2026, met.no
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
 *  Created on: Oct 25, 2011
 *      Author: Heiko Klein
 */

#ifndef FIMEX_XMLINPUTSTRING_H
#define FIMEX_XMLINPUTSTRING_H

#include "fimex/XMLInput.h"

namespace MetNoFimex {

/**
 * @headerfile fimex/XMLInputString.h
 */

class XMLInputString : public XMLInput
{
private:
    std::string content_;
    std::string url_;

public:
    /**
     * parse a in-memory xml-content. The url will be neede for external
     * references like style-definitions/-sheets or xincludes
     *
     */
    XMLInputString(const std::string& content, const std::string& url = std::string())
        : content_(content)
        , url_(url)
    {
    }
    XMLDoc_p getXMLDoc() const override;
    std::string id() const override;
};

} // namespace MetNoFimex

#endif // FIMEX_XMLINPUTSTRING_H
