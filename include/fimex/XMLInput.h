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

#ifndef XMLINPUT_H_
#define XMLINPUT_H_

#include "fimex/XMLDocDecl.h"

#include <string>

namespace MetNoFimex {

/**
 * @headerfile fimex/XMLInput.h
 */
/**
 * Interface for different XML sources like URL, file or string
 */
class XMLInput
{
public:
    virtual ~XMLInput() {}
    /**
     * retrieve the XMLDoc
     *
     * @return XMLDoc
     * @throw CDMException
     */
    virtual XMLDoc_p getXMLDoc() const = 0;

    /**
     * return an identifier of the XMLInput
     */
    virtual std::string id() const = 0;

    /**
     * check if information is available
     */
    virtual bool isEmpty() const {return (id().empty());}
};

} // namespace MetNoFimex

#endif /* XMLINPUT_H_ */
