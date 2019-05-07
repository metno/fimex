/*
 * Fimex, ProradXMLCDMReader.h
 *
 * (C) Copyright 2014, met.no
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
 *  Created on: Jan 27, 2014
 *      Author: Heiko Klein
 */
#ifndef PRORADXMLCDMREADER_H_
#define PRORADXMLCDMREADER_H_

#include "fimex/CDMReader.h"

namespace MetNoFimex
{

class ProradXMLCDMReader : public CDMReader
{
public:
    /**
     * Reader for cartesian prorad-xml files with the proradxml-rw library.
     *
     * @param source file-source, readable by libxml2
     */
    ProradXMLCDMReader(const std::string& source);
    ~ProradXMLCDMReader();

    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos);

private:
    class Impl;
    std::unique_ptr<Impl> pimpl_;
};

} /* namespace MetNoFimex */

#endif /* PRORADXMLCDMREADER_H_ */
