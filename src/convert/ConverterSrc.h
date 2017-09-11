/*
 * Fimex, ConverterSrc.h
 *
 * (C) Copyright 2015, met.no
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
 *  Created on: May 15, 2015
 *      Author: heikok
 */
#ifndef CONVERTERSRC_H_
#define CONVERTERSRC_H_

#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{

class CDMReader;
class ConverterSrc;
typedef boost::shared_ptr<ConverterSrc> ConverterSrcPtr;


/**
 * Class to bundle a CDMReader and restrictions like (only these few variables)
 *
 * TODO: restrictions not implemented yet
 */
class ConverterSrc
{
public:
    ConverterSrc(CDMReader_p reader) : reader_(reader) {}
    ~ConverterSrc() {}
    CDMReader_p getReader() {return reader_;}
private:
    CDMReader_p reader_;
};

} /* namespace MetNoFimex */

#endif /* CONVERTERSRC_H_ */
