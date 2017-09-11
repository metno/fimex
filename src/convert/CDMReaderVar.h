/*
 * Fimex, CDMReaderVar.h
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
 *  Created on: May 19, 2015
 *      Author: heikok
 */
#ifndef CDMREADERVAR_H_
#define CDMREADERVAR_H_

#include <boost/shared_ptr.hpp>
#include <string>

namespace MetNoFimex
{

class CDMReader;
/**
 * Helper class to identify a variable in a certain reader
 */
struct CDMReaderVar
{
    CDMReader_p reader;
    std::string varName;
    CDMReaderVar(CDMReader_p reader, std::string varName) : reader(reader), varName(varName) {}
    ~CDMReaderVar() {}
};

} /* namespace MetNoFimex */

#endif /* CDMREADERVAR_H_ */
