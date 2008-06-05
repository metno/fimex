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

#ifndef CDMWRITER_H_
#define CDMWRITER_H_

#include <string>
#include <boost/shared_ptr.hpp>
#include "CDMReader.h"

namespace MetNoFimex
{

class CDMWriter
{
public:
	CDMWriter(boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile) 
	: cdmReader(cdmReader), outputFile(outputFile) {}
	virtual ~CDMWriter() {}

protected:
	const boost::shared_ptr<CDMReader> cdmReader;
	const std::string outputFile;
};

} // namespace

#endif /*CDMWRITER_H_*/
