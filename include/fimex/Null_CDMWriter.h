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

#ifndef NULL_CDMWRITER_H_
#define NULL_CDMWRITER_H_

#include "fimex/CDMWriter.h"

namespace MetNoFimex
{

/**
 * CDMWriter does all operations as the NetCDF_CDMWriter, except writing to the file.
 * This class is useful for performance tests.
 */
class Null_CDMWriter : public CDMWriter
{
public:
	Null_CDMWriter(const CDMReader_p cdmReader, const std::string& outputFile);
	virtual ~Null_CDMWriter();
private:
};

}

#endif /*NULL_CDMWRITER_H_*/
