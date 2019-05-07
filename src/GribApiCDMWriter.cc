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

#include "fimex/GribApiCDMWriter.h"

#include "GribApiCDMWriter_Impl1.h"
#include "GribApiCDMWriter_Impl2.h"
#include "fimex/Type2String.h"

namespace MetNoFimex
{

GribApiCDMWriter::GribApiCDMWriter(CDMReader_p cdmReader, const std::string& outputFile, const int gribVersion, const std::string& configFile)
: CDMWriter(cdmReader, outputFile)
{
	if (gribVersion == 1) {
		GribApiCDMWriter_Impl1(cdmReader, outputFile, configFile).run();
	} else if (gribVersion == 2) {
		GribApiCDMWriter_Impl2(cdmReader, outputFile, configFile).run();
	} else {
		throw CDMException("Unknown grib Version: " + type2string(gribVersion));
	}
}

GribApiCDMWriter::~GribApiCDMWriter()
{
}



}
