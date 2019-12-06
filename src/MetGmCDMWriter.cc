/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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

#include "fimex/MetGmCDMWriter.h"

#include "metgm/MetGmCDMWriterSlicedImpl.h"

namespace MetNoFimex {

MetGmCDMWriter::MetGmCDMWriter(CDMReader_p cdmReader, const std::string& outputFile, const std::string& configFile)
    : CDMWriter(cdmReader, outputFile)
    , d_ptr(new MetGmCDMWriterSlicedImpl(cdmReader, outputFile, configFile))
{
    d_ptr->configureAndWrite();
}

MetGmCDMWriter::~MetGmCDMWriter() {}

} // namespace MetNoFimex
