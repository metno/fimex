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

#ifndef METGM_CDMWRITER_HPP
#define METGM_CDMWRITER_HPP

#include "fimex/CDMWriter.h"

namespace MetNoFimex {

/* forward declarations */
class MetGmCDMWriterImpl;

class MetGmCDMWriter : public CDMWriter
{
public:
    MetGmCDMWriter(CDMReader_p cdmReader, const std::string& outputFile, const std::string& configFile);
    ~MetGmCDMWriter();

protected:
    std::unique_ptr<MetGmCDMWriterImpl> d_ptr;
};

} // namespace MetNoFimex

#endif // METGM_CDMWRITER_HPP
