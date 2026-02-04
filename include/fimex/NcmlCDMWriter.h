/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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
 */

#ifndef NULL_CDMWRITER_H_
#define NULL_CDMWRITER_H_

#include "fimex/CDMWriter.h"

#include <iosfwd>

namespace MetNoFimex {

class CDMAttribute;
class CDMDimension;
class CDMVariable;

/**
 * Write CDM content to a text file.
 */
class NcmlCDMWriter : public CDMWriter
{
public:
    NcmlCDMWriter(const CDMReader_p cdmReader, const std::string& outputFile);
    NcmlCDMWriter(const CDMReader_p cdmReader, std::ostream& output, bool withData);
    ~NcmlCDMWriter();

    static void write(std::ostream& out, const CDMDimension& dim);
    static void write(std::ostream& out, const CDMAttribute& att, const std::string& indent);
    static void write(std::ostream& out, const CDMVariable& var, const std::vector<CDMAttribute>& attrs, bool closeXML);

private:
    void write(std::ostream& out, bool withData);
};

} // namespace MetNoFimex

#endif /*NULL_CDMWRITER_H_*/
