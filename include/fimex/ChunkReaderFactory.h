/*
  Fimex, include/fimex/ChunkReaderFactory.h

  Copyright (C) 2024-2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef FIMEX_ChunkReaderFactory_H
#define FIMEX_ChunkReaderFactory_H 1

#include <fimex/ChunkReader.h>

#include <memory>
#include <string>

namespace MetNoFimex {

class ChunkReaderFactory
{
public:
    ~ChunkReaderFactory();

    virtual ChunkReader_p readerFor(const std::string& url) = 0;
};

typedef std::shared_ptr<ChunkReaderFactory> ChunkReaderFactory_p;

ChunkReaderFactory_p createDefaultChunkReaderFactory();

} // namespace MetNoFimex

#endif // FIMEX_ChunkReaderFactory_H
