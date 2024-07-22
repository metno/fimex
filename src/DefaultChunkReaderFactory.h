/*
  Fimex, src/DefaultChunkReaderFactory.h

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

#ifndef FIMEX_DefaultChunkReaderFactory_H
#define FIMEX_DefaultChunkReaderFactory_H 1

#include "fimex/ChunkReaderFactory.h"

#include "FileChunkReader.h"
#include "HttpChunkReader.h"

#include <memory>
#include <mutex>
#include <string>

namespace MetNoFimex {

class DefaultChunkReaderFactory : public ChunkReaderFactory
{
public:
    ChunkReader_p readerFor(const std::string& url) override;

private:
    ChunkReader_p httpReaderFor(const std::string& url);
    ChunkReader_p fileReaderFor(const std::string& filename);

private:
    std::mutex mutex_;

    FileChunkReader_p file_cache_;
    HttpChunkReader_p http_cache_;
};

} // namespace MetNoFimex

#endif // FIMEX_DefaultChunkReaderFactory_H
