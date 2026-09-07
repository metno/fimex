/*
  Fimex, src/FileChunkReader.h

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

#ifndef FIMEX_FileChunkReader_H
#define FIMEX_FileChunkReader_H 1

#include "fimex/ChunkReader.h"

#include <string>

namespace MetNoFimex {

/**
 * A ChunkReader backed by a local file.
 *
 * Uses pread(2) for all data access, so concurrent reads from multiple
 * threads on the same instance are safe without any locking.
 */
class FileChunkReader : public ChunkReader
{
public:
    FileChunkReader(const std::string& path);
    ~FileChunkReader();

    const std::string& path() const { return path_; }

    size_t size() override;
    void read(size_t offset, size_t count, unsigned char* buffer) override;

private:
    std::string path_;
    int fd_;
    size_t size_;
};

typedef std::shared_ptr<FileChunkReader> FileChunkReader_p;

} // namespace MetNoFimex

#endif // FIMEX_FileChunkReader_H
