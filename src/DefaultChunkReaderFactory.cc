/*
  Fimex, src/DefaultChunkReaderFactory.cc

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

#include "DefaultChunkReaderFactory.h"

#include "fimex/ChunkReader.h"
#include "fimex/StringUtils.h"

#include "FileChunkReader.h"
#include "HttpChunkReader.h"

namespace MetNoFimex {

ChunkReader_p DefaultChunkReaderFactory::readerFor(const std::string& url)
{
    std::lock_guard<std::mutex> lock(mutex_);
    if (starts_with(url, "http://") || starts_with(url, "https://")) {
        return httpReaderFor(url);
    } else if (starts_with(url, "file:")) {
        return fileReaderFor(url.substr(5));
    } else {
        return fileReaderFor(url);
    }
}

ChunkReader_p DefaultChunkReaderFactory::httpReaderFor(const std::string& url)
{
    // TODO compare scheme + pass + host, but not path
    if (!http_cache_ || http_cache_->url() != url)
        http_cache_ = std::make_shared<HttpChunkReader>(url);
    return http_cache_;
}

ChunkReader_p DefaultChunkReaderFactory::fileReaderFor(const std::string& filename)
{
    if (!file_cache_ || file_cache_->path() != filename)
        file_cache_ = std::make_shared<FileChunkReader>(filename);
    return file_cache_;
}

} // namespace MetNoFimex
