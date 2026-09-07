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
#include "LruCache.h"

#include <functional>
#include <list>
#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>

namespace MetNoFimex {

// Key identifying a server and its embedded credentials.
// The path is excluded: TLS sessions and DNS results are bound to the server,
// not to individual files, so readers for different files on the same server
// can share them.
struct ServerKey
{
    std::string scheme;
    std::string user;
    std::string password;
    std::string host;
    std::string port; // normalised to include the default for the scheme

    bool operator==(const ServerKey&) const = default;
};

struct ServerKeyHash
{
    size_t operator()(const ServerKey& k) const
    {
        // Mix hashes of all five fields using a standard combination step so
        // that no single field's hash dominates and field order matters.
        size_t h = 0;
        auto mix = [&](const std::string& s) {
            h ^= std::hash<std::string>{}(s) + 0x9e3779b9u + (h << 6) + (h >> 2);
        };
        mix(k.scheme);
        mix(k.user);
        mix(k.password);
        mix(k.host);
        mix(k.port);
        return h;
    }
};

class DefaultChunkReaderFactory : public ChunkReaderFactory
{
public:
    DefaultChunkReaderFactory();
    ~DefaultChunkReaderFactory();

    ChunkReader_p readerFor(const std::string& url) override;

private:
    ChunkReader_p httpReaderFor(const std::string& url);
    ChunkReader_p fileReaderFor(const std::string& filename);

private:
    std::mutex mutex_;

    LruCache<std::string, FileChunkReader_p> file_cache_;
    LruCache<std::string, HttpChunkReader_p> http_cache_;

    // One HttpServerShare per ServerKey (scheme + credentials + host + port).
    // Readers for different files on the same server share TLS sessions and
    // DNS results; see HttpServerShare for details.
    std::unordered_map<ServerKey, HttpServerShare_p, ServerKeyHash> server_shares_;
};

} // namespace MetNoFimex

#endif // FIMEX_DefaultChunkReaderFactory_H
