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

#include <curl/urlapi.h>

namespace MetNoFimex {

namespace {

// Returns a key identifying a server and its embedded credentials.
// The key covers scheme, userinfo (user:password), host, and port — but NOT
// the path — because TLS sessions and DNS results are bound to the server,
// not individual files.  Two URLs that differ only in path produce the same
// key and will therefore share TLS sessions and DNS results.
//
// Using libcurl's own URL parser (CURLU) avoids ambiguities with IPv6 address
// literals, percent-encoded characters, and default-port normalization.
//
// Falls back to a key built from the full URL string if parsing fails —
// safe but forfeits sharing.
ServerKey serverKey(const std::string& url)
{
    struct CurluGuard {
        CURLU* u;
        ~CurluGuard() { if (u) curl_url_cleanup(u); }
    } g{curl_url()};
    if (!g.u || curl_url_set(g.u, CURLUPART_URL, url.c_str(), 0) != CURLUE_OK)
        return ServerKey{url, {}, {}, {}, {}};

    auto get = [&](CURLUPart part, unsigned int flags = 0) -> std::string {
        char* val = nullptr;
        curl_url_get(g.u, part, &val, flags);
        std::string s;
        if (val) { s = val; curl_free(val); }
        return s;
    };

    return ServerKey{
        get(CURLUPART_SCHEME),
        get(CURLUPART_USER),
        get(CURLUPART_PASSWORD),
        get(CURLUPART_HOST),
        // CURLU_DEFAULT_PORT fills in the scheme's default (80 / 443) when
        // absent, so https://host and https://host:443 produce the same key.
        get(CURLUPART_PORT, CURLU_DEFAULT_PORT),
    };
}

} // namespace

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
    // Look up or create the share handle for this server + credentials.
    // readerFor() already holds mutex_, so server_shares_ needs no extra locking.
    const ServerKey key = serverKey(url);
    auto it = server_shares_.find(key);
    if (it == server_shares_.end())
        it = server_shares_.emplace(key, std::make_shared<HttpServerShare>()).first;
    return std::make_shared<HttpChunkReader>(url, it->second);
}

ChunkReader_p DefaultChunkReaderFactory::fileReaderFor(const std::string& filename)
{
    if (!file_cache_ || file_cache_->path() != filename)
        file_cache_ = std::make_shared<FileChunkReader>(filename);
    return file_cache_;
}

} // namespace MetNoFimex
