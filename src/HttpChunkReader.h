/*
  Fimex, src/HttpChunkReader.h

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

#ifndef FIMEX_HttpChunkReader_H
#define FIMEX_HttpChunkReader_H 1

#include "fimex/ChunkReader.h"

#include <memory>
#include <mutex>
#include <string>

#include <curl/curl.h>
#include <curl/urlapi.h>

namespace MetNoFimex {

// Holds a CURLSH (curl share handle) that lets independent easy handles
// targeting the same server reuse TLS sessions and DNS results.  A separate
// share object is kept per (scheme, credentials, host, port) tuple so that
// servers with different credentials are never mixed.
//
// CURL_LOCK_DATA_CONNECT (connection-cache sharing) is intentionally NOT
// included: libcurl does not support sharing live connections between
// concurrent threads via CURLSH.
struct HttpServerShare
{
    HttpServerShare();

    std::shared_ptr<CURLSH> sh;

    // libcurl requires external locking when a share handle is used from
    // multiple threads.  One mutex per data type is sufficient.
    std::mutex ssl_mutex;
    std::mutex dns_mutex;
};

typedef std::shared_ptr<HttpServerShare> HttpServerShare_p;

class HttpChunkReader : public ChunkReader
{
public:
    /// @param share  Optional server share for TLS/DNS reuse.  Pass nullptr
    ///               (or omit) to create an unshared reader.
    HttpChunkReader(const std::string& url, HttpServerShare_p share = nullptr);
    const std::string& url() { return url_; }

    size_t size() override;
    void read(size_t offset, size_t count, unsigned char* buffer) override;

private:
    std::shared_ptr<CURL> curl_open() const;

    /// Returns true when the server advertised Accept-Ranges: bytes.
    /// read() will throw for non-zero offsets when this is false.
    bool acceptsRanges() const { return accepts_ranges_; }

private:
    // share_ must be declared before curl_ so that curl_ is destroyed first
    // (C++ destroys members in reverse declaration order).  curl_easy_cleanup
    // must run before curl_share_cleanup to avoid use-after-free inside libcurl.
    HttpServerShare_p share_;
    std::mutex mutex_;
    std::string url_;
    std::shared_ptr<CURL> curl_;
    size_t size_;
    bool accepts_ranges_;
};

typedef std::shared_ptr<HttpChunkReader> HttpChunkReader_p;

} // namespace MetNoFimex

#endif // FIMEX_HttpChunkReader_H
