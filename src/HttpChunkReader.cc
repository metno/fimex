/*
  Fimex, src/HttpChunkReader.cc

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

#include "HttpChunkReader.h"

#include "fimex/Logger.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace {
MetNoFimex::Logger_p logger = MetNoFimex::getLogger("fimex.HttpChunkReader");

struct WriteBuffer
{
    unsigned char* buffer;
    size_t position;
    size_t capacity;
};

size_t WriteCallback(void* contents, size_t size, size_t nmemb, WriteBuffer* wb)
{
    size_t count = size * nmemb;
    size_t oldLength = wb->position;
    if (wb->position + count > wb->capacity) {
#if 1
        std::ostringstream msg;
        msg << "buffer would overflow when adding " << count << " to buffer with capacity " << wb->capacity << " at " << wb->position;
        throw std::runtime_error(msg.str());
#else
        LOG4FIMEX(logger, MetNoFimex::Logger::ERROR, "cannot read " << count << " to buffer position " << wb->position << " with capacity " << wb->capacity);
        return 0;
#endif
    }

    std::copy((unsigned char*)contents, (unsigned char*)contents + count, wb->buffer + wb->position);
    wb->position += count;
    return count;
}

/// Collects selected response headers from a HEAD request.
struct HeadInfo
{
    bool accepts_ranges = false; ///< true iff "Accept-Ranges: bytes" was seen
};

/// curl HEADERFUNCTION callback — called once per header line (including CRLF).
size_t HeaderCallback(char* buffer, size_t size, size_t nitems, HeadInfo* info)
{
    const size_t total = size * nitems;
    // Build a lower-case copy so we can do case-insensitive matching (HTTP
    // header field names are case-insensitive per RFC 7230 §3.2).
    std::string line(buffer, total);
    std::transform(line.begin(), line.end(), line.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    // Strip trailing CRLF / LF.
    while (!line.empty() && (line.back() == '\r' || line.back() == '\n'))
        line.pop_back();

    // "accept-ranges: bytes"  →  server supports byte-range requests.
    // "accept-ranges: none"   →  server explicitly declares no range support.
    // Absent header           →  treat as no-range (conservative default).
    if (line.rfind("accept-ranges:", 0) == 0) {
        info->accepts_ranges = (line.find("bytes") != std::string::npos);
        LOG4FIMEX(logger, MetNoFimex::Logger::DEBUG, "curl Accept-Ranges: " << (info->accepts_ranges ? "bytes" : "none/absent"));
    }
    return total;
}

} // namespace

namespace MetNoFimex {

// --- HttpServerShare --------------------------------------------------------

namespace {

void share_lock_cb(CURL*, curl_lock_data data, curl_lock_access, void* userptr)
{
    auto* s = static_cast<HttpServerShare*>(userptr);
    if (data == CURL_LOCK_DATA_SSL_SESSION)
        s->ssl_mutex.lock();
    else if (data == CURL_LOCK_DATA_DNS)
        s->dns_mutex.lock();
}

void share_unlock_cb(CURL*, curl_lock_data data, void* userptr)
{
    auto* s = static_cast<HttpServerShare*>(userptr);
    if (data == CURL_LOCK_DATA_SSL_SESSION)
        s->ssl_mutex.unlock();
    else if (data == CURL_LOCK_DATA_DNS)
        s->dns_mutex.unlock();
}

} // namespace

HttpServerShare::HttpServerShare()
    : sh(curl_share_init(), [](CURLSH* s) { curl_share_cleanup(s); })
{
    if (!sh)
        throw std::runtime_error("curl_share_init failed");
    curl_share_setopt(sh.get(), CURLSHOPT_LOCKFUNC, share_lock_cb);
    curl_share_setopt(sh.get(), CURLSHOPT_UNLOCKFUNC, share_unlock_cb);
    // userptr is the address of this HttpServerShare, which is stable because
    // instances are always heap-allocated and managed by shared_ptr.
    curl_share_setopt(sh.get(), CURLSHOPT_USERDATA, this);
    curl_share_setopt(sh.get(), CURLSHOPT_SHARE, CURL_LOCK_DATA_SSL_SESSION);
    curl_share_setopt(sh.get(), CURLSHOPT_SHARE, CURL_LOCK_DATA_DNS);
}

// --- HttpChunkReader --------------------------------------------------------

HttpChunkReader::HttpChunkReader(const std::string& url, HttpServerShare_p share)
    : share_(std::move(share))
    , url_(url)
    , curl_(curl_open())
    , size_(0)
    , accepts_ranges_(false)
{
    HeadInfo head;
    curl_easy_setopt(curl_.get(), CURLOPT_NOBODY, 1L);
    curl_easy_setopt(curl_.get(), CURLOPT_HEADERFUNCTION, HeaderCallback);
    curl_easy_setopt(curl_.get(), CURLOPT_HEADERDATA, &head);
    CURLcode res = curl_easy_perform(curl_.get());
    // Tear down HEAD-specific options before any subsequent GET requests.
    curl_easy_setopt(curl_.get(), CURLOPT_NOBODY, 0L);
    curl_easy_setopt(curl_.get(), CURLOPT_HEADERFUNCTION, nullptr);
    curl_easy_setopt(curl_.get(), CURLOPT_HEADERDATA, nullptr);
    if (res == CURLE_OK) {
        curl_off_t cl;
        res = curl_easy_getinfo(curl_.get(), CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, &cl);
        if (res == CURLE_OK) {
            size_ = cl;
            LOG4FIMEX(logger, Logger::DEBUG, "curl '" << url_ << "' has size " << size_);
        }
    }
    if (res != CURLE_OK) {
        std::ostringstream msg;
        msg << "curl HEAD / Content-Length error";
        throw std::runtime_error(msg.str());
    }
    accepts_ranges_ = head.accepts_ranges;
    LOG4FIMEX(logger, Logger::DEBUG, "curl '" << url_ << "' accepts_ranges=" << accepts_ranges_);
}

std::shared_ptr<CURL> HttpChunkReader::curl_open() const
{
    std::shared_ptr<CURL> curl(curl_easy_init(), curl_easy_cleanup);
    curl_easy_setopt(curl.get(), CURLOPT_URL, url_.c_str());
    if (share_)
        curl_easy_setopt(curl.get(), CURLOPT_SHARE, share_->sh.get());
    return curl;
}

size_t HttpChunkReader::size()
{
    return size_;
}

void HttpChunkReader::read(size_t off, size_t count, unsigned char* buffer)
{
    std::lock_guard<std::mutex> lock(mutex_);
    LOG4FIMEX(logger, Logger::DEBUG, "curl read " << count << " at offset " << off);
    if (off + count > size_) {
        std::ostringstream msg;
        msg << "reading past end";
        throw std::runtime_error(msg.str());
    }
    if (!accepts_ranges_ && off != 0) {
        std::ostringstream msg;
        msg << "server does not support HTTP range requests; cannot read at offset " << off << " from '" << url_ << "'";
        throw std::runtime_error(msg.str());
    }

    if (off == 0 && count == size_) {
        // Full-file read — no Range header needed (works even without range support).
        curl_easy_setopt(curl_.get(), CURLOPT_RANGE, nullptr);
    } else if (accepts_ranges_) {
        char range[64];
        snprintf(range, sizeof(range), "%ld-%ld", off, off + count - 1);
        LOG4FIMEX(logger, Logger::DEBUG, "curl range='" << range << "'");
        curl_easy_setopt(curl_.get(), CURLOPT_RANGE, range);
    } else {
        // server does not accept range requests, so we cannot fulfil the read request
        std::ostringstream msg;
        msg << "HTTPChunkReader: refusing range request for '" << url_ << "' as the server does not announce support for that";
        throw std::runtime_error(msg.str());
    }

    WriteBuffer wb{buffer, 0, count};
    curl_easy_setopt(curl_.get(), CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl_.get(), CURLOPT_WRITEDATA, &wb);
    CURLcode res = curl_easy_perform(curl_.get());
    if (res != CURLE_OK) {
        std::ostringstream msg;
        msg << "curl GET error " << res;
        throw std::runtime_error(msg.str());
    }
}

} // namespace MetNoFimex
