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
        LOG4FIMEX(logger, MetNoFimex::Logger::ERROR, "cannot read " << count << " to buffer position " << wb->position << " with capacity " << wb->capacity);
#if 0
        std::ostringstream msg;
        msg << "buffer would overflow when adding " << count << " to buffer with capacity " << wb->capacity << " at " << wb->position;
        throw std::runtime_error(msg.str());
#else
        return 0;
#endif
    }

    std::copy((unsigned char*)contents, (unsigned char*)contents + count, wb->buffer + wb->position);
    wb->position += count;
    return count;
}

} // namespace

namespace MetNoFimex {

HttpChunkReader::HttpChunkReader(const std::string& url)
    : url_(url)
    , curl_(curl_open())
    , size_(0)
{
    curl_easy_setopt(curl_.get(), CURLOPT_NOBODY, 1L);
    CURLcode res = curl_easy_perform(curl_.get());
    curl_easy_setopt(curl_.get(), CURLOPT_NOBODY, 0L);
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
}

std::shared_ptr<CURL> HttpChunkReader::curl_open() const
{
    std::shared_ptr<CURL> curl(curl_easy_init(), curl_easy_cleanup);
    curl_easy_setopt(curl.get(), CURLOPT_URL, url_.c_str());
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

    char range[64];
    snprintf(range, sizeof(range), "%ld-%ld", off, off + count - 1);
    LOG4FIMEX(logger, Logger::DEBUG, "curl range='" << range << "'");
    curl_easy_setopt(curl_.get(), CURLOPT_RANGE, range);

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
