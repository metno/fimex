/*
  Fimex, src/FileChunkReader.cc

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

#include "FileChunkReader.h"

#include "fimex/Logger.h"

#include <sstream>
#include <stdexcept>

#include <cerrno>
#include <cstring>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

namespace {
MetNoFimex::Logger_p logger = MetNoFimex::getLogger("fimex.FileChunkReader");
} // namespace

namespace MetNoFimex {

FileChunkReader::FileChunkReader(const std::string& path)
    : path_(path)
    , fd_(-1)
    , size_(0)
{
    fd_ = ::open(path_.c_str(), O_RDONLY);
    if (fd_ < 0)
        throw std::runtime_error("cannot open file '" + path_ + "': " + std::strerror(errno));

    struct stat st;
    if (::fstat(fd_, &st) != 0) {
        ::close(fd_);
        throw std::runtime_error("cannot stat file '" + path_ + "': " + std::strerror(errno));
    }
    size_ = static_cast<size_t>(st.st_size);

    LOG4FIMEX(logger, Logger::DEBUG, "opened file '" << path_ << "'");
}

FileChunkReader::~FileChunkReader()
{
    if (fd_ >= 0)
        ::close(fd_);
}

size_t FileChunkReader::size()
{
    return size_;
}

void FileChunkReader::read(size_t off, size_t count, unsigned char* buffer)
{
    LOG4FIMEX(logger, Logger::DEBUG, "file '" << path_ << "' read " << count << " bytes from " << off);

    size_t done = 0;
    while (done < count) {
        const ssize_t n = ::pread(fd_, buffer + done, count - done, static_cast<off_t>(off + done));
        if (n > 0) {
            done += static_cast<size_t>(n);
        } else if (n == 0) {
            // unexpected EOF
            std::ostringstream msg;
            msg << "unexpected EOF reading " << count << " bytes at offset " << off << " from '" << path_ << "' (got " << done << ")";
            throw std::runtime_error(msg.str());
        } else if (errno == EINTR) {
            continue; // signal interrupted — retry
        } else {
            std::ostringstream msg;
            msg << "error reading " << count << " bytes at offset " << off << " from '" << path_ << "': " << std::strerror(errno);
            throw std::runtime_error(msg.str());
        }
    }
}

} // namespace MetNoFimex
