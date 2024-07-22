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
#include "fimex/StringUtils.h"

#include <sstream>
#include <stdexcept>

namespace {
MetNoFimex::Logger_p logger = MetNoFimex::getLogger("fimex.FileChunkReader");

std::shared_ptr<FILE> open_binary(const std::string& path)
{
    FILE* fileh = fopen(path.c_str(), "rb");
    if (!fileh)
        throw std::runtime_error("cannot open file '" + path + "'");
    return std::shared_ptr<FILE>(fileh, fclose);
}
} // namespace

namespace MetNoFimex {

FileChunkReader::FileChunkReader(const std::string& path)
    : path_(path)
    , file_(open_binary(path_))
{
    fseeko(file_.get(), 0, SEEK_END);
    size_ = ftello(file_.get());
    LOG4FIMEX(logger, Logger::DEBUG, "opened file '" << path_ << "'");
}

size_t FileChunkReader::size()
{
    return size_;
}

void FileChunkReader::read(size_t off, size_t count, unsigned char* buffer)
{
    std::lock_guard<std::mutex> lock(mutex_);
    LOG4FIMEX(logger, Logger::DEBUG, "file '" << path_ << "' read " << count << " bytes from " << off);
    if (fseeko(file_.get(), off, SEEK_SET) != 0) {
        std::ostringstream msg;
        msg << "error seeking to " << off << " in '" << path_ << "'";
        throw std::runtime_error(msg.str());
    }
    const size_t actual = fread(buffer, 1, count, file_.get());
    if (actual != count) {
        std::ostringstream msg;
        msg << "error reading " << count << " bytes starting at " << off << " from '" << path_ << "'";
        throw std::runtime_error(msg.str());
    }
}

} // namespace MetNoFimex
