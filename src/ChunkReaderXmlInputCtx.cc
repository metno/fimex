/*
 * Fimex
 *
 * (C) Copyright 2024-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "fimex/ChunkReaderXmlInputCtx.h"

namespace MetNoFimex {

ChunkReaderXmlInputCtx::ChunkReaderXmlInputCtx(ChunkReader_p cr)
    : cr_(cr)
    , offset_(0)
{
}

int ChunkReaderXmlInputCtx::read(char* buffer, int len)
{
    if (!cr_ || len < 0) {
        return -1;
    }
    if (offset_ + len >= cr_->size()) {
        len = cr_->size() - offset_;
    }
    if (len == 0) {
        return 0;
    }
    try {
        cr_->read(offset_, len, (unsigned char*)buffer);
        offset_ += len;
        return len;
    } catch (...) {
        return -1;
    }
}

int readChunkReaderXmlInputCtx(void* ctx, char* buffer, int len)
{
    auto crctx = (ChunkReaderXmlInputCtx*)ctx;
    return crctx->read(buffer, len);
}

int closeChunkReaderXmlInputCtx(void* ctx)
{
    return 0;
}

} // namespace MetNoFimex
