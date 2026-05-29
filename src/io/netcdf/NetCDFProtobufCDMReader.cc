/*
 * (C) Copyright 2026, met.no
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

#include "NetCDFProtobufCDMReader.h"

#include "NetCDFProtobufIndexReader.h"

#include "fimex/CDM.h"
#include "fimex/CDMDimension.h"
#include "fimex/CDMException.h"
#include "fimex/CDMVariable.h"
#include "fimex/ChunkReaderFactory.h"
#include "fimex/Data.h"
#include "fimex/FileUtils.h"
#include "fimex/Logger.h"
#include "fimex/ProtobufUtils.h"
#include "fimex/SharedArray.h"
#include "fimex/SliceBuilder.h"

#include <algorithm>
#include <bit>
#include <cstring>
#include <mutex>
#include <numeric>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "fimex_netcdf_config.h"

#include <zlib.h>
#ifdef HAVE_LIBAEC
#include <libaec.h>
#endif

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.NetCDFProtobufCDMReader");

// ---- element-size lookup by CDMDataType -------------------------------------

size_t cdmElemSize(CDMDataType dt)
{
    // clang-format off
    switch (dt) {
    case CDM_CHAR:   return sizeof(char);
    case CDM_UCHAR:  return sizeof(unsigned char);
    case CDM_SHORT:  return sizeof(short);
    case CDM_USHORT: return sizeof(unsigned short);
    case CDM_INT:    return sizeof(int);
    case CDM_UINT:   return sizeof(unsigned int);
    case CDM_INT64:  return sizeof(long long);
    case CDM_UINT64: return sizeof(unsigned long long);
    case CDM_FLOAT:  return sizeof(float);
    case CDM_DOUBLE: return sizeof(double);
    default:         return 0; // NAT, STRING, STRINGS
    }
    // clang-format on
}

// ---- HDF5 shuffle-filter un-shuffle -----------------------------------------

/// Reverses the HDF5 shuffle filter in-place.
/// The shuffle filter groups all byte-planes together:
///   plane 0 = all byte-0 of every element, plane 1 = all byte-1, …
/// Un-shuffling restores interleaved (element-major) order.
void unshuffle(uint8_t* data, size_t nbytes, size_t elem_size)
{
    if (elem_size <= 1)
        return;
    const size_t n_elems = nbytes / elem_size;
    std::vector<uint8_t> tmp(nbytes);
    for (size_t b = 0; b < elem_size; b++) {
        for (size_t i = 0; i < n_elems; i++) {
            tmp[i * elem_size + b] = data[b * n_elems + i];
        }
    }
    std::memcpy(data, tmp.data(), nbytes);
}

// ---- byte-order swap --------------------------------------------------------

void byteswap(uint8_t* data, size_t n_elems, size_t elem_size)
{
    for (size_t i = 0; i < n_elems; i++) {
        uint8_t* e = data + i * elem_size;
        for (size_t j = 0; j < elem_size / 2; j++)
            std::swap(e[j], e[elem_size - 1 - j]);
    }
}

// ---- SZIP/AEC decompression (mirrors SZ_BufftoBuffDecompress from sz_compat.c) -

#ifdef HAVE_LIBAEC

/// Decode a raw AEC bitstream into @p out, resized to @p aec_buf_size bytes.
static void decodeAecBuffer(const uint8_t* src, size_t src_len, int aec_bps, int ppb, int rsi, int flags, size_t aec_buf_size, std::vector<uint8_t>& out)
{
    out.resize(aec_buf_size);
    struct aec_stream strm = {};
    strm.bits_per_sample = aec_bps;
    strm.block_size = ppb;
    strm.rsi = rsi;
    strm.flags = flags;
    strm.next_in = src;
    strm.avail_in = src_len;
    strm.next_out = out.data();
    strm.avail_out = aec_buf_size;
    const int ret = aec_buffer_decode(&strm);
    if (ret != AEC_OK)
        throw std::runtime_error("libaec aec_buffer_decode failed: " + std::to_string(ret));
}

/// Remove the padding pixels that AEC inserts when pps % ppb != 0.
/// Each padded scanline is rsi*ppb pixels wide; real data is only pps wide.
static void removeScanlinePadding(std::vector<uint8_t>& data, int pps, int ppb, int rsi, size_t n_aec_pixels)
{
    const size_t line_size = static_cast<size_t>(pps);
    const size_t padded_line = static_cast<size_t>(rsi) * static_cast<size_t>(ppb);
    const size_t buf_size = data.size();
    size_t dst_off = line_size;
    for (size_t src_off = padded_line; src_off < buf_size && dst_off + line_size <= n_aec_pixels; src_off += padded_line, dst_off += line_size) {
        std::memmove(data.data() + dst_off, data.data() + src_off, line_size);
    }
    data.resize(n_aec_pixels);
}

/// Restore element-major byte order from AEC's byte-plane layout (wide types).
/// AEC encodes byte-plane 0 of all elements, then byte-plane 1, etc.
/// Writes the deinterleaved result into @p result (resized to n_elems * elem_bytes).
static void deinterleaveWideSamples(const std::vector<uint8_t>& planes, size_t n_elems, int elem_bytes, std::vector<uint8_t>& result)
{
    result.resize(n_elems * static_cast<size_t>(elem_bytes));
    for (size_t i = 0; i < n_elems; i++)
        for (int b = 0; b < elem_bytes; b++)
            result[i * static_cast<size_t>(elem_bytes) + static_cast<size_t>(b)] = planes[static_cast<size_t>(b) * n_elems + i];
}

/// Decompress an SZIP/AEC chunk, mirroring SZ_BufftoBuffDecompress from
/// libaec's sz_compat.c (RSI ceiling division, wide-type deinterleave, padding).
///
/// @param dst  Receives the final decoded bytes (resized to dest_len).
/// @param tmp  Scratch buffer for the AEC decode step; may alias neither src nor dst.
static void szDecompress(const uint8_t* src, size_t src_len, size_t dest_len, int bits_per_pixel, int pixels_per_block, int pixels_per_scanline,
                         bool nn_coding, std::vector<uint8_t>& dst, std::vector<uint8_t>& tmp)
{
    // For 32/64-bit types AEC operates on individual bytes (bits_per_sample=8).
    const bool wide = (bits_per_pixel == 32 || bits_per_pixel == 64);
    const int aec_bps = wide ? 8 : bits_per_pixel;
    const int elem_bytes = bits_per_pixel / 8;
    // RSI = ceil(pps / ppb) — sz_compat.c uses ceiling division.
    const int rsi = (pixels_per_scanline + pixels_per_block - 1) / pixels_per_block;

    int flags = AEC_DATA_MSB;
    if (nn_coding)
        flags |= AEC_DATA_PREPROCESS;

    // For wide types the byte planes span the whole dest_len stream, so
    // n_aec_pixels == dest_len.
    const size_t n_aec_pixels = wide ? dest_len : dest_len / static_cast<size_t>(elem_bytes);
    const size_t scanlines = (n_aec_pixels + static_cast<size_t>(pixels_per_scanline) - 1) / static_cast<size_t>(pixels_per_scanline);
    const size_t aec_buf_size = static_cast<size_t>(rsi) * static_cast<size_t>(pixels_per_block) * scanlines;

    // Decode AEC bitstream into tmp (may be larger than dest_len due to padding).
    decodeAecBuffer(src, src_len, aec_bps, pixels_per_block, rsi, flags, aec_buf_size, tmp);

    if ((pixels_per_scanline % pixels_per_block) != 0)
        removeScanlinePadding(tmp, pixels_per_scanline, pixels_per_block, rsi, n_aec_pixels);

    if (wide) {
        // Deinterleave from tmp (byte-plane layout) into dst (element-major).
        const size_t n_elems = dest_len / static_cast<size_t>(elem_bytes);
        deinterleaveWideSamples(tmp, n_elems, elem_bytes, dst);
    } else {
        tmp.resize(dest_len);
        std::swap(dst, tmp);
    }
}

/// Decompress an SZIP chunk including the 4-byte HDF5 size header.
/// When HDF5 cannot compress a chunk (stored size == decoded size), the chunk
/// is stored verbatim with no header — detect and pass through.
///
/// @param compressed  The raw chunk bytes (taken by value so src pointer into it
///                    remains stable while dst/tmp are resized).
/// @param dst         Receives the decoded bytes (resized to decoded_size).
/// @param tmp         Scratch buffer for AEC output; may alias neither compressed
///                    contents nor dst.
static void applyHdf5Szip(std::vector<uint8_t> compressed, size_t decoded_size, const NetCDFFilter& filter,
                           std::vector<uint8_t>& dst, std::vector<uint8_t>& tmp)
{
    if (filter.szip_pixels_per_block == 0)
        throw std::runtime_error("SZIP pixels_per_block is zero — corrupt index");
    // Passthrough: stored size equals decoded size means HDF5 gave up compressing.
    if (compressed.size() == decoded_size) {
        std::swap(dst, compressed);
        return;
    }
    if (compressed.size() < 4)
        throw std::runtime_error("SZIP chunk too small to contain HDF5 header");
    // HDF5 prepends a 4-byte little-endian uint32 (uncompressed size) to every
    // SZIP chunk (H5Zszip.c H5Z__filter_szip / UINT32ENCODE).
    const uint32_t hdr_size =
        (static_cast<uint32_t>(compressed[0])) | (static_cast<uint32_t>(compressed[1]) << 8) | (static_cast<uint32_t>(compressed[2]) << 16) | (static_cast<uint32_t>(compressed[3]) << 24);
    if (hdr_size != static_cast<uint32_t>(decoded_size))
        throw std::runtime_error("SZIP header size " + std::to_string(hdr_size) + " != expected " + std::to_string(decoded_size));
    szDecompress(compressed.data() + 4, compressed.size() - 4, decoded_size, static_cast<int>(filter.szip_bits_per_pixel),
                 static_cast<int>(filter.szip_pixels_per_block), static_cast<int>(filter.szip_pixels_per_scanline), filter.szip_nn_coding, dst, tmp);
}
#endif // HAVE_LIBAEC

/// Deflate-decompress @p src into @p dst, which is resized to @p decoded_size.
static void applyDeflate(const std::vector<uint8_t>& src, size_t decoded_size, std::vector<uint8_t>& dst)
{
    dst.resize(decoded_size);
    uLongf dest_len = static_cast<uLongf>(decoded_size);
    const int ret = ::uncompress(dst.data(), &dest_len, src.data(), static_cast<uLong>(src.size()));
    if (ret != Z_OK)
        throw std::runtime_error("zlib uncompress failed: " + std::to_string(ret));
    if (dest_len != static_cast<uLongf>(decoded_size))
        throw std::runtime_error("decompressed chunk size mismatch");
}

/// Validate decompressed size, then apply shuffle (if present) and byte-swap.
/// Must be called after all decompression: HDF5 spec requires unshuffle after
/// decompression, not interleaved with it.
static void applyPostProcessing(std::vector<uint8_t>& buf, const NetCDFVarChunks& vc, size_t decoded_size, size_t elem_size, size_t chunk_nelems)
{
    if (buf.size() != decoded_size)
        throw std::runtime_error("chunk raw size " + std::to_string(buf.size()) + " != expected " + std::to_string(decoded_size));
    if (elem_size <= 1)
        return;
    const bool has_shuffle = std::any_of(vc.filters.begin(), vc.filters.end(), [](const NetCDFFilter& f) { return f.kind == NetCDFFilter::Kind::Shuffle; });
    if (has_shuffle)
        unshuffle(buf.data(), decoded_size, elem_size);
    if (vc.big_endian != (std::endian::native == std::endian::big))
        byteswap(buf.data(), chunk_nelems, elem_size);
}

/// Read a single chunk from the source file, decompress (if needed),
/// un-shuffle (if needed), and byte-swap (if needed).
///
/// Filters are applied in reverse (decompression) order as required by HDF5.
/// Shuffle is always applied after decompression (applyPostProcessing).
///
/// @param buf  Scratch buffer.  On entry the content is ignored; on return it
///             holds the fully decoded chunk bytes (size = chunk_nelems * elem_size).
///             Reusing the same buffer across calls avoids repeated allocation.
/// @param tmp  Scratch buffer used as decompressor output; content is undefined
///             on return.  Must be a different vector from @p buf.
void decodeChunk(ChunkReader& cr, const NetCDFChunkInfo& ci, const NetCDFVarChunks& vc, size_t elem_size, size_t chunk_nelems,
                 std::vector<uint8_t>& buf, std::vector<uint8_t>& tmp)
{
    const size_t decoded_size = chunk_nelems * elem_size;
    buf.resize(ci.size);
    cr.read(static_cast<size_t>(ci.addr), ci.size, buf.data());

    for (auto it = vc.filters.rbegin(); it != vc.filters.rend(); ++it) {
        switch (it->kind) {
        case NetCDFFilter::Kind::Shuffle:
            break; // applied after decompression by applyPostProcessing()
        case NetCDFFilter::Kind::Deflate:
            applyDeflate(buf, decoded_size, tmp);
            std::swap(buf, tmp);
            break;
        case NetCDFFilter::Kind::Szip:
#ifdef HAVE_LIBAEC
            // Pass buf by value (moves it); buf is then used as the AEC scratch
            // inside applyHdf5Szip, while tmp receives the decoded result.
            applyHdf5Szip(std::move(buf), decoded_size, *it, tmp, buf);
            std::swap(buf, tmp);
#else
            throw std::runtime_error("SZIP-compressed chunk encountered but fimex was built without libaec");
#endif
            break;
        case NetCDFFilter::Kind::Generic:
            throw std::runtime_error("unsupported HDF5 filter id " + std::to_string(it->generic_filter_id) + " in chunk pipeline");
        }
    }

    applyPostProcessing(buf, vc, decoded_size, elem_size, chunk_nelems);
}

// ---- N-dimensional intersection copy ----------------------------------------

/// Copy the intersection of a decoded chunk with the requested slice into the
/// output buffer.
///
/// All index arrays are in CDM dimension order: dimension 0 is the fastest-
/// varying axis (innermost loop / contiguous in memory).
///
/// @param chunk_data   Decoded chunk bytes (chunk_nelems * elem_size).
/// @param chunk_offset Origin of the chunk in the global dataspace (CDM order).
/// @param chunk_stride Flat-index stride for each CDM dimension in the chunk
///                     buffer (dim 0 = 1, dim d = prod(chunk_shape[0..d-1])).
/// @param out_data     Pointer to the output buffer.
/// @param out_start    Global start of the requested slice (= sb start, CDM order).
/// @param out_stride   Flat-index stride for each CDM dimension in the output
///                     buffer (dim 0 = 1, dim d = prod(out_size[0..d-1])).
/// @param inter_start  Global start of the intersection region (CDM order).
/// @param inter_size   Size of the intersection in each dimension (CDM order).
/// @param elem_size    Bytes per element.
/// @param ndims        Number of dimensions.
void copyIntersection(const uint8_t* chunk_data, const std::vector<uint64_t>& chunk_offset, const std::vector<size_t>& chunk_stride, uint8_t* out_data,
                      const std::vector<size_t>& out_start, const std::vector<size_t>& out_stride, const std::vector<size_t>& inter_start,
                      const std::vector<size_t>& inter_size, size_t elem_size, int ndims)
{
    // Scalar variable: no dimensional intersection — copy the single element.
    if (ndims == 0) {
        std::memcpy(out_data, chunk_data, elem_size);
        return;
    }

    // Bundle the invariant context so the recursive helper avoids a long
    // parameter list without incurring std::function type-erasure overhead.
    struct Ctx {
        const std::vector<uint64_t>& chunk_offset;
        const std::vector<size_t>& chunk_stride;
        const std::vector<size_t>& out_start;
        const std::vector<size_t>& out_stride;
        const std::vector<size_t>& inter_start;
        const std::vector<size_t>& inter_size;
        size_t elem_size;

        // Processes one dimension per call, outermost first
        // (cur_dim = ndims-1 = slowest), ending with cur_dim = 0 (fastest).
        // At cur_dim == 0 we do a single contiguous memcpy.
        void recurse(const uint8_t* src_base, uint8_t* dst_base, int cur_dim) const
        {
            if (cur_dim == 0) {
                const size_t g0 = inter_start[0];
                const uint8_t* src = src_base + (g0 - static_cast<size_t>(chunk_offset[0])) * elem_size;
                uint8_t* dst = dst_base + (g0 - out_start[0]) * elem_size;
                std::memcpy(dst, src, inter_size[0] * elem_size);
            } else {
                for (size_t i = 0; i < inter_size[cur_dim]; i++) {
                    const size_t g = inter_start[cur_dim] + i;
                    const uint8_t* src = src_base + (g - static_cast<size_t>(chunk_offset[cur_dim])) * chunk_stride[cur_dim] * elem_size;
                    uint8_t* dst = dst_base + (g - out_start[cur_dim]) * out_stride[cur_dim] * elem_size;
                    recurse(src, dst, cur_dim - 1);
                }
            }
        }
    };

    Ctx{chunk_offset, chunk_stride, out_start, out_stride, inter_start, inter_size, elem_size}.recurse(chunk_data, out_data, ndims - 1);
}

// ---- getDataSlice helpers ---------------------------------------------------

/// Validate element size and confirm that the indexed chunk rank matches the CDM rank.
static size_t validatedElemSize(const std::string& varName, CDMDataType dt, const NetCDFVarChunks& vc, int ndims)
{
    const size_t elem_size = cdmElemSize(dt);
    if (elem_size == 0)
        throw CDMException("unsupported data type for variable '" + varName + "'");
    if (static_cast<int>(vc.chunk_shape.size()) != ndims)
        throw CDMException("chunk shape rank " + std::to_string(vc.chunk_shape.size()) + " != CDM rank " + std::to_string(ndims) + " for variable '" + varName +
                           "'");
    return elem_size;
}

/// Compute output element count with overflow checking.
static size_t computeOutputNelems(const std::string& varName, const std::vector<size_t>& sb_size, size_t elem_size)
{
    size_t nelems = 1;
    for (size_t s : sb_size) {
        if (s > 0 && nelems > std::numeric_limits<size_t>::max() / s)
            throw CDMException("output element count overflows for variable '" + varName + "'");
        nelems *= s;
    }
    if (nelems > std::numeric_limits<size_t>::max() / elem_size)
        throw CDMException("output buffer size overflows for variable '" + varName + "'");
    return nelems;
}

/// Fill a raw byte buffer with @p fill_val truncated to @p elem_size bytes.
/// Mirrors the byte-truncation semantics of the old makeFillBuffer.
static void fillBuffer(uint8_t* buf, size_t out_nelems, size_t elem_size, double fill_val)
{
    const uint64_t fv_bits = std::bit_cast<uint64_t>(fill_val);
    uint8_t fill_bytes[8] = {};
    if constexpr (std::endian::native == std::endian::big) {
        for (size_t b = 0; b < elem_size; b++)
            fill_bytes[b] = static_cast<uint8_t>(fv_bits >> ((7 - b) * 8));
    } else {
        for (size_t b = 0; b < elem_size; b++)
            fill_bytes[b] = static_cast<uint8_t>(fv_bits >> (b * 8));
    }
    for (size_t i = 0; i < out_nelems; i++)
        std::memcpy(buf + i * elem_size, fill_bytes, elem_size);
}

/// Allocate a typed shared array of @p out_nelems elements, fill it with
/// the fill-value byte pattern, and return both the DataPtr and a raw byte
/// pointer into the same allocation.  The caller writes chunk data through
/// the raw pointer and then returns the DataPtr directly — no final memcpy.
template <typename T>
static std::pair<DataPtr, uint8_t*> makeOutputBufferT(size_t out_nelems, double fill_val)
{
    auto arr = make_shared_array<T>(out_nelems);
    uint8_t* raw = reinterpret_cast<uint8_t*>(arr.get());
    fillBuffer(raw, out_nelems, sizeof(T), fill_val);
    return {createData(out_nelems, arr), raw};
}

static std::pair<DataPtr, uint8_t*> makeOutputBuffer(CDMDataType dt, size_t out_nelems, double fill_val)
{
    // clang-format off
    switch (dt) {
    case CDM_CHAR:   return makeOutputBufferT<char>(out_nelems, fill_val);
    case CDM_UCHAR:  return makeOutputBufferT<unsigned char>(out_nelems, fill_val);
    case CDM_SHORT:  return makeOutputBufferT<short>(out_nelems, fill_val);
    case CDM_USHORT: return makeOutputBufferT<unsigned short>(out_nelems, fill_val);
    case CDM_INT:    return makeOutputBufferT<int>(out_nelems, fill_val);
    case CDM_UINT:   return makeOutputBufferT<unsigned int>(out_nelems, fill_val);
    case CDM_INT64:  return makeOutputBufferT<long long>(out_nelems, fill_val);
    case CDM_UINT64: return makeOutputBufferT<unsigned long long>(out_nelems, fill_val);
    case CDM_FLOAT:  return makeOutputBufferT<float>(out_nelems, fill_val);
    case CDM_DOUBLE: return makeOutputBufferT<double>(out_nelems, fill_val);
    default:         return {createData(CDM_INT, 0), nullptr};
    }
    // clang-format on
}

/// Compute flat-index strides (in elements) for the given dimension sizes.
/// Dimension 0 is fastest-varying (stride = 1); dim d has stride = product of sizes[0..d-1].
static std::vector<size_t> computeStrides(int ndims, const std::vector<size_t>& sizes)
{
    std::vector<size_t> strides(ndims, 1);
    for (int d = 1; d < ndims; d++)
        strides[d] = strides[d - 1] * sizes[d - 1];
    return strides;
}

/// Compute chunk element count and per-dimension strides with overflow checking.
static std::pair<size_t, std::vector<size_t>> computeChunkLayout(const std::string& varName, const NetCDFVarChunks& vc, int ndims)
{
    size_t chunk_nelems = 1;
    std::vector<size_t> shape(ndims);
    for (int d = 0; d < ndims; d++) {
        const size_t cs = static_cast<size_t>(vc.chunk_shape[d]);
        if (cs > 0 && chunk_nelems > std::numeric_limits<size_t>::max() / cs)
            throw CDMException("chunk element count overflows for variable '" + varName + "'");
        chunk_nelems *= cs;
        shape[d] = cs;
    }
    return {chunk_nelems, computeStrides(ndims, shape)};
}

/// Binary-search for the first chunk that could overlap the requested slice.
/// Requires: chunks are sorted by (offset[ndims-1], ..., offset[0]) — maintained at index load.
static std::vector<NetCDFChunkInfo>::const_iterator findFirstRelevantChunk(const std::vector<NetCDFChunkInfo>& chunks, const std::vector<size_t>& sb_start,
                                                                           const std::vector<uint64_t>& chunk_shape, int ndims)
{
    if (ndims == 0)
        return chunks.begin();
    const size_t chunk_sz = static_cast<size_t>(chunk_shape[ndims - 1]);
    const size_t sl_start = sb_start[ndims - 1];
    const uint64_t first_off = sl_start >= chunk_sz ? static_cast<uint64_t>(sl_start - chunk_sz + 1) : 0;
    return std::lower_bound(chunks.begin(), chunks.end(), first_off, [ndims](const NetCDFChunkInfo& c, uint64_t v) { return c.offset[ndims - 1] < v; });
}

/// Iterate over all chunks that overlap the requested slice and copy each
/// intersection into @p out_buf.  @p getReader returns an open ChunkReader
/// for a given file index (1-based; 0 is the missing-data sentinel).
///
/// When the variable's chunk_grid is populated (dense layout), chunks are
/// found in O(intersecting) time by iterating only the grid cells that
/// overlap the requested slice.  For sparse or misaligned layouts the
/// chunk_grid is empty and the original sorted-vector approach is used.
template <typename ReaderGetter>
static void copyChunksIntoSlice(const NetCDFVarChunks& vc, uint8_t* out_buf, const std::vector<size_t>& sb_start,
                                const std::vector<size_t>& sb_size, const std::vector<size_t>& out_stride, int ndims, size_t elem_size, size_t chunk_nelems,
                                const std::vector<size_t>& chunk_stride, ReaderGetter getReader)
{
    std::vector<size_t> inter_start(ndims), inter_size(ndims);
    std::vector<uint8_t> chunk_buf, chunk_tmp;

    if (!vc.chunk_grid.empty()) {
        // ---- Grid path: O(intersecting) ----------------------------------------
        // Compute the range of grid cells [g_start, g_end) that overlap the
        // requested slice in each dimension.
        std::vector<size_t> g_start(ndims), g_end(ndims);
        for (int d = 0; d < ndims; d++) {
            const size_t cs = static_cast<size_t>(vc.chunk_shape[d]);
            if (sb_size[d] == 0)
                return;
            g_start[d] = sb_start[d] / cs;
            g_end[d] = std::min((sb_start[d] + sb_size[d] + cs - 1) / cs, vc.grid_dims[d]);
            if (g_start[d] >= g_end[d])
                return;
        }

        // Flat strides for the grid (dim 0 fastest, matching chunk_grid layout).
        std::vector<size_t> g_stride(ndims, 1);
        for (int d = 1; d < ndims; d++)
            g_stride[d] = g_stride[d - 1] * vc.grid_dims[d - 1];

        // Grid counter initialised to the starting corner; flat is its flat index.
        std::vector<size_t> g(g_start);
        size_t flat = 0;
        for (int d = 0; d < ndims; d++)
            flat += g_start[d] * g_stride[d];

        // Odometer loop: dimension 0 is innermost (consecutive accesses stride 1).
        while (true) {
            const int32_t ci_idx = vc.chunk_grid[flat];
            if (ci_idx >= 0) {
                const NetCDFChunkInfo& ci = vc.chunks[static_cast<size_t>(ci_idx)];
                bool empty = false;
                for (int d = 0; d < ndims; d++) {
                    const size_t ch_start = static_cast<size_t>(ci.offset[d]);
                    const size_t ch_end = ch_start + static_cast<size_t>(vc.chunk_shape[d]);
                    inter_start[d] = std::max(ch_start, sb_start[d]);
                    const size_t ie = std::min(ch_end, sb_start[d] + sb_size[d]);
                    if (ie <= inter_start[d]) {
                        empty = true;
                        break;
                    }
                    inter_size[d] = ie - inter_start[d];
                }
                if (!empty) {
                    decodeChunk(*getReader(ci.file_index), ci, vc, elem_size, chunk_nelems, chunk_buf, chunk_tmp);
                    copyIntersection(chunk_buf.data(), ci.offset, chunk_stride, out_buf, sb_start, out_stride, inter_start, inter_size, elem_size, ndims);
                }
            }
            // Advance odometer (dim 0 carries first).
            int carry = 0;
            while (carry < ndims) {
                flat += g_stride[carry];
                if (++g[carry] < g_end[carry])
                    break;
                flat -= (g_end[carry] - g_start[carry]) * g_stride[carry];
                g[carry] = g_start[carry];
                ++carry;
            }
            if (carry == ndims)
                break;
        }
        return;
    }

    // ---- Sorted-vector fallback: used for sparse or misaligned layouts ---------
    auto chunk_it = findFirstRelevantChunk(vc.chunks, sb_start, vc.chunk_shape, ndims);
    const auto chunk_end = vc.chunks.end();
    const size_t sl_end_slowest = ndims > 0 ? sb_start[ndims - 1] + sb_size[ndims - 1] : 0;

    for (; chunk_it != chunk_end; ++chunk_it) {
        const NetCDFChunkInfo& ci = *chunk_it;
        if (ndims > 0 && ci.offset[ndims - 1] >= sl_end_slowest)
            break;

        bool empty = false;
        for (int d = 0; d < ndims; d++) {
            const size_t ch_start = static_cast<size_t>(ci.offset[d]);
            const size_t ch_end = ch_start + static_cast<size_t>(vc.chunk_shape[d]);
            inter_start[d] = std::max(ch_start, sb_start[d]);
            const size_t inter_end = std::min(ch_end, sb_start[d] + sb_size[d]);
            if (inter_end <= inter_start[d]) {
                empty = true;
                break;
            }
            inter_size[d] = inter_end - inter_start[d];
        }
        if (empty || ci.file_index == 0)
            continue;

        decodeChunk(*getReader(ci.file_index), ci, vc, elem_size, chunk_nelems, chunk_buf, chunk_tmp);
        copyIntersection(chunk_buf.data(), ci.offset, chunk_stride, out_buf, sb_start, out_stride, inter_start, inter_size, elem_size, ndims);
    }
}

} // anonymous namespace

// ============================================================================
// Impl
// ============================================================================

struct NetCDFProtobufCDMReader::Impl
{
    NetCDFIndexed indexed;
    std::string root_path;
    ChunkReaderFactory_p chunk_factory;

    // File handles cached by file_index (1-based; 0 is the missing-data sentinel).
    // One entry per distinct source file in the index; typically a small number.
    // Access is protected by cr_cache_mutex so that concurrent getDataSlice calls
    // from different threads share handles without re-opening files.
    std::mutex cr_cache_mutex;
    std::unordered_map<uint32_t, ChunkReader_p> cr_cache;

    /// Return (or open) the ChunkReader for source file @p fi.
    /// Thread-safe: the returned shared_ptr keeps the reader alive after the
    /// lock is released, so callers may use it concurrently (FileChunkReader
    /// uses pread and requires no per-read locking).
    ChunkReader_p getReader(uint32_t fi)
    {
        std::lock_guard<std::mutex> lock(cr_cache_mutex);
        auto it = cr_cache.find(fi);
        if (it == cr_cache.end()) {
            const std::string path = joinFilename(root_path, indexed.files[fi]);
            it = cr_cache.emplace(fi, chunk_factory->readerFor(path)).first;
        }
        return it->second;
    }
};

// ============================================================================
// NetCDFProtobufCDMReader
// ============================================================================

NetCDFProtobufCDMReader::NetCDFProtobufCDMReader(const std::string& indexFile, const XMLInput& configXML)
    : NetCDFProtobufCDMReader(indexFile, protobufIndexRootPath(indexFile, configXML))
{
}

NetCDFProtobufCDMReader::NetCDFProtobufCDMReader(const std::string& indexFile, const std::string& rootPath)
    : p_(std::make_unique<Impl>())
{
    p_->chunk_factory = createDefaultChunkReaderFactory();

    ChunkReader_p idx_reader = p_->chunk_factory->readerFor(indexFile);
    readNetCDFProtobufIndex(*idx_reader, indexFile, *cdm_, p_->indexed);

    p_->root_path = rootPath;
}

NetCDFProtobufCDMReader::~NetCDFProtobufCDMReader() = default;

DataPtr NetCDFProtobufCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice for variable '" << varName << "' unLimDimPos=" << unLimDimPos);

    const auto& var = cdm_->getVariable(varName);
    if (var.getDataType() == CDM_NAT)
        return createData(CDM_INT, 0);

    if (auto mem = getDataSliceFromMemory(var, unLimDimPos))
        return mem;

    SliceBuilder sb(*cdm_, varName);
    if (cdm_->hasUnlimitedDim(var)) {
        if (const auto* unlim_dim = cdm_->getUnlimitedDim())
            sb.setStartAndSize(unlim_dim->getName(), unLimDimPos, 1);
    }
    return getDataSlice(varName, sb);
}

DataPtr NetCDFProtobufCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice for variable '" << varName << "'");

    const CDMVariable& var = cdm_->getVariable(varName);
    const CDMDataType dt = var.getDataType();

    if (dt == CDM_NAT)
        return createData(CDM_INT, 0);

    if (DataPtr mem = getDataSliceFromMemory(var, sb))
        return mem;

    const auto it = p_->indexed.vars.find(varName);
    if (it == p_->indexed.vars.end())
        throw CDMException("no chunk layout for variable '" + varName + "' in NetCDF protobuf index");
    const NetCDFVarChunks& vc = it->second;

    const auto& sb_start = sb.getDimensionStartPositions();
    const auto& sb_size = sb.getDimensionSizes();
    const int ndims = static_cast<int>(sb_size.size());

    const size_t elem_size = validatedElemSize(varName, dt, vc, ndims);
    const size_t out_nelems = computeOutputNelems(varName, sb_size, elem_size);

    auto [out_data, out_buf] = makeOutputBuffer(dt, out_nelems, cdm_->getFillValue(varName));
    if (!out_buf)
        return out_data; // CDM_NAT or unsupported type — empty DataPtr already returned above
    auto out_stride = computeStrides(ndims, sb_size);
    const auto [chunk_nelems, chunk_stride] = computeChunkLayout(varName, vc, ndims);

    copyChunksIntoSlice(vc, out_buf, sb_start, sb_size, out_stride, ndims, elem_size, chunk_nelems, chunk_stride,
                        [&](uint32_t fi) { return p_->getReader(fi); });

    return out_data;
}

} // namespace MetNoFimex
