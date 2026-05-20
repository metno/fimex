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

/**
 * NetCDF 3 (CDF-1, CDF-2, CDF-5) binary header parser.
 *
 * Format reference:
 *   https://docs.unidata.ucar.edu/nug/current/file_format_specifications.html
 *
 * The file layout is (all integer fields are big-endian):
 *
 *   magic[4]          "CDF\x01" | "CDF\x02" | "CDF\x05"
 *   numrecs           int32 (CDF-1/2) | int64 (CDF-5); 0xFFFF… = streaming
 *   dim_list          tag[4] + count + dim records
 *   att_list          tag[4] + count + attribute records  (global, skipped)
 *   var_list          tag[4] + count + variable records
 *
 * Integer widths by version:
 *   Field              CDF-1   CDF-2   CDF-5
 *   ─────────────────────────────────────────
 *   numrecs            4       4       8
 *   count (lists)      4       4       8
 *   dim_length         4       4       8
 *   name length        4       4       8
 *   ndims / dimid      4       4       8
 *   att nelems         4       4       8
 *   nc_type            4       4       4  (unchanged)
 *   vsize              4       4       8
 *   begin              4       8       8
 */

#include "Nc3Header.h"

#include <cstdint>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace MetNoFimex {

namespace {

// Sanity caps for header-list sizes.  These are well above any real-world
// NetCDF-3 file while still preventing attacker-controlled OOM allocation.
// The NetCDF classic format documents a maximum of 1024 dimensions and 512
// variables; we allow 64 K of each to be generous to unusual files.
constexpr uint64_t MAX_NAME_LEN = 65536;
constexpr uint64_t MAX_NC3_DIMS = 65536;
constexpr uint64_t MAX_NC3_VARS = 65536;
constexpr uint64_t MAX_NC3_ATTS = 65536;
// Cap per-attribute element count so that nelems * sizeof(largest_type)
// cannot overflow uint64_t or std::streamoff (int64_t).
// (1<<32) * 8 = 32 GiB — safe for both types, unreachable in any real file.
constexpr uint64_t MAX_ATT_NELEMS = uint64_t(1) << 32;

// Element sizes for NC type codes 0..11.
// Index 0 is invalid; CDF-5 adds types 7-11.
constexpr int NC_TYPE_SIZES[12] = {
    0, // 0: invalid
    1, // 1: NC_BYTE
    1, // 2: NC_CHAR
    2, // 3: NC_SHORT
    4, // 4: NC_INT
    4, // 5: NC_FLOAT
    8, // 6: NC_DOUBLE
    1, // 7: NC_UBYTE  (CDF-5)
    2, // 8: NC_USHORT (CDF-5)
    4, // 9: NC_UINT   (CDF-5)
    8, // 10: NC_INT64  (CDF-5)
    8, // 11: NC_UINT64 (CDF-5)
};

/**
 * Streaming reader for big-endian NetCDF 3 binary data.
 *
 * All "size/count" fields change width depending on the file version:
 *   - readSZ()    reads 4 bytes (CDF-1/2) or 8 bytes (CDF-5)
 *   - readBegin() reads 4 bytes (CDF-1) or 8 bytes (CDF-2/5)
 *   - readVsize() reads 4 bytes (CDF-1/2) or 8 bytes (CDF-5)
 *
 * The "tag" fields (ABSENT/NC_DIMENSION/NC_ATTRIBUTE/NC_VARIABLE) and nc_type
 * are always 4 bytes regardless of version.
 */
class Nc3Stream
{
    std::ifstream f_;
    int version_ = 1;

public:
    explicit Nc3Stream(const std::string& path)
        : f_(path, std::ios::binary)
    {
        if (!f_)
            throw std::runtime_error("Nc3Header: cannot open: " + path);
    }

    void setVersion(int v) { version_ = v; }

    bool good() const { return f_.good(); }

    // ---- primitive reads -------------------------------------------------------

    uint32_t readU32()
    {
        uint8_t b[4];
        f_.read(reinterpret_cast<char*>(b), 4);
        return (uint32_t(b[0]) << 24) | (uint32_t(b[1]) << 16) | (uint32_t(b[2]) << 8) | uint32_t(b[3]);
    }

    uint64_t readU64()
    {
        const uint64_t hi = readU32();
        const uint64_t lo = readU32();
        return (hi << 32) | lo;
    }

    /// General integer count/size: 4 bytes (CDF-1/2) or 8 bytes (CDF-5).
    uint64_t readSZ() { return version_ == 5 ? readU64() : readU32(); }

    /// Variable begin offset: 4 bytes (CDF-1) or 8 bytes (CDF-2/5).
    uint64_t readBegin() { return version_ == 1 ? readU32() : readU64(); }

    /// Variable vsize: 4 bytes (CDF-1/2) or 8 bytes (CDF-5).
    uint64_t readVsize() { return version_ == 5 ? readU64() : readU32(); }

    // ---- composite reads -------------------------------------------------------

    /// Read a length-prefixed, 4-byte-padded name string.
    std::string readName()
    {
        const uint64_t len = readSZ();
        if (len > MAX_NAME_LEN)
            throw std::runtime_error("Nc3Header: name length " + std::to_string(len) + " exceeds limit");
        std::string s(static_cast<size_t>(len), '\0');
        if (len > 0)
            f_.read(s.data(), static_cast<std::streamsize>(len));
        const uint64_t pad = (4 - (len % 4)) % 4;
        if (pad > 0)
            f_.seekg(static_cast<std::streamoff>(pad), std::ios::cur);
        return s;
    }

    /// Read and discard one attribute entry.
    void skipAtt()
    {
        readName();                         // attribute name
        const uint32_t nc_type = readU32(); // type code (always 4 bytes)
        const uint64_t nelems = readSZ();   // element count
        if (nelems > MAX_ATT_NELEMS)
            throw std::runtime_error("Nc3Header: attribute element count " + std::to_string(nelems) + " exceeds limit");
        const int tsz = (nc_type < 12) ? NC_TYPE_SIZES[nc_type] : 1;
        // nelems <= MAX_ATT_NELEMS (2^32), tsz <= 8  →  bytes <= 2^35: no overflow.
        const uint64_t bytes = nelems * static_cast<uint64_t>(tsz);
        const uint64_t pad = (4 - (bytes % 4)) % 4;
        f_.seekg(static_cast<std::streamoff>(bytes + pad), std::ios::cur);
    }

    /// Read and discard an att_list block (ABSENT or NC_ATTRIBUTE + entries).
    void skipAttList()
    {
        const uint32_t tag = readU32();
        if (tag == 0) {
            // ABSENT = two 4-byte zeros; the first (tag) is already consumed.
            readU32();
            return;
        }
        // tag == 0x0000000C (NC_ATTRIBUTE)
        const uint64_t count = readSZ();
        if (count > MAX_NC3_ATTS)
            throw std::runtime_error("Nc3Header: attribute count " + std::to_string(count) + " exceeds limit");
        for (uint64_t i = 0; i < count; ++i)
            skipAtt();
    }

    // ---- section readers -------------------------------------------------------

    /// Parse the dim_list section; set @p unlim_dimid to the unlimited dim index
    /// (or leave it at -1 if none).
    void readDimList(std::vector<Nc3Header::DimInfo>& dims, int& unlim_dimid)
    {
        const uint32_t tag = readU32();
        if (tag == 0) {
            readU32();
            return;
        } // ABSENT
        // tag == 0x0000000A (NC_DIMENSION)
        const uint64_t count = readSZ();
        if (count > MAX_NC3_DIMS)
            throw std::runtime_error("Nc3Header: dimension count " + std::to_string(count) + " exceeds limit");
        dims.reserve(static_cast<size_t>(count));
        for (uint64_t i = 0; i < count; ++i) {
            Nc3Header::DimInfo d;
            d.name = readName();
            d.length = readSZ(); // 0 means unlimited
            if (d.length == 0)
                unlim_dimid = static_cast<int>(i);
            dims.push_back(std::move(d));
        }
    }

    /// Parse one variable record.
    /// @p ndims_total is the number of dimensions already parsed; used to
    /// validate that all dimids are in range.
    Nc3VarLayout readVar(size_t ndims_total)
    {
        Nc3VarLayout v;
        v.name = readName();
        const uint64_t nd = readSZ(); // number of dimensions
        if (nd > MAX_NC3_DIMS)
            throw std::runtime_error("Nc3Header: variable '" + v.name + "' has " + std::to_string(nd) + " dimensions, exceeding limit");
        v.dimids.resize(static_cast<size_t>(nd));
        for (uint64_t i = 0; i < nd; ++i) {
            const uint64_t dimid = readSZ();
            if (dimid >= ndims_total)
                throw std::runtime_error("Nc3Header: variable '" + v.name + "' dimid " + std::to_string(dimid) +
                                         " out of range (ndims=" + std::to_string(ndims_total) + ")");
            v.dimids[static_cast<size_t>(i)] = static_cast<int>(dimid);
        }
        skipAttList();
        v.nc_type = static_cast<int>(readU32()); // nc_type always 4 bytes
        v.vsize = readVsize();
        v.begin = readBegin();
        return v;
    }

    /// Parse the var_list section.
    void readVarList(std::vector<Nc3VarLayout>& vars, size_t ndims_total)
    {
        const uint32_t tag = readU32();
        if (tag == 0) {
            readU32();
            return;
        } // ABSENT
        // tag == 0x0000000B (NC_VARIABLE)
        const uint64_t count = readSZ();
        if (count > MAX_NC3_VARS)
            throw std::runtime_error("Nc3Header: variable count " + std::to_string(count) + " exceeds limit");
        vars.reserve(static_cast<size_t>(count));
        for (uint64_t i = 0; i < count; ++i)
            vars.push_back(readVar(ndims_total));
    }
};

} // anonymous namespace

// ---------------------------------------------------------------------------

bool Nc3Header::isNc3File(const std::string& path)
{
    std::ifstream f(path, std::ios::binary);
    char magic[4] = {};
    f.read(magic, 4);
    return f.good() && magic[0] == 'C' && magic[1] == 'D' && magic[2] == 'F' && (magic[3] == '\x01' || magic[3] == '\x02' || magic[3] == '\x05');
}

Nc3Header Nc3Header::parse(const std::string& path)
{
    Nc3Stream s(path);

    Nc3Header h;
    h.unlim_dimid = -1;
    h.total_rec_size = 0;

    // ---- magic / version -------------------------------------------------------
    const uint32_t magic = s.readU32();
    const uint32_t prefix = magic >> 8;
    const int ver = static_cast<int>(magic & 0xFFu);

    // 'C'=0x43, 'D'=0x44, 'F'=0x46 → prefix 0x434446
    static constexpr uint32_t CDF_PREFIX = (uint32_t('C') << 16) | (uint32_t('D') << 8) | uint32_t('F');

    if (prefix != CDF_PREFIX || (ver != 1 && ver != 2 && ver != 5))
        throw std::runtime_error("not a NetCDF-3 file: " + path);

    h.version = ver;
    s.setVersion(ver);

    // ---- numrecs ---------------------------------------------------------------
    if (ver == 5) {
        const uint64_t nr = s.readU64();
        h.numrecs = (nr == std::numeric_limits<uint64_t>::max()) ? std::numeric_limits<uint64_t>::max() : nr;
    } else {
        const uint32_t nr = s.readU32();
        h.numrecs = (nr == std::numeric_limits<uint32_t>::max()) ? std::numeric_limits<uint64_t>::max() : static_cast<uint64_t>(nr);
    }

    // ---- dim_list, att_list, var_list ------------------------------------------
    s.readDimList(h.dims, h.unlim_dimid);
    s.skipAttList(); // ignore global attributes, retrieved from CDM
    s.readVarList(h.vars, h.dims.size());

    if (!s.good())
        throw std::runtime_error("I/O error parsing NetCDF-3 header: " + path);

    // ---- fill in unlimited dim length ------------------------------------------
    // The binary header stores 0 for the unlimited dimension size; the actual
    // current record count is in numrecs.
    if (h.unlim_dimid >= 0 && h.numrecs != std::numeric_limits<uint64_t>::max())
        h.dims[static_cast<size_t>(h.unlim_dimid)].length = h.numrecs;

    // ---- total record size (stride between records for any record variable) ----
    for (const auto& v : h.vars) {
        if (!v.dimids.empty() && h.unlim_dimid >= 0 && v.dimids[0] == h.unlim_dimid)
            h.total_rec_size += v.vsize;
    }

    return h;
}

} // namespace MetNoFimex
