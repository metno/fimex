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

#ifndef FIMEX_NC3HEADER_H_
#define FIMEX_NC3HEADER_H_

#include <cstdint>
#include <limits>
#include <string>
#include <vector>

namespace MetNoFimex {

/// Chunk-layout description for one variable extracted from a NetCDF 3 header.
struct Nc3VarLayout
{
    std::string name;
    int nc_type;             ///< NC_BYTE=1 … NC_DOUBLE=6; NC_UBYTE=7 … NC_UINT64=11 (CDF-5)
    std::vector<int> dimids; ///< dimension indices in NC3 order (slowest = dim 0)
    uint64_t begin;          ///< absolute byte offset of the first data byte
    uint64_t vsize;          ///< bytes per record (record vars) or total (fixed vars),
                             ///< rounded up to a 4-byte boundary
};

/**
 * Metadata extracted from a NetCDF 3 (CDF-1, CDF-2, CDF-5) binary file header.
 *
 * Only the fields needed to reconstruct the on-disk chunk layout are populated;
 * CDM metadata (attributes, types) is obtained from the libnetcdf-based
 * NetCDF_CDMReader and is not duplicated here.
 */
struct Nc3Header
{
    struct DimInfo
    {
        std::string name;
        uint64_t length;
    };

    int version;      ///< 1 (CDF-1), 2 (CDF-2), or 5 (CDF-5)
    uint64_t numrecs; ///< current number of records; UINT64_MAX = streaming

    int unlim_dimid; ///< index in @p dims of the unlimited dimension, -1 if none

    std::vector<DimInfo> dims;
    std::vector<Nc3VarLayout> vars;

    /// Sum of all record-variable vsizes.  This is the stride (in bytes)
    /// between consecutive records of any given record variable.
    uint64_t total_rec_size;

    /// Return true if @p path begins with a recognised CDF magic number.
    static bool isNc3File(const std::string& path);

    /// Parse the binary header of a NetCDF 3 file.
    /// @throws std::runtime_error on I/O errors or format violations.
    static Nc3Header parse(const std::string& path);
};

} // namespace MetNoFimex

#endif /* FIMEX_NC3HEADER_H_ */
