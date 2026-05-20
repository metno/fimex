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

#ifndef FIMEX_NETCDFPROTOBUFINDEXWRITER_H_
#define FIMEX_NETCDFPROTOBUFINDEXWRITER_H_

#include <iosfwd>
#include <string>

namespace MetNoFimex {

/// Four-byte format discriminator appended after FIMEX_PROTO_INDEX_MAGIC.
/// 'N' = NetCDF, three zero bytes reserved for future versioning.
extern const char NETCDF_PROTO_INDEX_MAGIC[4];

/**
 * Build a protobuf index for a NetCDF4/HDF5 file and write it to disk or a
 * stream.
 *
 * The index contains:
 *  - A full CDM model (dimensions, variables, attributes) with coordinate-
 *    variable data embedded so that the reader never needs libnetcdf.
 *  - Per-variable HDF5 chunk layout (byte offset, compressed size, dataspace
 *    origin) enabling direct, lock-free reads from the source file.
 *
 * Requires both the netcdf-C and HDF5 C libraries at write time, but the
 * resulting index can be read without either library.
 */
class NetCDFProtobufIndexWriter
{
public:
    /**
     * Build the index for @p ncfile and write to @p outputfile which will
     * refer to the NetCDF file qs @p srcfile.
     * @throws std::runtime_error on any I/O or library error.
     */
    void write(const std::string& ncfile, const std::string& srcfile, const std::string& outputfile);

    /**
     * Build the index for @p ncfile and write to @p out (binary) which will
     * refer to the NetCDF file qs @p srcfile.
     * @throws std::runtime_error on any I/O or library error.
     */
    void write(const std::string& ncfile, const std::string& srcfile, std::ostream& out);
};

} // namespace MetNoFimex

#endif /* FIMEX_NETCDFPROTOBUFINDEXWRITER_H_ */
