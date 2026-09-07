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

#ifndef FIMEX_NETCDFPROTOBUFCDMREADER_H_
#define FIMEX_NETCDFPROTOBUFCDMREADER_H_

#include "fimex/CDMReader.h"
#include "fimex/XMLInput.h"

#include <memory>
#include <string>

namespace MetNoFimex {

/**
 * A CDMReader that serves metadata and data entirely from a pre-built protobuf
 * NetCDF index, without using libnetcdf or libhdf5.
 *
 * Coordinate-variable data is read from the index's embedded CDM.  All other
 * variable data is read directly from the original .nc4 file as raw bytes via
 * ChunkReader (pread-based, fully thread-safe), decompressed with zlib, and
 * un-shuffled in software.
 *
 * Multiple instances can safely read from the same source file in parallel.
 */
class NetCDFProtobufCDMReader : public CDMReader
{
public:
    /**
     * @param indexFile  Path to the .ncfp (NetCDF protobuf index) file.
     * @param configXML  Reader configuration.  If it contains a
     *                   @c /cdm_fimex_index_reader_config/root_path element,
     *                   that value is used to resolve source filenames stored
     *                   in the index; otherwise the directory of @p indexFile
     *                   is used.
     */
    explicit NetCDFProtobufCDMReader(const std::string& indexFile, const XMLInput& configXML);

    /**
     * @param indexFile  Path to the .ncfp (NetCDF protobuf index) file.
     * @param rootPath   Directory used to resolve the source .nc4 filename
     *                   stored in the index.  Defaults to the directory of
     *                   @p indexFile when empty.
     */
    explicit NetCDFProtobufCDMReader(const std::string& indexFile, const std::string& rootPath = {});
    ~NetCDFProtobufCDMReader() override;

    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos) override;
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override;

private:
    struct Impl;
    std::unique_ptr<Impl> p_;
};

} // namespace MetNoFimex

#endif /* FIMEX_NETCDFPROTOBUFCDMREADER_H_ */
