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

#include "NetCDFProtobufIndexWriter.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "NetCDF_CDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "Nc3Header.h"

#include "fimex/CDM.h"
#include "fimex/CDMVariable.h"
#include "fimex/Logger.h"
#include "fimex/ProtobufCDM.h"

#include "netcdf_index.pb.h"

#include <hdf5.h>

#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace MetNoFimex {

namespace {

Logger_p logger = getLogger("fimex.NetCDFProtobufIndexWriter");

// ---- HDF5 RAII helpers -------------------------------------------------------

// clang-format off
template <herr_t (*Closer)(hid_t)>
struct H5Guard {
    hid_t id;
    explicit H5Guard(hid_t id_) : id(id_) {}
    ~H5Guard() { if (id >= 0) Closer(id); }
    H5Guard(const H5Guard&)            = delete;
    H5Guard& operator=(const H5Guard&) = delete;
};
// clang-format on
using H5FileGuard = H5Guard<&H5Fclose>;
using H5DatasetGuard = H5Guard<&H5Dclose>;
using H5SpaceGuard = H5Guard<&H5Sclose>;
using H5TypeGuard = H5Guard<&H5Tclose>;
using H5PlistGuard = H5Guard<&H5Pclose>;

/// RAII guard that suppresses HDF5 error messages for its lifetime.
/// Use in a tight nested block around a single HDF5 call to avoid hiding
/// legitimate errors from subsequent operations.
struct H5ErrorSuppressor
{
    H5E_auto2_t old_func;
    void* old_client_data;
    H5ErrorSuppressor()
    {
        H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
        H5Eset_auto2(H5E_DEFAULT, nullptr, nullptr);
    }
    ~H5ErrorSuppressor() { H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data); }
    H5ErrorSuppressor(const H5ErrorSuppressor&) = delete;
    H5ErrorSuppressor& operator=(const H5ErrorSuppressor&) = delete;
};

// ---- Coordinate-variable detection ------------------------------------------

/// A variable is a coordinate variable if it is 1-D with the same name as its
/// only dimension, or if it is scalar (0-D).
bool isCoordinateVariable(const CDMVariable& var, const CDM& cdm)
{
    const auto& shape = var.getShape();
    if (shape.empty())
        return true; // scalar
    if (shape.size() == 1 && cdm.hasDimension(var.getName()) && shape[0] == var.getName())
        return true;
    return false;
}

// ---- HDF5 chunk-layout extraction -------------------------------------------

/// Extract the HDF5 filter pipeline from @p plist_id into the proto variable.
/// Returns false if any mandatory filter cannot be fully parsed (caller should
/// skip the variable).
static bool extractFiltersFromPlist(hid_t plist_id, const std::string& hdf5_path, fimex_index::NetCDFVariable* fvar)
{
    const int nfilters = H5Pget_nfilters(plist_id);
    for (int fi = 0; fi < nfilters; fi++) {
        unsigned int flags = 0;
        size_t cd_nelmts = 16;
        unsigned int cd_values[16] = {};
        char filter_name[256] = {};
        const H5Z_filter_t ftype = H5Pget_filter2(plist_id, fi, &flags, &cd_nelmts, cd_values, sizeof(filter_name), filter_name, nullptr);
        if (ftype == H5Z_FILTER_SHUFFLE) {
            fvar->add_filters()->mutable_shuffle();
        } else if (ftype == H5Z_FILTER_DEFLATE) {
            fvar->add_filters()->mutable_deflate();
        } else if (ftype == H5Z_FILTER_SZIP) {
            // No '#ifdef LIBAEC': libaec is not required to read the parameters.
            if (cd_nelmts < 4) {
                LOG4FIMEX(logger, Logger::WARN, "HDF5: incomplete SZIP parameters on '" << hdf5_path << "' – skipping");
                return false;
            }
            auto* sz = fvar->add_filters()->mutable_szip();
            sz->set_nn_coding((cd_values[0] & 0x20u) != 0); // H5_SZIP_NN_OPTION_MASK
            sz->set_pixels_per_block(cd_values[1]);
            sz->set_bits_per_pixel(cd_values[2]);
            sz->set_pixels_per_scanline(cd_values[3]);
        } else {
            // Unknown filter: store as generic so the index captures the full
            // pipeline.  A reader that does not support this filter id will
            // report a clear error rather than silently producing corrupt data.
            LOG4FIMEX(logger, Logger::WARN, "HDF5: unknown filter " << ftype << " (" << filter_name << ") on '" << hdf5_path << "' – storing as generic");
            auto* gen = fvar->add_filters()->mutable_generic();
            gen->set_filter_id(static_cast<uint32_t>(ftype));
            for (size_t ci = 0; ci < cd_nelmts; ++ci)
                gen->add_cd_values(cd_values[ci]);
        }
    }
    return true;
}

/// Store a chunked dataset's layout: chunk shape and per-chunk addresses/offsets.
/// Offsets are stored in CDM order (reversed from HDF5's slowest-first order).
static void fillChunkedLayout(hid_t ds_id, hid_t space_id, hid_t plist_id, int hdf5_ndims, fimex_index::NetCDFVariable* fvar)
{
    std::vector<hsize_t> hdf5_chunk(hdf5_ndims);
    H5Pget_chunk(plist_id, hdf5_ndims, hdf5_chunk.data());
    for (int d = hdf5_ndims - 1; d >= 0; --d)
        fvar->add_chunk_shape(static_cast<uint64_t>(hdf5_chunk[d]));

    hsize_t n_chunks = 0;
    H5Dget_num_chunks(ds_id, space_id, &n_chunks);

    std::vector<hsize_t> hdf5_offset(hdf5_ndims);
    for (hsize_t ci = 0; ci < n_chunks; ci++) {
        haddr_t addr = 0;
        hsize_t chunk_size = 0;
        H5Dget_chunk_info(ds_id, space_id, ci, hdf5_offset.data(), nullptr, &addr, &chunk_size);
        auto* fc = fvar->add_chunks();
        fc->set_addr(static_cast<uint64_t>(addr));
        fc->set_size(static_cast<uint64_t>(chunk_size));
        fc->set_file_index(1);
        for (int d = hdf5_ndims - 1; d >= 0; --d)
            fc->add_offset(static_cast<uint64_t>(hdf5_offset[d]));
    }
}

/// Store a contiguous or compact dataset as a single chunk spanning the dataset.
static void fillContiguousLayout(hid_t ds_id, hid_t space_id, int hdf5_ndims, fimex_index::NetCDFVariable* fvar)
{
    std::vector<hsize_t> hdf5_shape(hdf5_ndims);
    H5Sget_simple_extent_dims(space_id, hdf5_shape.data(), nullptr);
    for (int d = hdf5_ndims - 1; d >= 0; --d)
        fvar->add_chunk_shape(static_cast<uint64_t>(hdf5_shape[d]));

    auto* fc = fvar->add_chunks();
    fc->set_addr(static_cast<uint64_t>(H5Dget_offset(ds_id)));
    fc->set_size(static_cast<uint64_t>(H5Dget_storage_size(ds_id)));
    fc->set_file_index(1);
    for (int d = 0; d < hdf5_ndims; d++)
        fc->add_offset(0);
}

/// Fill @p fvar with the HDF5 chunk layout for the dataset at @p hdf5_path
/// inside the already-open @p file_id.
/// Returns false if the dataset cannot be opened or has an unsupported layout.
bool fillChunkLayoutNc4(hid_t file_id, const std::string& hdf5_path, int cdm_ndims, fimex_index::NetCDFVariable* fvar)
{
    // Suppress HDF5 error messages only for the dataset-open call — some CDM
    // variables (string-type, virtual) may not exist in HDF5.
    hid_t raw_ds;
    {
        H5ErrorSuppressor suppress;
        raw_ds = H5Dopen2(file_id, hdf5_path.c_str(), H5P_DEFAULT);
    }
    if (raw_ds < 0) {
        LOG4FIMEX(logger, Logger::DEBUG, "HDF5: dataset not found: " << hdf5_path);
        return false;
    }
    H5DatasetGuard ds(raw_ds);

    H5TypeGuard dtype(H5Dget_type(ds.id));
    fvar->set_byte_order(H5Tget_order(dtype.id) == H5T_ORDER_BE ? fimex_index::BYTE_ORDER_BE : fimex_index::BYTE_ORDER_LE);

    H5PlistGuard plist(H5Dget_create_plist(ds.id));
    if (!extractFiltersFromPlist(plist.id, hdf5_path, fvar))
        return false;

    H5SpaceGuard space(H5Dget_space(ds.id));
    const int hdf5_ndims = H5Sget_simple_extent_ndims(space.id);
    if (hdf5_ndims != cdm_ndims) {
        LOG4FIMEX(logger, Logger::WARN, "HDF5: rank mismatch for '" << hdf5_path << "' (HDF5=" << hdf5_ndims << " CDM=" << cdm_ndims << ") – skipping");
        return false;
    }

    const H5D_layout_t layout = H5Pget_layout(plist.id);
    if (layout == H5D_CHUNKED) {
        fillChunkedLayout(ds.id, space.id, plist.id, hdf5_ndims, fvar);
    } else if (layout == H5D_CONTIGUOUS || layout == H5D_COMPACT) {
        fillContiguousLayout(ds.id, space.id, hdf5_ndims, fvar);
    } else {
        LOG4FIMEX(logger, Logger::WARN, "HDF5: unsupported layout for '" << hdf5_path << "' – skipping");
        return false;
    }
    return true;
}

// ---- NetCDF 3 chunk-layout extraction ----------------------------------------

/// Sizes in bytes for NC3 type codes 0..11 (CDF-5 adds 7–11).
static constexpr int NC3_TYPE_SIZES[12] = {
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

/// Fill @p fvar for a scalar (0-D) NC3 variable: single chunk at @p begin.
static bool fillScalarNc3Variable(const Nc3VarLayout& nc3var, int elem_size, fimex_index::NetCDFVariable* fvar)
{
    fvar->add_chunk_shape(1);
    auto* fc = fvar->add_chunks();
    fc->set_addr(nc3var.begin);
    fc->set_size(static_cast<uint64_t>(elem_size));
    fc->set_file_index(1);
    return true;
}

/// Fill @p fvar for a non-record (fixed) NC3 variable: one chunk spanning all data.
/// Chunk shape is in CDM order (fastest-varying first = reversed NC3 order).
static bool fillFixedNc3Variable(const Nc3Header& header, const Nc3VarLayout& nc3var, int ndims, int elem_size, fimex_index::NetCDFVariable* fvar)
{
    uint64_t total_elems = 1;
    for (int d = ndims - 1; d >= 0; --d) {
        const uint64_t dimlen = header.dims[static_cast<size_t>(nc3var.dimids[d])].length;
        fvar->add_chunk_shape(dimlen);
        if (dimlen > 0 && total_elems > std::numeric_limits<uint64_t>::max() / dimlen) {
            LOG4FIMEX(logger, Logger::WARN, "NC3: variable '" << nc3var.name << "' element count overflows uint64 – skipping");
            return false;
        }
        total_elems *= dimlen;
    }
    const uint64_t elem_size_u = static_cast<uint64_t>(elem_size);
    if (total_elems > std::numeric_limits<uint64_t>::max() / elem_size_u) {
        LOG4FIMEX(logger, Logger::WARN, "NC3: variable '" << nc3var.name << "' byte size overflows uint64 – skipping");
        return false;
    }
    auto* fc = fvar->add_chunks();
    fc->set_addr(nc3var.begin);
    fc->set_size(total_elems * elem_size_u);
    fc->set_file_index(1);
    for (int d = 0; d < ndims; ++d)
        fc->add_offset(0);
    return true;
}

/// Fill @p fvar for a record NC3 variable: one chunk per record.
/// Chunk shape in CDM order: [fastest_non-unlim_dim ... slowest_non-unlim_dim, 1].
/// NC3 dimids are slowest-first: dimids[0]=unlim, dimids[1..ndims-1]=non-unlim.
static bool fillRecordNc3Variable(const Nc3Header& header, const Nc3VarLayout& nc3var, int ndims, int elem_size, const CDM& cdm,
                                  fimex_index::NetCDFVariable* fvar)
{
    uint64_t nelems_per_rec = 1;
    for (int d = ndims - 1; d >= 1; --d) {
        const uint64_t dimlen = header.dims[static_cast<size_t>(nc3var.dimids[d])].length;
        fvar->add_chunk_shape(dimlen);
        if (dimlen > 0 && nelems_per_rec > std::numeric_limits<uint64_t>::max() / dimlen) {
            LOG4FIMEX(logger, Logger::WARN, "NC3: variable '" << nc3var.name << "' per-record element count overflows uint64 – skipping");
            return false;
        }
        nelems_per_rec *= dimlen;
    }
    fvar->add_chunk_shape(1); // unlimited dim: one record per chunk

    // The number of records from the CDM (authoritative, handles streaming).
    const CDMDimension* unlim = cdm.getUnlimitedDim();
    const uint64_t numrecs = unlim ? static_cast<uint64_t>(unlim->getLength()) : header.numrecs;
    // A streaming file (numrecs == UINT64_MAX) has an indeterminate count; skip.
    if (numrecs == std::numeric_limits<uint64_t>::max()) {
        LOG4FIMEX(logger, Logger::WARN, "NC3: record variable '" << nc3var.name << "' has indeterminate numrecs – skipping");
        return false;
    }

    const uint64_t elem_size_u = static_cast<uint64_t>(elem_size);
    if (nelems_per_rec > std::numeric_limits<uint64_t>::max() / elem_size_u) {
        LOG4FIMEX(logger, Logger::WARN, "NC3: variable '" << nc3var.name << "' per-record byte size overflows uint64 – skipping");
        return false;
    }
    const uint64_t chunk_bytes = nelems_per_rec * elem_size_u;
    const uint64_t stride = (header.total_rec_size > 0) ? header.total_rec_size : chunk_bytes;

    if (numrecs > 1 && stride > 0 && (numrecs - 1) > (std::numeric_limits<uint64_t>::max() - nc3var.begin) / stride) {
        LOG4FIMEX(logger, Logger::WARN, "NC3: record address for variable '" << nc3var.name << "' overflows uint64 – skipping");
        return false;
    }

    for (uint64_t rec = 0; rec < numrecs; ++rec) {
        auto* fc = fvar->add_chunks();
        fc->set_addr(nc3var.begin + rec * stride);
        fc->set_size(chunk_bytes);
        fc->set_file_index(1);
        // Offset in CDM order: [0 ... 0, rec] — slowest (unlimited) dim last.
        for (int d = 1; d < ndims; ++d)
            fc->add_offset(0);
        fc->add_offset(rec);
    }
    return true;
}

/// Fill @p fvar with the on-disk chunk layout for the NC3 variable @p nc3var.
/// NC3 is always big-endian, uncompressed, and unsorted.
/// Returns false if the variable should be skipped.
bool fillChunkLayoutNc3(const Nc3Header& header, const Nc3VarLayout& nc3var, const CDM& cdm, fimex_index::NetCDFVariable* fvar)
{
    if (nc3var.nc_type < 1 || nc3var.nc_type > 11) {
        LOG4FIMEX(logger, Logger::WARN, "NC3: unsupported nc_type=" << nc3var.nc_type << " for variable '" << nc3var.name << "' – skipping");
        return false;
    }
    // Belt-and-suspenders: validate dimids even though the parser already did.
    for (int dimid : nc3var.dimids) {
        if (dimid < 0 || static_cast<size_t>(dimid) >= header.dims.size()) {
            LOG4FIMEX(logger, Logger::WARN, "NC3: variable '" << nc3var.name << "' has out-of-range dimid " << dimid << " – skipping");
            return false;
        }
    }

    fvar->set_byte_order(fimex_index::BYTE_ORDER_BE); // NC3 is always big-endian

    const int elem_size = NC3_TYPE_SIZES[nc3var.nc_type];
    const int ndims = static_cast<int>(nc3var.dimids.size());
    const bool is_record = (ndims > 0) && (header.unlim_dimid >= 0) && (nc3var.dimids[0] == header.unlim_dimid);

    if (ndims == 0)
        return fillScalarNc3Variable(nc3var, elem_size, fvar);
    if (!is_record)
        return fillFixedNc3Variable(header, nc3var, ndims, elem_size, fvar);
    return fillRecordNc3Variable(header, nc3var, ndims, elem_size, cdm, fvar);
}

} // anonymous namespace

// ---- write() helper functions -----------------------------------------------

namespace {

/// Embed data for all coordinate variables and scalars into the CDM.
/// The reader can then serve them from memory without opening the source file.
static void embedCoordinateData(NetCDF_CDMReader& nc_reader, CDM& cdm)
{
    // getVariables() returns a const ref, so collect names first, then get
    // the mutable CDMVariable& via getVariable().
    for (const CDMVariable& var_const : cdm.getVariables()) {
        CDMVariable& var = cdm.getVariable(var_const.getName());
        if (var.hasData() || !isCoordinateVariable(var, cdm))
            continue;
        if (var.getDataType() == CDM_NAT || var.getDataType() == CDM_STRING || var.getDataType() == CDM_STRINGS)
            continue;
        try {
            var.setData(nc_reader.getData(var.getName()));
        } catch (const std::exception& e) {
            LOG4FIMEX(logger, Logger::WARN, "cannot read coordinate data for '" << var.getName() << "': " << e.what());
        }
    }
}

/// Build the chunk layout for all non-embedded NC3 variables in @p cdm.
static void buildNc3Layout(const Nc3Header& nc3, const CDM& cdm, fimex_index::NetCDFIndex& ncidx)
{
    for (const CDMVariable& var : cdm.getVariables()) {
        if (var.hasData() || var.getDataType() == CDM_NAT)
            continue;
        const Nc3VarLayout* nc3var = nullptr;
        for (const auto& nv : nc3.vars) {
            if (nv.name == var.getName()) {
                nc3var = &nv;
                break;
            }
        }
        if (!nc3var) {
            LOG4FIMEX(logger, Logger::DEBUG, "NC3: variable '" << var.getName() << "' not found in NC3 header – skipping");
            continue;
        }
        fimex_index::NetCDFVariable* fvar = ncidx.add_variables();
        fvar->set_name(var.getName());
        if (!fillChunkLayoutNc3(nc3, *nc3var, cdm, fvar))
            ncidx.mutable_variables()->RemoveLast();
    }
}

/// Build the chunk layout for all non-embedded NC4/HDF5 variables in @p cdm.
static void buildNc4Layout(const std::string& ncfile, const CDM& cdm, fimex_index::NetCDFIndex& ncidx)
{
    H5FileGuard h5file(H5Fopen(ncfile.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT));
    if (h5file.id < 0)
        throw std::runtime_error("HDF5: cannot open file: " + ncfile);

    for (const CDMVariable& var : cdm.getVariables()) {
        if (var.hasData() || var.getDataType() == CDM_NAT)
            continue;
        const int ndims = static_cast<int>(var.getShape().size());
        fimex_index::NetCDFVariable* fvar = ncidx.add_variables();
        fvar->set_name(var.getName());
        if (!fillChunkLayoutNc4(h5file.id, "/" + var.getName(), ndims, fvar))
            ncidx.mutable_variables()->RemoveLast();
    }
}

} // namespace

// ---- public API --------------------------------------------------------------

const char NETCDF_PROTO_INDEX_MAGIC[4]{'N', 0, 0, 0};

void NetCDFProtobufIndexWriter::write(const std::string& ncfile, const std::string& srcfile, const std::string& outputfile)
{
    std::ofstream out(outputfile, std::ios::binary);
    if (!out)
        throw std::runtime_error("cannot open output file: " + outputfile);
    write(ncfile, srcfile, out);
}

void NetCDFProtobufIndexWriter::write(const std::string& ncfile, const std::string& srcfile, std::ostream& out)
{
    NetCDF_CDMReader nc_reader(ncfile, false);
    CDM cdm = nc_reader.getCDM();
    embedCoordinateData(nc_reader, cdm);

    fimex_index::NetCDFIndex ncidx;
    ProtobufCDM::writeCDM(ncidx.mutable_cdm(), cdm);
    ncidx.add_files(""); // files[0] = missing-data sentinel
    ncidx.add_files(srcfile);

    if (Nc3Header::isNc3File(ncfile))
        buildNc3Layout(Nc3Header::parse(ncfile), cdm, ncidx);
    else
        buildNc4Layout(ncfile, cdm, ncidx);

    out.write(FIMEX_PROTO_INDEX_MAGIC, sizeof(FIMEX_PROTO_INDEX_MAGIC));
    out.write(NETCDF_PROTO_INDEX_MAGIC, sizeof(NETCDF_PROTO_INDEX_MAGIC));
    if (!ncidx.SerializeToOstream(&out))
        throw std::runtime_error("could not serialise NetCDF protobuf index");
}

} // namespace MetNoFimex
