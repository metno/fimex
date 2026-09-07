/*
 * Fimex, test/testNetCDFProtobufIndex.cc
 *
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
 * Focused test for the NetCDF protobuf index: verifies that sub-slices
 * retrieved via a SliceBuilder whose extents do NOT align with the underlying
 * HDF5 chunk boundaries produce identical data when read through
 * NetCDFProtobufCDMReader versus the original NetCDF_CDMReader.
 *
 * Input: grib/gfs/gfs_0p25_20240717_00.nc from TEST_EXTRADATA_DIR.
 * The test is skipped if that file is absent.
 */

#include "testinghelpers.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "NetCDF_CDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "NetCDFProtobufCDMReader.h"
#include "NetCDFProtobufIndexWriter.h"

#include "fimex/CDM.h"
#include "fimex/CDMDimension.h"
#include "fimex/CDMVariable.h"
#include "fimex/Data.h"
#include "fimex/FileUtils.h"
#include "fimex/SliceBuilder.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "fimex_netcdf_config.h"

using namespace MetNoFimex;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
namespace {

// RAII guard that removes a file on destruction.
// Does nothing if the stored path is empty.

// clang-format off
struct FileGuard {
    std::string path;
    explicit FileGuard(std::string p) : path(std::move(p)) {}
    ~FileGuard() { if (!path.empty()) remove(path); }
    FileGuard(const FileGuard&) = delete;
    FileGuard& operator=(const FileGuard&) = delete;
};
// clang-format on

// Compare two DataPtr values element-wise.  All differences exceeding 'tol'
// are reported.  Returns the number of differing elements.
size_t countDiffs(DataPtr ref, DataPtr got, double tol)
{
    if (!ref || !got || ref->size() != got->size())
        return std::numeric_limits<size_t>::max();
    const size_t n = ref->size();
    size_t bad = 0;
    for (size_t i = 0; i < n; ++i) {
        double r = ref->getDouble(i);
        double g = got->getDouble(i);
        if (std::isnan(r) && std::isnan(g))
            continue;
        if (std::abs(r - g) > tol)
            ++bad;
    }
    return bad;
}

/**
 * If @p dimName is present in @p var's shape and [@p start, @p start + @p size)
 * fits within its extent, call sb.setStartAndSize() and return true.
 * Returns false if the dimension is not part of the variable or the range
 * exceeds the dimension length (in which case @p sb is left unchanged).
 */
bool setSliceDim(SliceBuilder& sb, const CDMVariable& var, const CDM& cdm, const std::string& dimName, size_t start, size_t size)
{
    const std::vector<std::string>& shape = var.getShape();
    if (std::find(shape.begin(), shape.end(), dimName) == shape.end())
        return false;
    const size_t len = cdm.getDimension(dimName).getLength();
    if (start + size > len)
        return false;
    sb.setStartAndSize(dimName, start, size);
    return true;
}

// Variants: { human-readable tag, extra args for nccopy or nullptr for original }.
struct Variant
{
    const char* desc;
    const char* args;
};

void test_variant(miutil::cpptest::test_recorder* mi_cpptest_recorder, const std::string& testName, const Variant& variant, const std::string& ncFile,
                  const std::string& varName, const SliceBuilder& sb, const DataPtr refData)
{
    // Optionally produce a converted copy via nccopy.
    std::string srcFile = ncFile;
    std::string ncFileVariant;
    if (variant.args) {
#ifdef NCCOPY_EXECUTABLE
        static const std::string NCCOPY(NCCOPY_EXECUTABLE);
        // nccopy does not re-compress chunks => create an uncompressed copy first
        const std::string ncFileNone = testName + "none.nc";
        const std::string cmd_none = NCCOPY + " -F none -k4 " + " \"" + ncFile + "\" \"" + ncFileNone + "\"";
        const auto cmd_none_result = std::system(cmd_none.c_str());
        TEST4FIMEX_CHECK_MESSAGE(cmd_none_result == 0, "nccopy command '" << cmd_none << "' failed");
        const FileGuard ncGuardNone(ncFileNone);

        // now apply filters / chunking / ...
        ncFileVariant = testName + variant.desc + ".nc";
        const std::string cmd = NCCOPY + " " + variant.args + " \"" + ncFileNone + "\" \"" + ncFileVariant + "\"";
        const auto cmd_result = std::system(cmd.c_str());
        TEST4FIMEX_CHECK_MESSAGE(cmd_result == 0, "nccopy command '" << cmd << "' failed");
        srcFile = ncFileVariant;
#else  // !NCCOPY_EXECUTABLE
        return;
#endif // !NCCOPY_EXECUTABLE
    }

    const std::string idxFileVariant = testName + "_" + variant.desc + ".ncfp";

    const FileGuard ncGuard(ncFileVariant);
    const FileGuard idxGuard(idxFileVariant);

    NetCDFProtobufIndexWriter().write(srcFile, srcFile, idxFileVariant);
    if (!exists(idxFileVariant)) {
        TEST4FIMEX_CHECK_MESSAGE(false, "[" << variant.desc << "] index file not written");
        return;
    }

    auto idxReader = std::make_shared<NetCDFProtobufCDMReader>(idxFileVariant);
    if (!idxReader->getCDM().hasVariable(varName)) {
        TEST4FIMEX_CHECK_MESSAGE(false, "[" << variant.desc << "] variable " << varName << " not found");
        return;
    }

    DataPtr idxData = idxReader->getDataSlice(varName, sb);
    if (!idxData || refData->size() != idxData->size()) {
        TEST4FIMEX_CHECK_MESSAGE(false, "[" << variant.desc << "] data size mismatch or null");
        return;
    }

    const size_t bad = countDiffs(refData, idxData, 1e-5);
    TEST4FIMEX_CHECK_MESSAGE(bad == 0, "[" << variant.desc << "] " << bad << "/" << refData->size() << " elements differ between NetCDF and index reader");
}
} // anonymous namespace

TEST4FIMEX_TEST_CASE(test_subslice_air_temperature_pl)
{
    if (!hasTestExtra())
        return;

    const auto ncFile = pathTestExtra("grib/gfs/gfs_0p25_20240717_00.nc");
    const std::string varName = "air_temperature_pl";

    auto refReader = std::make_shared<NetCDF_CDMReader>(ncFile, false);

    const CDM& cdm = refReader->getCDM();
    TEST4FIMEX_REQUIRE_MESSAGE(cdm.hasVariable(varName), "variable " << varName << " not found");
    const CDMVariable& var = cdm.getVariable(varName);
    SliceBuilder sb(cdm, varName);
    // clang-format off
    const bool sliceOk =
        setSliceDim(sb, var, cdm, "time",      1, 5) &&
        setSliceDim(sb, var, cdm, "latitude",  5, 3) &&
        setSliceDim(sb, var, cdm, "longitude", 3, 9);
    // clang-format on
    TEST4FIMEX_REQUIRE_MESSAGE(sliceOk, "failed to set slice dimensions");

    const DataPtr refData = refReader->getDataSlice(varName, sb);
    TEST4FIMEX_REQUIRE_MESSAGE(refData, "ref data size is null");

    // clang-format off
    const Variant variants[] = {
        { "original",       nullptr },
#ifdef NCCOPY_EXECUTABLE
        { "deflate-1-shuf", "-d 1 -s" },
        { "deflate-9",      "-d 9" },
        { "chunksize-4",    "-k4 -c time/4,longitude/4,latitude/4" },
        { "szip",           "-F 'air_temperature_pl,4,4,8'" },
        { "szip32",         "-F 'air_temperature_pl,4,32,8'" },
#endif // NCCOPY_EXECUTABLE
    };
    // clang-format on

    for (const auto& v : variants) {
        test_variant(mi_cpptest_recorder, "nc4_gfs", v, ncFile, varName, sb, refData);
    }
}

TEST4FIMEX_TEST_CASE(test_ncfp_nc3)
{
    const auto ncFile = pathTest("verticalOceanSG2.nc"); // included in fimex repo
    const std::string varName = "temp";

    auto refReader = std::make_shared<NetCDF_CDMReader>(ncFile, false);

    const CDM& cdm = refReader->getCDM();
    TEST4FIMEX_REQUIRE_MESSAGE(cdm.hasVariable(varName), "variable " << varName << " not found");
    const CDMVariable& var = cdm.getVariable(varName);
    SliceBuilder sb(cdm, varName);
    // clang-format off
    const bool sliceOk =
        setSliceDim(sb, var, cdm, "xi_rho",     2, 9)  &&
        setSliceDim(sb, var, cdm, "eta_rho",    3, 10) &&
        setSliceDim(sb, var, cdm, "s_rho",      5, 10) &&
        setSliceDim(sb, var, cdm, "ocean_time", 1, 2);
    // clang-format on
    TEST4FIMEX_REQUIRE_MESSAGE(sliceOk, "failed to set slice dimensions");

    const DataPtr refData = refReader->getDataSlice(varName, sb);
    TEST4FIMEX_REQUIRE_MESSAGE(refData, "ref data size is null");

    const Variant variants[] = {
        {"cdf1", nullptr}, // original, no nccopy
#ifdef NCCOPY_EXECUTABLE
        {"cdf2", "-k 2"}, // 64-bit offset (CDF-2)
        {"cdf5", "-k 5"}, // 64-bit data   (CDF-5)
#endif                    // NCCOPY_EXECUTABLE
    };

    for (const auto& v : variants) {
        test_variant(mi_cpptest_recorder, "nc3_ocean", v, ncFile, varName, sb, refData);
    }
}
