/*
  Fimex, src/NetCDFIoFactory.cc

  Copyright (C) 2019-2026 met.no

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

#include "NetCDFIoFactory.h"

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "NetCDF_CDMReader.h"
#include "NetCDF_CDMWriter.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include "fimex/CDMException.h"
#include "fimex/ChunkReaderFactory.h"
#include "fimex/FileUtils.h"
#include "fimex/IoPlugin.h"
#include "fimex/Logger.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/StringUtils.h"
#include "fimex/XMLInputString.h"
#include "fimex/XMLUtils.h"

#include "fimex_netcdf_config.h"
#ifdef HAVE_PROTOBUF
#include "NetCDFProtobufCDMReader.h"
#include "NetCDFProtobufIndexReader.h"
#endif

#include <regex>
#include <set>

namespace MetNoFimex {

const char FILETYPE_NCFP[] = "ncfp";

namespace {

Logger_p logger = getLogger("fimex.NetCDFIoFactory");

bool detectHDF5(const char* magic)
{
    const char* hdf5 = "\211HDF\r\n\032";
    for (int i = 0; i < 7; i++) {
        if (hdf5[i] != magic[i])
            return false;
    }
    return true;
}

bool detectNetCDF(const char* magic)
{
    const char* netcdf = "CDF\001";
    for (int i = 0; i < 4; i++) {
        if (netcdf[i] != magic[i])
            return false;
    }
    return true;
}

bool isNetCDF4Type(const std::string& type)
{
    return (type == "nc4" || type == "nc" || type == "netcdf4" || type == "netcdf");
}

bool isNetCDF4ClassicType(const std::string& type)
{
    return (type == "nc4c" || type == "nc4classic" || type == "netcdf4classic");
}

bool isNetCDF3Type(const std::string& type)
{
    return (type == "nc3" || type == "cdf" || type == "netcdf3");
}

int getNetCDFVersion(const std::string& type)
{
    if (isNetCDF4Type(type)) {
        return 5;
    }
    if (isNetCDF4ClassicType(type)) {
        return 4;
    }
    if (isNetCDF3Type(type)) {
        return 3;
    }
    return 0;
}

bool isNetCDFType(const std::string& type)
{
    return getNetCDFVersion(type) != 0 || (type == "ncdap");
}

bool isNetCDFZarrFile(const std::string& file)
{
    static const std::regex zarr_file("^file:.*#.*mode=.*zarr.*");
    std::smatch zarr_file_match;
    return std::regex_match(file, zarr_file_match, zarr_file);
}

// #define ALL_OPENDAP_SERVERS_SUPPORT_RANGE_REQUESTS 0

/// Probe whether @p url points to an OPeNDAP server by fetching the first few
/// bytes of the Dataset Descriptor Structure (DDS) endpoint (<url>.dds).
/// A genuine OPeNDAP DDS response begins with "Dataset {".
/// Returns true only if the probe succeeds and the response matches.
/// Any error (network timeout, 404, non-OPeNDAP server) returns false.
bool probeOpenDAP(const std::string& url)
{
    // Strip an optional query string / constraint expression before appending
    // ".dds", so that e.g. "https://host/data.nc?var[0:10]" becomes
    // "https://host/data.nc.dds".
    const auto qmark = url.find('?');
    const std::string base = (qmark != std::string::npos) ? url.substr(0, qmark) : url;
    const std::string dds_url = base + ".dds";

#ifdef ALL_OPENDAP_SERVERS_SUPPORT_RANGE_REQUESTS
    // "Dataset {" is the mandatory opening of every valid DDS response.
    static const std::string DDS_MAGIC = "Dataset {";
    static const size_t PROBE_BYTES = DDS_MAGIC.size();
#endif

    try {
        auto cr = createDefaultChunkReaderFactory()->readerFor(dds_url);
#ifdef ALL_OPENDAP_SERVERS_SUPPORT_RANGE_REQUESTS
        if (cr->size() < PROBE_BYTES)
            return false;
        std::vector<unsigned char> buf(PROBE_BYTES);
        cr->read(0, PROBE_BYTES, buf.data());
        return std::equal(DDS_MAGIC.begin(), DDS_MAGIC.end(), reinterpret_cast<const char*>(buf.data()));
#else  // !ALL_OPENDAP_SERVERS_SUPPORT_RANGE_REQUESTS
       // guess that it is OPeNDAP if there is some response for the dds url
        return cr->size() > 0;
#endif // !ALL_OPENDAP_SERVERS_SUPPORT_RANGE_REQUESTS
    } catch (const std::exception& e) {
        LOG4FIMEX(logger, Logger::DEBUG, "OPeNDAP probe failed for " << dds_url << ": " << e.what());
        return false;
    }
}

} // namespace

size_t NetCDFIoFactory::matchMagicSize()
{
#ifdef HAVE_PROTOBUF
    return std::max(size_t(7), getNetCDFProtobufIndexMagicSize());
#else
    return 7;
#endif
}

int NetCDFIoFactory::matchMagic(const char* magic, size_t count)
{
    if ((count >= 7 && detectHDF5(magic)) || (count >= 4 && detectNetCDF(magic)))
        return 1;
#ifdef HAVE_PROTOBUF
    if (checkNetCDFProtobufIndexMagic(magic, count))
        return 1;
#endif
    return 0;
}

bool NetCDFIoFactory::matchFileType(const std::string& type, bool& is_rw)
{
    std::string nc;
    is_rw = false;
    static const std::regex re_type_rw("(\\w+)(\\+rw)?");
    std::smatch what;
    if (std::regex_match(type, what, re_type_rw)) {
        nc = what[1].str();
        is_rw = what[2].matched;
    }
    return isNetCDFType(nc) ? 1 : 0;
}

int NetCDFIoFactory::matchFileTypeName(const std::string& type)
{
    bool is_rw = false;
    return matchFileType(type, is_rw) ? 1 : 0;
}

int NetCDFIoFactory::matchFileName(const std::string& fileName)
{
    const std::string ext = getExtension(fileName);
    if (ext == "nc" || ext == "nc4")
        return 1;
#ifdef HAVE_PROTOBUF
    if (ext == FILETYPE_NCFP)
        return 1;
#endif

    // match urls with scheme "dods" -- explicit OPeNDAP alias
    if (starts_with(fileName, "dods://") || starts_with(fileName, "dap://") || starts_with(fileName, "dap4://"))
        return 1;

    // For http(s):// URLs probe the .dds endpoint to detect OPeNDAP servers.
    // This is potentially slow (network round-trip) but can be bypassed by
    // specifying the file type explicitly (e.g. --file-type ncdap).
    if (starts_with(fileName, "http://") || starts_with(fileName, "https://")) {
        if (probeOpenDAP(fileName)) {
            // after a successful probe, it is very likely that this actualy is OPeNDAP
            // so we return 2 (reader factory maximises)
            return 2;
        }
    }

    if (isNetCDFZarrFile(fileName))
        return 1;

    return 0;
}

CDMReader_p NetCDFIoFactory::createReader(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                          const std::vector<std::string>& args)
{
    bool is_rw = false;
    matchFileType(fileTypeName, is_rw);
    if (is_rw)
        return createReaderWriter(fileTypeName, fileName, config, args);

#ifdef HAVE_PROTOBUF
    try {
        const size_t magic_size = getNetCDFProtobufIndexMagicSize();
        auto cr = createDefaultChunkReaderFactory()->readerFor(fileName);
        if (cr->size() >= magic_size) {
            std::vector<char> magic_buf(magic_size, 0);
            cr->read(0, magic_size, reinterpret_cast<unsigned char*>(magic_buf.data()));
            if (checkNetCDFProtobufIndexMagic(magic_buf.data(), magic_size)) {
                return std::make_shared<NetCDFProtobufCDMReader>(fileName, config);
            }
        }
    } catch (const std::exception&) {
        // ignore if probing fails
    }
#endif

    CDMReader_p reader;
    std::vector<std::string> files;
    expand_files(files, fileName);
    if (files.size() > 1) {
        std::ostringstream ncml;
        ncml << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << std::endl
             << "<netcdf xmlns=\"http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2\">"
             << "<aggregation type=\"joinExisting\">";
        for (const auto& f : files) {
            ncml << "<netcdf location=\"";
            escapeXmlToStream(ncml, f);
            ncml << "\" />";
        }
        ncml << "</aggregation></netcdf>";
        reader = std::make_shared<NcmlCDMReader>(XMLInputString(ncml.str()));
    } else {
        std::string file = fileName;

        // remove file: URL-prefix, except for zarr
        if (!isNetCDFZarrFile(file)) {
            file = std::regex_replace(file, std::regex("^file:"), "", std::regex_constants::format_first_only);
        }

        // java-netcdf allows dods: prefix for dods-files while netcdf-C requires http:
        // Also normalise dap:// and dap4:// to http://.
        file = std::regex_replace(file, std::regex("^(dods|dap|dap4):"), "http:", std::regex_constants::format_first_only);

        reader = std::make_shared<NetCDF_CDMReader>(file, false);
    }
    if (!config.isEmpty())
        reader = std::make_shared<NcmlCDMReader>(reader, config);
    return reader;
}

CDMReaderWriter_p NetCDFIoFactory::createReaderWriter(const std::string&, const std::string& fileName, const XMLInput& config, const std::vector<std::string>&)
{
    if (!config.isEmpty())
        throw CDMException("Cannot open writeable NetCDF file with Ncml config: " + config.id());
    return std::make_shared<NetCDF_CDMReader>(fileName, true);
}

void NetCDFIoFactory::createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& config)
{
    auto version = getNetCDFVersion(fileTypeName);
    if (version == 0) {
        version = 5; // default to netcdf-4
    }
    NetCDF_CDMWriter(input, fileName, config, version);
}

} // namespace MetNoFimex

DEFINE_IO_PLUGIN("netcdf", MetNoFimex::NetCDFIoFactory)
