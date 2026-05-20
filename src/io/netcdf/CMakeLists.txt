SET(HAVE_NETCDF_H 1)

IF(ENABLE_PROTOBUF)
  SET(HAVE_PROTOBUF 1)
ENDIF()

FIND_PROGRAM(NCCOPY_EXECUTABLE nccopy)

SET(libfimex_netcdf_SOURCES
  NetCDF_CDMReader.cc
  NetCDF_CDMReader.h
  NetCDF_CDMWriter.cc
  NetCDF_CDMWriter.h
  NetCDF_Utils.cc
  NetCDF_Utils.h
  NetCDFIoFactory.cc
  NetCDFIoFactory.h
)

SET(libfimex_netcdf_LINK "${IO_PACKAGES};${netCDF_PACKAGE}")

IF(ENABLE_PROTOBUF AND HAVE_NETCDF_HDF5_LIB)
  SET(metno_protos_dir "${CMAKE_CURRENT_SOURCE_DIR}/protobuf")
  SET(netcdf_protos netcdf_index.proto)
  protoc_generated(netcdf_proto_gen "${CMAKE_CURRENT_BINARY_DIR}" netcdf_index)
  protoc_generate("${netcdf_proto_gen}" "${netcdf_protos}"
    "${metno_protos_dir};${CMAKE_SOURCE_DIR}/src/protobuf")

  LIST(APPEND libfimex_netcdf_SOURCES
    NetCDFProtobufIndexWriter.cc
    NetCDFProtobufIndexWriter.h
    NetCDFProtobufIndexReader.cc
    NetCDFProtobufIndexReader.h
    Nc3Header.cc
    Nc3Header.h
    NetCDFProtobufCDMReader.cc
    NetCDFProtobufCDMReader.h
    "${netcdf_proto_gen}"
  )
  list_prepend_dir(libfimex_netcdf_SOURCES "${metno_protos_dir}" ${netcdf_protos})

  # The writer uses the HDF5 C API directly; the reader needs zlib.
  # Both are available as transitive dependencies of the netCDF target, but
  # we make the dependency explicit for clarity.
  FIND_PACKAGE(HDF5 REQUIRED COMPONENTS C)
  FIND_PACKAGE(ZLIB REQUIRED)
  LIST(APPEND libfimex_netcdf_LINK "${PROTOBUF_LIBRARIES};HDF5::HDF5;ZLIB::ZLIB")

  # SZIP decompression via libaec (implements H5Z_FILTER_SZIP = 4).
  FIMEX_FIND_PACKAGE(libaec CMAKE_NAME libaec LIBRARY_NAME aec INCLUDE_HDR libaec.h)
  LIST(APPEND libfimex_netcdf_LINK ${libaec_PACKAGE})
  SET(HAVE_LIBAEC 1)
ENDIF()

FIMEX_ADD_LIBRARY(fimex-io-netcdf "${libfimex_netcdf_SOURCES}" "${libfimex_netcdf_LINK}")
TARGET_INCLUDE_DIRECTORIES(libfimex-io-netcdf PUBLIC
  "${CMAKE_CURRENT_BINARY_DIR}"
  "${CMAKE_CURRENT_SOURCE_DIR}"
  "${CMAKE_BINARY_DIR}/src"
  $<$<BOOL:${LIBAEC_INCLUDE_DIR}>:${LIBAEC_INCLUDE_DIR}>
)

CONFIGURE_FILE(fimex_netcdf_config.h.in fimex_netcdf_config.h)
