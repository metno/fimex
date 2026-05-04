IF(ENABLE_GRIBAPI)
  SET(HAVE_GRIB_API 1)
ELSE()
  SET(HAVE_ECCODES 1)
ENDIF()
IF(ENABLE_PROTOBUF)
  SET(HAVE_PROTOBUF 1)
ENDIF()

CONFIGURE_FILE(fimex_grib_config.h.in fimex_grib_config.h)

SET(libfimex_grib_SOURCES
  GribApiCDMWriter.cc
  GribApiCDMWriter.h
  GribApiCDMWriter_ImplAbstract.cc
  GribApiCDMWriter_ImplAbstract.h
  GribApiCDMWriter_Impl1.cc
  GribApiCDMWriter_Impl1.h
  GribApiCDMWriter_Impl2.cc
  GribApiCDMWriter_Impl2.h
  GribCDMIndexer.cc
  GribCDMIndexer.h
  GribCDMReader.cc
  GribCDMReader.h
  GribFileIndex.cc
  GribFileIndex.h
  GribUtils.cc
  GribUtils.h
  GribIoFactory.cc
  GribIoFactory.h
  GribReaderConfig.cc
  GribReaderConfig.h

  LogTimer.cc
  LogTimer.h
)

IF(ENABLE_PROTOBUF)
  SET (metno_protos_dir "${CMAKE_CURRENT_SOURCE_DIR}/protobuf")
  SET (grib_protos grib_index.proto)
  protoc_generated(grib_proto_gen "${CMAKE_CURRENT_BINARY_DIR}" grib_index)
  protoc_generate("${grib_proto_gen}" "${grib_protos}" "${metno_protos_dir};${CMAKE_SOURCE_DIR}/src/protobuf")

  LIST(APPEND libfimex_grib_SOURCES
    GribProtobufIndexReader.cc
    GribProtobufIndexReader.h
    GribProtobufIndexWriter.cc
    GribProtobufIndexWriter.h

    "${grib_proto_gen}"
  )
  list_prepend_dir(libfimex_grib_SOURCES "${metno_protos_dir}" ${grib_protos})
ENDIF(ENABLE_PROTOBUF)

FIMEX_ADD_LIBRARY(fimex-io-grib "${libfimex_grib_SOURCES}" "${IO_PACKAGES};${eccodes_PACKAGE};${grib_api_PACKAGE};${date_PACKAGE};${PROTOBUF_LIBRARIES}")
TARGET_INCLUDE_DIRECTORIES(libfimex-io-grib PUBLIC "${CMAKE_CURRENT_BINARY_DIR};${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_BINARY_DIR}/src")
