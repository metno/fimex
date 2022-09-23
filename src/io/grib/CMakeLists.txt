IF(ENABLE_GRIBAPI)
  SET(HAVE_GRIB_API 1)
ELSE()
  SET(HAVE_ECCODES 1)
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
  GribCDMReader.cc
  GribCDMReader.h
  GribFileIndex.cc
  GribFileIndex.h
  GribUtils.cc
  GribUtils.h
  GribIoFactory.cc
  GribIoFactory.h
  )

FIMEX_ADD_LIBRARY(fimex-io-grib "${libfimex_grib_SOURCES}" "${IO_PACKAGES};${eccodes_PACKAGE};${grib_api_PACKAGE};${date_PACKAGE}")
TARGET_INCLUDE_DIRECTORIES(libfimex-io-grib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
