SET(HAVE_NETCDF_H 1)

CONFIGURE_FILE(fimex_netcdf_config.h.in fimex_netcdf_config.h)

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

FIMEX_ADD_LIBRARY(fimex-io-netcdf "${libfimex_netcdf_SOURCES}" "${IO_PACKAGES};${netCDF_PACKAGE}")
TARGET_INCLUDE_DIRECTORIES(libfimex-io-netcdf PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
