SUBDIRS = . felt_reader binSrc

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I. ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${XML_CPPFLAGS} -Ifelt_reader

lib_LIBRARIES = libutplukk.a
libutplukk_a_SOURCES = interpolation.c CachedInterpolation.cc CDM.cc CDMAttribute.cc CDMDimension.cc CDMDataType.cc CDMExtractor.cc CDMInterpolator.cc CDMReader.cc CDMVariable.cc Data.cc Null_CDMWriter.cc Utils.cc 
include_HEADERS = config.h CachedInterpolation.h CDMAttribute.h CDMDataType.h CDMDimension.h CDMException.h CDMExtractor.h CDMInterpolator.h CDMReader.h CDMVariable.h CDMWriter.h CDM.h Data.h DataImpl.h interpolation.h Null_CDMWriter.h Utils.h

if HAVE_NETCDF
  libutplukk_a_SOURCES += NetCDF_CF10_CDMReader.cc NetCDF_CDMWriter.cc NetCDF_Utils.cc
  include_HEADERS += NetCDF_CF10_CDMReader.h NetCDF_CDMWriter.h NetCDF_Utils.h
endif
