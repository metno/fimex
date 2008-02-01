SUBDIRS = . felt_reader

AM_CFLAGS = -std=c99 -Wall -pedantic -g
AM_CXXFLAGS = -Wall -g

INCLUDE = @XML_CFLAGS@

AM_CPPFLAGS = -I. -I/disk1/metno/local/include/milib -Ifelt_reader -I/usr/include/netcdf-3

lib_LIBRARIES = libutplukk.a
libutplukk_a_SOURCES = interpolation.c TimeException.cc Time.cc CDM.cc CDMAttribute.cc CDMDimension.cc CDMDataType.cc CDMVariable.cc Data.cc Utils.cc NetCDF_CDMWriter.cc
include_HEADERS = CDMAttribute.h CDMDataType.h CDMDimension.h CDMException.h CDM.h CDMReader.h CDMVariable.h CDMWriter.h Data.h interpolation.h NetCDF_CDMWriter.h TimeException.h Time.h TimeSliceData.h Utils.h

