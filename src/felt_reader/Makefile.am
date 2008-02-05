AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall
AM_CPPFLAGS = -I. -I.. -I/disk1/metno/local/include/milib -I/usr/include/libxml2 -Ifelt_reader -I/usr/include/netcdf-3

lib_LIBRARIES = libutplukk_felt.a
libutplukk_felt_a_SOURCES = FeltCDMReader.cc Felt_Array.cc Felt_File.cc FeltParameters.cc Felt_File_Error.cc
include_HEADERS = FeltCDMReader.h Felt_File.h Felt_Array.h FeltParameters.h Felt_File_Error.h

