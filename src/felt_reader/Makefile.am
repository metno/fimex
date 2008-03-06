AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I. -I.. ${milib_CFLAGS} ${XML_CPPFLAGS} -Ifelt_reader


lib_LIBRARIES = libutplukk_felt.a
libutplukk_felt_a_SOURCES = 
include_HEADERS =

if HAVE_MILIB
libutplukk_felt_a_SOURCES += FeltCDMReader.cc Felt_Array.cc Felt_File.cc FeltParameters.cc Felt_File_Error.cc
include_HEADERS += FeltCDMReader.h Felt_File.h Felt_Array.h FeltParameters.h Felt_File_Error.h
endif
