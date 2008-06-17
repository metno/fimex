AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I../../include -I$(top_srcdir)/include ${milib_CFLAGS} ${XML_CPPFLAGS}


lib_LIBRARIES = libfimex_felt.a
libfimex_felt_a_SOURCES = 

if HAVE_MILIB
libfimex_felt_a_SOURCES += FeltCDMReader.cc Felt_Array.cc Felt_File.cc FeltParameters.cc Felt_File_Error.cc
endif
