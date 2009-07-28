AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include ${milib_CFLAGS} ${XML_CPPFLAGS}


lib_LTLIBRARIES = libfimex_felt.la
libfimex_felt_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION)
libfimex_felt_la_SOURCES = FeltCDMReader2.cc Felt_Array2.cc Felt_File2.cc FeltParameters.cc Felt_File_Error.cc

if HAVE_MILIB
libfimex_felt_la_SOURCES += FeltCDMReader.cc Felt_Array.cc Felt_File.cc
endif
