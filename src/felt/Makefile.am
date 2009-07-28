AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include


lib_LTLIBRARIES = libfelt.la
libfelt_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION)
libfelt_la_SOURCES = FeltField.cpp FeltFile.cpp FeltGridDefinition.cpp feltTypeConversion.cpp
