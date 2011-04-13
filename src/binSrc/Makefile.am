SUBDIRS = .

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include -DDATADIR="\"$(datadir)\"" ${milib_CFLAGS} ${NETCDF_CPPFLAGS}  @MIFI_GRIB_API_CPPFLAGS@ ${XML_CPPFLAGS}

bin_PROGRAMS = fimex
fimex_SOURCES = fimex.cc
fimex_LDADD = ../libfimex.la @BOOST_PROGRAM_OPTIONS_LIB@

if HAVE_GRIBAPI
  bin_PROGRAMS += fiIndexGribs fiGribCut
  fiIndexGribs_SOURCES = fiIndexGribs.cc
  fiIndexGribs_LDADD = ../libfimex.la @BOOST_PROGRAM_OPTIONS_LIB@
  fiGribCut_SOURCES = fiGribCut.cc
  fiGribCut_LDADD = ../libfimex.la @BOOST_PROGRAM_OPTIONS_LIB@ @BOOST_IOSTREAMS_LIB@
endif