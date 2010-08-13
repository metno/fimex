SUBDIRS = .

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include -DDATADIR="\"$(datadir)\"" ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${XML_CPPFLAGS}

LDADD = ../felt/libfelt.la ../libfimex.la @BOOST_PROGRAM_OPTIONS_LIB@


if HAVE_MILIB
  LDADD += ${milib_LIBS}
endif

bin_PROGRAMS = fimex
fimex_SOURCES = fimex.cc

if HAVE_GRIBAPI
  bin_PROGRAMS += fiIndexGribs fiGribCut
  fiIndexGribs_SOURCES = fiIndexGribs.cc
  fiGribCut_SOURCES = fiGribCut.cc
  fiGribCut_LDADD = @BOOST_IOSTREAMS_LIB@
endif