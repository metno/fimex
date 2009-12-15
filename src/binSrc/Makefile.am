SUBDIRS = .

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include -DDATADIR="\"$(datadir)\"" ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${XML_CPPFLAGS}

LDADD = ../felt/libfelt.la ../libfimex.la $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_IOSTREAMS_LIB) $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_DATE_TIME_LIB) -lm @XML_LIBS@


if HAVE_MILIB
  LDADD += ${milib_LIBS}
endif
if HAVE_UDUNITS
  LDADD += ${UDUNITS_LDFLAGS} ${UDUNITS_LIBS}
endif
if HAVE_PROJ4
  LDADD += ${PROJ4_LDFLAGS} ${PROJ4_LIBS}
endif
if HAVE_NETCDF
  LDADD += ${NETCDF_LDFLAGS} ${NETCDF_LIBS}
endif

bin_PROGRAMS = fimex
fimex_SOURCES = fimex.cc

if HAVE_GRIBAPI
  bin_PROGRAMS += fiIndexGribs fiGribCut
  fiIndexGribs_SOURCES = fiIndexGribs.cc
  fiGribCut_SOURCES = fiGribCut.cc
endif