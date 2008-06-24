SUBDIRS = .

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/include -I$../../include -DDATADIR="\"$(datadir)\"" ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${XML_CPPFLAGS} -Ifelt_reader

LDADD = ../felt_reader/libfimex_felt.a ../libfimex.a $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_PROGRAM_OPTIONS_LIB) -lm @XML_LIBS@


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