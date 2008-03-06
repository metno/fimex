SUBDIRS = . felt_reader

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I. ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${XML_CPPFLAGS} -Ifelt_reader

LDADD = felt_reader/libutplukk_felt.a libutplukk.a -lproj $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_PROGRAM_OPTIONS_LIB) -lm @XML_LIBS@


if HAVE_MILIB
  LDADD += ${milib_LIBS}
endif
if HAVE_UDUNITS
  LDADD += ${UDUNITS_LDFLAGS} ${UDUNITS_LIBS}
endif
if HAVE_NETCDF
  LDADD += ${NETCDF_LDFLAGS} ${NETCDF_LIBS}
endif

bin_PROGRAMS = utplukk
utplukk_SOURCES = utplukk.cc

lib_LIBRARIES = libutplukk.a
libutplukk_a_SOURCES = interpolation.c TimeException.cc Time.cc CachedInterpolation.cc CDM.cc CDMAttribute.cc CDMDimension.cc CDMDataType.cc CDMExtractor.cc CDMInterpolator.cc CDMVariable.cc Data.cc Utils.cc 
include_HEADERS = CachedInterpolation.h CDMAttribute.h CDMDataType.h CDMDimension.h CDMException.h CDMExtractor.h CDMInterpolator.h CDMReader.h CDMVariable.h CDMWriter.h CDM.h Data.h interpolation.h TimeException.h Time.h TimeSliceData.h Utils.h

if HAVE_NETCDF
  libutplukk_a_SOURCES += NetCDF_CDMWriter.cc
  include_HEADERS += NetCDF_CDMWriter.h
endif
