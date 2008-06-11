CLEANFILES = test.nc test2.nc test3.nc testInterpolator.nc testInterpolator2.nc
CC_TESTS = testInterpolation testData testCDM
SH_TESTS = 
LDADD = ../src/felt_reader/libfimex_felt.a ../src/libfimex.a -lproj $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_UNIT_TEST_FRAMEWORK_LIB) -lm @XML_LIBS@

AM_CPPFLAGS = -DTOP_SRCDIR=\"$(top_srcdir)\" -I$(top_srcdir)/src -I$(top_srcdir)/src/felt_reader ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${UDUNITS_CPPFLAGS}
AM_CXXFLAGS = -Wall -pedantic 
AM_CFLAGS = -std=c99 -Wall -pedantic

if HAVE_UDUNITS
  LDADD += ${UDUNITS_LDFLAGS} ${UDUNITS_LIBS}
  CC_TESTS += testUnits
endif
if HAVE_MILIB
  LDADD += ${milib_LIBS}
  CC_TESTS += testFeltReader
  NETCDF_MI_SH_TESTS = testFeltReadNetCDFWrite.sh testNetCDFReadWrite.sh
  NETCDF_MI_TESTS = testNetcdfWriter testExtractor testInterpolator
endif
if HAVE_NETCDF
  LDADD += ${NETCDF_LDFLAGS} ${NETCDF_LIBS}
  CC_TESTS += ${NETCDF_MI_TESTS}
  SH_TESTS += ${NETCDF_MI_SH_TESTS}
endif

EXTRA_DIST = $(SH_TESTS) inData.txt diana.setup felt2netcdf.cfg
check_SCRIPTS = $(SH_TESTS)
TESTS = $(CC_TESTS) $(SH_TESTS)

check_PROGRAMS = $(CC_TESTS)
testInterpolation_SOURCES = testInterpolation.cc
testData_SOURCES = testData.cc
testCDM_SOURCES = testCDM.cc 
testFeltReader_SOURCES = testFeltReader.cc
testNetcdfWriter_SOURCES = testNetcdfWriter.cc
testExtractor_SOURCES = testExtractor.cc
testInterpolator_SOURCES = testInterpolator.cc
testUnits_SOURCES = testUnits.cc
