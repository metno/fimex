CC_TESTS = testInterpolation testData testCDM testTime testTimeSliceData
SH_TESTS = 
LDADD = ../src/felt_reader/libutplukk_felt.a ../src/libutplukk.a -lproj $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_UNIT_TEST_FRAMEWORK_LIB) -lm @XML_LIBS@

AM_CPPFLAGS = -I../src -I../src/felt_reader ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${UDUNITS_CPPFLAGS}
AM_CXXFLAGS = -Wall -pedantic 
AM_CFLAGS = -std=c99 -Wall -pedantic

if HAVE_MILIB
  LDADD += ${milib_LIBS}
  CC_TESTS += testFeltReader
  NETCDF_MI_TESTS = testNetcdfWriter testExtractor testInterpolator
endif
if HAVE_UDUNITS
  LDADD += ${UDUNITS_LDFLAGS} ${UDUNITS_LIBS}
endif
if HAVE_NETCDF
  LDADD += ${NETCDF_LDFLAGS} ${NETCDF_LIBS}
  CC_TESTS += ${NETCDF_MI_TESTS}
  SH_TESTS += testNetCDFReadWrite.sh
endif

TESTS = $(CC_TESTS) $(SH_TESTS)

check_PROGRAMS = $(CC_TESTS)
testInterpolation_SOURCES = testInterpolation.cc
testData_SOURCES = testData.cc
testCDM_SOURCES = testCDM.cc 
testFeltReader_SOURCES = testFeltReader.cc
testTime_SOURCES = testTime.cc
testTimeSliceData_SOURCES = testTimeSliceData.cc
testNetcdfWriter_SOURCES = testNetcdfWriter.cc
testExtractor_SOURCES = testExtractor.cc
testInterpolator_SOURCES = testInterpolator.cc
