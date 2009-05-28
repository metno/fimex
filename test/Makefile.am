CLEANFILES = test.nc test2.nc test3.nc test4.nc testQualityExtractor.nc testInterpolator.nc testInterpolator2.nc test.grb1 test.grb2 bicubicOutData.txt bilinearOutData.txt
CC_TESTS = testTimeSpec testSpatialAxisSpec testInterpolation testData testCDM testXMLDoc testQualityExtractor
SH_TESTS = 
LDADD = ../src/felt_reader/libfimex_felt.a ../src/libfimex.a $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_UNIT_TEST_FRAMEWORK_LIB) -lm @XML_LIBS@

AM_CPPFLAGS = -DTOP_SRCDIR=\"$(top_srcdir)\" -I$(top_srcdir)/include -I../include ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${UDUNITS_CPPFLAGS} ${PROJ4_CPPFLAGS} ${XML_CPPFLAGS}
AM_CXXFLAGS = -Wall
AM_CFLAGS = -std=c99 -Wall

if HAVE_UDUNITS
  LDADD += ${UDUNITS_LDFLAGS} ${UDUNITS_LIBS}
  CC_TESTS += testUnits
endif
if HAVE_PROJ4
  LDADD += ${PROJ4_LDFLAGS} ${PROJ4_LIBS}
endif
if HAVE_MILIB
  LDADD += ${milib_LIBS}
  CC_TESTS += testFeltReader testQualityExtractor
  NETCDF_MI_SH_TESTS = testFeltReadNetCDFWrite.sh testNetCDFReadWrite.sh
  NETCDF_MI_TESTS = testNetcdfWriter testExtractor testInterpolator testTimeInterpolator
  GRIBAPI_MI_TESTS = testGribWriter
endif
if HAVE_NETCDF
  LDADD += ${NETCDF_LDFLAGS} ${NETCDF_LIBS}
  CC_TESTS += ${NETCDF_MI_TESTS}
  SH_TESTS += ${NETCDF_MI_SH_TESTS}
endif
if HAVE_GRIBAPI
  CC_TESTS += ${GRIBAPI_MI_TESTS}
endif

EXTRA_DIST = $(SH_TESTS) inData.txt diana.setup felt2netcdf.cfg
check_SCRIPTS = $(SH_TESTS)
TESTS = $(CC_TESTS) $(SH_TESTS)

check_PROGRAMS = $(CC_TESTS)
testInterpolation_SOURCES = testInterpolation.cc
testSpatialAxisSpec_SOURCES = testSpatialAxisSpec.cc
testTimeSpec_SOURCES = testTimeSpec.cc
testData_SOURCES = testData.cc
testCDM_SOURCES = testCDM.cc 
testFeltReader_SOURCES = testFeltReader.cc
testNetcdfWriter_SOURCES = testNetcdfWriter.cc
testGribWriter_SOURCES = testGribWriter.cc
testExtractor_SOURCES = testExtractor.cc
testQualityExtractor_SOURCES = testQualityExtractor.cc
testInterpolator_SOURCES = testInterpolator.cc
testTimeInterpolator_SOURCES = testTimeInterpolator.cc
testUnits_SOURCES = testUnits.cc
testXMLDoc_SOURCES = testXMLDoc.cc
