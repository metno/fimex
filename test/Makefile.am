CLEANFILES = test.nc test2.nc test3.nc test4.nc testQualityExtractor.nc testInterpolator.nc testInterpolator2.nc testInterpolator3.nc coordTestFilled.nc test.grb1 test.grb2 bicubicOutData.txt bilinearOutData.txt
CC_TESTS = testBinaryConstants \
           testUtils \
           testTimeSpec \
           testSpatialAxisSpec \
           testData \
           testProjections \
           testInterpolation \
           testUnits \
           testFileReaderFactory \
           testCDM \
           testSliceBuilder \
           testXMLDoc \
           testFeltReader

SH_TESTS = 
LDADD = ../src/felt/libfelt.la ../src/libfimex.la $(BOOST_UNIT_TEST_FRAMEWORK_LIB)

AM_CPPFLAGS = -DTOP_SRCDIR=\"$(top_srcdir)\" -I$(top_srcdir)/include -I../include ${milib_CFLAGS} ${NETCDF_CPPFLAGS} @MIFI_UDUNITS_CPPFLAGS@ @MIFI_UDUNITS2_CPPFLAGS@ @MIFI_PROJ_CPPFLAGS@ @MIFI_NETCDF_CPPFLAGS@ @XML_CPPFLAGS@ @BOOST_CPPFLAGS@
AM_CXXFLAGS = 
AM_CFLAGS = 

if HAVE_MILIB
  LDADD += ${milib_LIBS}
  CC_TESTS += 
endif
if HAVE_FELT_OR_MILIB 
  CC_TESTS += testQualityExtractor
  NETCDF_MI_SH_TESTS = testFeltReadNetCDFWrite.sh testNetCDFReadWrite.sh testNcmlRead.sh
  NETCDF_MI_TESTS = testNetcdfWriter testExtractor testInterpolator testTimeInterpolator test_c_consumer
  GRIBAPI_MI_TESTS = testGribWriter
endif
if HAVE_NETCDF
  CC_TESTS += ${NETCDF_MI_TESTS} testCoordSys
  GRIBAPI_NC_TESTS = testGribReader
  SH_TESTS += ${NETCDF_MI_SH_TESTS} testInterpolatorFill.sh
endif
if HAVE_GRIBAPI
  CC_TESTS += ${GRIBAPI_MI_TESTS} ${GRIBAPI_NC_TESTS}
endif

include wdb/make.mk


EXTRA_DIST = $(SH_TESTS) inData.txt diana.setup felt2netcdf.cfg testXMLDoc.xml coordTest.nc wdbreadercfg.xml
check_SCRIPTS = $(SH_TESTS)
TESTS = $(CC_TESTS) $(SH_TESTS)

check_PROGRAMS = $(CC_TESTS)
testBinaryConstants_SOURCES = testBinaryConstants.cc
testUtils_SOURCES = testUtils.cc
testInterpolation_SOURCES = testInterpolation.cc
testSpatialAxisSpec_SOURCES = testSpatialAxisSpec.cc
testTimeSpec_SOURCES = testTimeSpec.cc
testData_SOURCES = testData.cc
testCDM_SOURCES = testCDM.cc
testSliceBuilder_SOURCES = testSliceBuilder.cc
testProjections_SOURCES = testProjections.cc
testCoordSys_SOURCES = testCoordSys.cc 
testFeltReader_SOURCES = testFeltReader.cc
testNetcdfWriter_SOURCES = testNetcdfWriter.cc
testGribReader_SOURCES = testGribReader.cc
testGribWriter_SOURCES = testGribWriter.cc
testFileReaderFactory_SOURCES = testFileReaderFactory.cc
testExtractor_SOURCES = testExtractor.cc
testQualityExtractor_SOURCES = testQualityExtractor.cc
testInterpolator_SOURCES = testInterpolator.cc
testTimeInterpolator_SOURCES = testTimeInterpolator.cc
testUnits_SOURCES = testUnits.cc
testXMLDoc_SOURCES = testXMLDoc.cc
test_c_consumer_SOURCES = test_c_consumer.c
