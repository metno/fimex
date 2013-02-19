EXTRA_DIST = $(SH_TESTS) inData.txt diana.setup felt2netcdf.cfg \
             felt2nc_variables_level1000.xml \
             test.ncml fillInterpolator.cfg testXMLDoc.xml coordTest.nc char.nc \
             testQualityConfig.xml testQEmask_data.nc testQEmask_mask.nc testQEmask.xml \
             coordRefTimeTest.nc test.ncml twoCoordsTest.nc \
             verticalPressure.nc verticalSigmaHybrid.nc verticalPressurePressure.nc \
             verticalSigmaHybridPressure.nc verticalPressureHeight.nc \
             verticalOceanSG2.nc verticalOceanSG2_depth.nc \
             sample_ed1.gm sample_ed2.gm \
             erai.sfc.40N.0.75d.200301011200.nc template_noaa17.nc \
             test_merge_inner.nc test_merge_outer.nc

CLEANFILES = test.nc test2.nc test3.nc test4.nc out.nc\
             testQualityExtractor.nc test_0time.nc \
             testInterpolator.nc testInterpolator2.nc testInterpolator3.nc \
             coordTestFilled.nc test.grb1 test.grb2 \
             bicubicOutData.txt bilinearOutData.txt \
             testMetgmReadEd1.nc testMetgmReadEd2.nc \
             hirlam12.mgm hirlam12.mgm.nc testGribRead.nc

CC_TESTS = testInterpolation \
           testVerticalCoordinates \
           testBinaryConstants \
           testUtils \
           testTimeSpec \
           testSpatialAxisSpec \
           testData \
           testProjections \
           testUnits \
           testFileReaderFactory \
           testCDM \
           testSliceBuilder \
           testXMLDoc \
           testFeltReader

SH_TESTS = 
LDADD = ../src/felt/libfelt.la ../src/libfimex.la $(BOOST_UNIT_TEST_FRAMEWORK_LIB)

AM_CPPFLAGS = -DTOP_SRCDIR=\"$(top_srcdir)\" -I$(top_srcdir)/include -I$(top_srcdir)/src \
              ${milib_CFLAGS} \
              @MIFI_UDUNITS_CPPFLAGS@ @MIFI_UDUNITS2_CPPFLAGS@ \
              @MIFI_PROJ_CPPFLAGS@ @XML_CPPFLAGS@ \
              @BOOST_CPPFLAGS@
AM_CXXFLAGS = 
AM_CFLAGS = 

if HAVE_FELT
  CC_TESTS += testQualityExtractor
  NETCDF_MI_SH_TESTS = testFeltReadNetCDFWrite.sh testNetCDFReadWrite.sh testNcmlRead.sh
  NETCDF_MI_TESTS = testNetcdfWriter testExtractor testInterpolator testTimeInterpolator test_c_consumer
  GRIBAPI_MI_TESTS = testGribWriter
endif
if HAVE_NETCDF
  CC_TESTS += ${NETCDF_MI_TESTS} testCoordSys \
              testCoordinateSystemSliceBuilder  testNcmlReader \
              testProcessor testMerger testNetCDFReaderWriter
  GRIBAPI_NC_TESTS = testGribReader
  METGM_NC_TESTS = testMetGmWriter 
  SH_TESTS += ${NETCDF_MI_SH_TESTS} testCharNc.sh \
              testInterpolatorFill.sh \
              testVerticalInterpolator.sh testVerticalInterpolator2.sh \
              testVerticalInterpolatorHeight.sh testVerticalInterpolatorOceanSG2ToDepth.sh \
              testQualityExtractorFimex.sh testMerger.sh
endif
if HAVE_GRIBAPI
  CC_TESTS += ${GRIBAPI_MI_TESTS} ${GRIBAPI_NC_TESTS}
endif
if HAVE_METGM
  CC_TESTS += testMetGmReader ${METGM_NC_TESTS}
endif
include wdb/make.mk


check_SCRIPTS = $(SH_TESTS)
TESTS = $(CC_TESTS) $(SH_TESTS)

check_PROGRAMS = $(CC_TESTS)
testBinaryConstants_SOURCES = testBinaryConstants.cc
testUtils_SOURCES = testUtils.cc
testInterpolation_SOURCES = testInterpolation.cc
testVerticalCoordinates_SOURCES = testVerticalCoordinates.cc
testSpatialAxisSpec_SOURCES = testSpatialAxisSpec.cc
testTimeSpec_SOURCES = testTimeSpec.cc
testData_SOURCES = testData.cc
testCDM_SOURCES = testCDM.cc
testSliceBuilder_SOURCES = testSliceBuilder.cc
testProjections_SOURCES = testProjections.cc
testCoordSys_SOURCES = testCoordSys.cc 
testCoordinateSystemSliceBuilder_SOURCES = testCoordinateSystemSliceBuilder.cc
testFeltReader_SOURCES = testFeltReader.cc
testNetcdfWriter_SOURCES = testNetcdfWriter.cc
testNetCDFReaderWriter_SOURCES = testNetCDFReaderWriter.cc
testNcmlReader_SOURCES = testNcmlReader.cc
testMetGmReader_SOURCES = testMetGmReader.cc
testMetGmWriter_SOURCES = testMetGmWriter.cc
testMerger_SOURCES = testMerger.cc
testGribReader_SOURCES = testGribReader.cc
testGribWriter_SOURCES = testGribWriter.cc
testFileReaderFactory_SOURCES = testFileReaderFactory.cc
testExtractor_SOURCES = testExtractor.cc
testProcessor_SOURCES = testProcessor.cc
testQualityExtractor_SOURCES = testQualityExtractor.cc
testInterpolator_SOURCES = testInterpolator.cc
testTimeInterpolator_SOURCES = testTimeInterpolator.cc
testUnits_SOURCES = testUnits.cc
testXMLDoc_SOURCES = testXMLDoc.cc
test_c_consumer_SOURCES = test_c_consumer.c
