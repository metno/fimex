TESTS = testInterpolation testFeltReader testTime testTimeSliceData testNetcdfWriter
LDADD = ../src/felt_reader/libutplukk_felt.a ../src/libutplukk.a -L/disk1/metno/local/lib -lmic -lmi -lg2c -L/usr/lib/netcdf-3 -lnetcdf_c++ -lnetcdf -ludunits -lproj $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_UNIT_TEST_FRAMEWORK_LIB) -lm @XML_LIBS@
#LIBS = `xml2-config --libs`
AM_CPPFLAGS = -I../src -I../src/felt_reader -I/disk1/metno/local/include/milib -I/usr/include/netcdf-3
AM_CXXFLAGS = -Wall -pedantic 
AM_CFLAGS = -std=c99 -Wall -pedantic
bin_PROGRAMS = $(TESTS)
testInterpolation_SOURCES = testInterpolation.cc 
testFeltReader_SOURCES = testFeltReader.cc
testTime_SOURCES = testTime.cc
testTimeSliceData_SOURCES = testTimeSliceData.cc
testNetcdfWriter_SOURCES = testNetcdfWriter.cc
