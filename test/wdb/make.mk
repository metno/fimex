if HAVE_LIBPQ

CC_TESTS += wdbTest

wdbTest_SOURCES = \
	wdb/TestingGridData.h wdb/TestingGridData.cpp \
	wdb/GridDataFactory.h wdb/GridDataFactory.cpp \
	wdb/Wdb2CdmBuilderTest.cc \
	wdb/WdbIndexTest.cc \
	wdb/ConfigurationTest.cpp \
	wdb/GlobalWdbConfigurationTest.cpp \
	wdb/TimeHandlerTest.cpp

wdbTest_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/src/ -DWDB_TEST_DIR=\"@srcdir@/wdb\" @MIFI_PQ_CPPFLAGS@
wdbTest_LDFLAGS = $(AM_LDFLAGS) ../src/libfimex.la
wdbTest_LDADD = $(LDADD) @BOOST_PROGRAM_OPTIONS_LIB@ @BOOST_DATE_TIME_LIB@ @BOOST_SYSTEM_LIB@

test_wdb: wdbTest
	@./wdbTest

endif

EXTRA_DIST += wdb/local.wdb.xml wdb/wdb_config.xml wdb/wdb.wdbml
