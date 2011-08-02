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
  	
  wdbTest_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/src/ -DTEST_DIR=\"@srcdir@/wdb\" @MIFI_PQ_CPPFLAGS@
  
  wdbTest_LDFLAGS = $(AM_LDFLAGS) ../src/libfimex.la
  
  LDADD += @BOOST_PROGRAM_OPTIONS_LIB@
  
test_wdb: wdbTest
	@./wdbTest

endif
