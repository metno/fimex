if HAVE_LIBPQ
 
  CC_TESTS += wdbTest
  
  wdbTest_SOURCES = \
  	wdb/Wdb2CdmBuilderTest.cc \
  	wdb/WdbIndexTest.cc \
  	wdb/WdbConfigurationTest.cpp \
  	wdb/GlobalWdbConfigurationTest.cpp 
  	
  wdbTest_CPPFLAGS = $(AM_CPPFLAGS) -I$(top_srcdir)/src/ -DTEST_DIR=\"@srcdir@/wdb\"
  
  wdbTest_LDFLAGS = $(AM_LDFLAGS) ../src/libfimex.la
  
  LDADD += @BOOST_PROGRAM_OPTIONS_LIB@
  
test_wdb: wdbTest
	@./wdbTest

endif
