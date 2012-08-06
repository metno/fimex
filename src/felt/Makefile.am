AM_CFLAGS = 
AM_CXXFLAGS = $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include @MIFI_PROJ_CPPFLAGS@


lib_LTLIBRARIES = libfelt.la
libfelt_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) @BOOST_LDFLAGS@ @MIFI_PROJ_LDFLAGS@
libfelt_la_LIBADD = @BOOST_DATE_TIME_LIB@ @BOOST_FILESYSTEM_LIB@ @BOOST_IOSTREAMS_LIB@ @MIFI_PROJ_LIBS@
libfelt_la_SOURCES = FeltField.cpp FeltFile.cpp FeltGridDefinition.cpp FeltTypeConversion.cpp
