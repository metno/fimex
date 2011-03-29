#
# Convenience library for wdb access. Will not be installed
# 
if HAVE_LIBPQ

noinst_LIBRARIES = libwdb.a

libwdb_a_CPPFLAGS = -I$(top_srcdir)/include/ ${XML_CPPFLAGS}

libwdb_a_SOURCES = \
	WdbConnection.h WdbConnection.cpp \
	CdmNameTranslator.h CdmNameTranslator.cpp \
	GridData.h GridData.cpp \
	GridInformation.h GridInformation.cpp \
	DataIndex.h DataIndex.cpp \
	Parameter.h Parameter.cpp \
	Level.h Level.cpp
	
endif
