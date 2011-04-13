#
# Convenience library for wdb access. Will not be installed
# 
if HAVE_LIBPQ

noinst_LIBRARIES = libwdb.a

libwdb_a_CPPFLAGS = -I$(top_srcdir)/include/ ${XML_CPPFLAGS}

libwdb_a_SOURCES = \
	WdbConnection.h WdbConnection.cpp \
	DataSanitizer.h DataSanitizer.cpp \
	CdmNameTranslator.h CdmNameTranslator.cpp \
	GridData.h GridData.cpp \
	WdbIndex.h WdbIndex.cpp \
	Wdb2CdmBuilder.h Wdb2CdmBuilder.cpp \
	Parameter.h Parameter.cpp \
	Level.h Level.cpp \
	gridInformation/GridInformation.h gridInformation/GridInformation.cpp \
	gridInformation/LatLonGridInformation.h gridInformation/LatLonGridInformation.cpp \
	gridInformation/RotatedLatLonGridInformation.h gridInformation/RotatedLatLonGridInformation.cpp \
	gridInformation/MetricGridInformation.h gridInformation/MetricGridInformation.cpp
	
endif
