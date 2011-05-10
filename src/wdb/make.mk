if HAVE_LIBPQ

libfimex_la_SOURCES += \
	wdb/WdbConnection.h wdb/WdbConnection.cpp \
	wdb/DataSanitizer.h wdb/DataSanitizer.cpp \
	wdb/CdmNameTranslator.h wdb/CdmNameTranslator.cpp \
	wdb/GridData.h wdb/GridData.cpp \
	wdb/WdbIndex.h wdb/WdbIndex.cpp \
	wdb/Wdb2CdmBuilder.h wdb/Wdb2CdmBuilder.cpp \
	wdb/Parameter.h wdb/Parameter.cpp \
	wdb/Level.h wdb/Level.cpp \
	wdb/WdbCDMReaderParser.h wdb/WdbCDMReaderParser.cc \
	wdb/WdbCDMReaderParserInfo.h wdb/WdbCDMReaderParserInfo.cpp \
	wdb/gridInformation/GridInformation.h wdb/gridInformation/GridInformation.cpp \
	wdb/gridInformation/LatLonGridInformation.h wdb/gridInformation/LatLonGridInformation.cpp \
	wdb/gridInformation/RotatedLatLonGridInformation.h wdb/gridInformation/RotatedLatLonGridInformation.cpp \
	wdb/gridInformation/MetricGridInformation.h wdb/gridInformation/MetricGridInformation.cpp
	
endif
