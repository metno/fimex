if HAVE_LIBPQ

libfimex_la_SOURCES += \
	wdb/CdmNameTranslator.h wdb/CdmNameTranslator.cpp \
	wdb/WdbIndex.h wdb/WdbIndex.cpp \
	wdb/Wdb2CdmBuilder.h wdb/Wdb2CdmBuilder.cpp \
	wdb/WdbCDMReaderParser.h wdb/WdbCDMReaderParser.cc \
	wdb/WdbCDMReaderParserInfo.h wdb/WdbCDMReaderParserInfo.cpp

include wdb/gridInformation/make.mk
include wdb/database_access/make.mk
	
endif
