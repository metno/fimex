if HAVE_LIBPQ

libfimex_la_SOURCES += \
	wdb/WdbIndex.h wdb/WdbIndex.cpp \
	wdb/Wdb2CdmBuilder.h wdb/Wdb2CdmBuilder.cpp

include wdb/gridInformation/make.mk
include wdb/database_access/make.mk
include wdb/config/make.mk
	
endif
