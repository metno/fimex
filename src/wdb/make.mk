if HAVE_LIBPQ

libfimex_la_SOURCES += \
	wdb/WdbIndex.h wdb/WdbIndex.cpp \
	wdb/Wdb2CdmBuilder.h wdb/Wdb2CdmBuilder.cpp \
	wdb/DataHandler.h \
	wdb/TimeHandler.h wdb/TimeHandler.cpp \
	wdb/LevelHandler.h wdb/LevelHandler.cpp \
	wdb/VersionHandler.h wdb/VersionHandler.cpp \
	wdb/GridHandler.h wdb/GridHandler.cpp \
	wdb/ValueParameterHandler.h wdb/ValueParameterHandler.cpp
	

include wdb/gridInformation/make.mk
include wdb/database_access/make.mk
include wdb/config/make.mk
	
endif
