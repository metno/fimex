SUBDIRS = . felt_reader binSrc

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I. ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${XML_CPPFLAGS}

lib_LIBRARIES = libfimex.a
libfimex_a_SOURCES = interpolation.c \
					 CachedInterpolation.cc \
					 CachedVectorReprojection.cc \
					 CDM.cc \
					 CDMAttribute.cc \
					 CDMDimension.cc \
					 CDMDataType.cc \
					 CDMExtractor.cc \
					 CDMInterpolator.cc \
					 CDMNamedEntity.cc \
					 CDMReader.cc \
					 CDMVariable.cc \
					 Data.cc \
					 DataTypeChanger.cc \
					 Null_CDMWriter.cc \
					 ReplaceStringObject.cc \
					 ReplaceStringTimeObject.cc \
					 Utils.cc \
					 XMLDoc.cc
include_HEADERS = config.h \
	              CachedInterpolation.h \
				  CachedVectorReprojection.h \
	              CDMAttribute.h \
	              CDMDataType.h \
	              CDMDimension.h \
	              CDMException.h \
	              CDMExtractor.h \
	              CDMInterpolator.h \
  			      CDMNamedEntity.h \
	              CDMReader.h \
	              CDMVariable.h \
	              CDMWriter.h CDM.h \
	              Data.h \
	              DataImpl.h \
 				  DataTypeChanger.h \
	              interpolation.h \
	              Null_CDMWriter.h \
	              ReplaceStringObject.h \
	              ReplaceStringTimeObject.h \
	              Utils.h \
	              XMLDoc.h

if HAVE_NETCDF
  libfimex_a_SOURCES += NetCDF_CF10_CDMReader.cc NetCDF_CDMWriter.cc NetCDF_Utils.cc
  include_HEADERS += NetCDF_CF10_CDMReader.h NetCDF_CDMWriter.h NetCDF_Utils.h
endif
