SUBDIRS = . felt_reader binSrc

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${UDUNITS_CPPFLAGS} ${PROJ4_CPPFLAGS} ${XML_CPPFLAGS}

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
					 CDMTimeInterpolator.cc \
					 CDMVariable.cc \
					 Data.cc \
					 DataTypeChanger.cc \
					 GribApiCDMWriter.cc \
					 GribApiCDMWriter_ImplAbstract.cc \
					 GribApiCDMWriter_Impl1.cc \
					 GribApiCDMWriter_Impl2.cc \
					 GribApiCDMWriter.cc \
					 Logger.cc \
					 NcmlCDMReader.cc \
					 Null_CDMWriter.cc \
					 ReplaceStringObject.cc \
					 ReplaceStringTimeObject.cc \
					 SpatialAxisSpec.cc \
					 TimeLevelDataSliceFetcher.cc \
					 TimeUnit.cc \
					 TimeSpec.cc \
					 Units.cc \
					 Utils.cc \
					 XMLDoc.cc

if HAVE_NETCDF
  libfimex_a_SOURCES += NetCDF_CF10_CDMReader.cc NetCDF_CDMWriter.cc NetCDF_Utils.cc
endif
