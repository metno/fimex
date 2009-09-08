SUBDIRS = felt felt_reader . binSrc

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${UDUNITS_CPPFLAGS} ${PROJ4_CPPFLAGS} ${XML_CPPFLAGS}

lib_LTLIBRARIES = libfimex.la
libfimex_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) felt_reader/libfimex_felt.la felt/libfelt.la $(BOOST_LDFLAGS) $(BOOST_REGEX_LIB) $(BOOST_FILESYSTEM_LIB) $(BOOST_PROGRAM_OPTIONS_LIB) $(BOOST_DATE_TIME_LIB) -lm @XML_LIBS@
# grib-api libs are automatically added to LDFLAGS
if HAVE_MILIB
  libfimex_la_LDFLAGS += ${milib_LIBS}
endif
if HAVE_UDUNITS
  libfimex_la_LDFLAGS += ${UDUNITS_LDFLAGS} ${UDUNITS_LIBS}
endif
if HAVE_PROJ4
  libfimex_la_LDFLAGS += ${PROJ4_LDFLAGS} ${PROJ4_LIBS}
endif
if HAVE_NETCDF
  libfimex_la_LDFLAGS += ${NETCDF_LDFLAGS} ${NETCDF_LIBS}
endif
libfimex_la_SOURCES = interpolation.c \
					 CachedInterpolation.cc \
					 CachedForwardInterpolation.cc \
					 CachedVectorReprojection.cc \
					 CDM.cc \
					 CDMAttribute.cc \
					 CDMDimension.cc \
					 CDMDataType.cc \
					 CDMExtractor.cc \
					 CDMInterpolator.cc \
					 CDMNamedEntity.cc \
					 CDMQualityExtractor.cc \
					 CDMReader.cc \
					 CDMTimeInterpolator.cc \
					 CDMVariable.cc \
					 Data.cc \
					 DataTypeChanger.cc \
					 GribApiCDMWriter.cc \
					 GribApiCDMWriter_ImplAbstract.cc \
					 GribApiCDMWriter_Impl1.cc \
					 GribApiCDMWriter_Impl2.cc \
					 GribFileIndex.cc \
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
  libfimex_la_SOURCES += NetCDF_CF10_CDMReader.cc NetCDF_CDMWriter.cc NetCDF_Utils.cc
endif
