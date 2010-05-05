SUBDIRS = felt . binSrc

AM_CFLAGS = -std=c99 -Wall -pedantic
AM_CXXFLAGS = -Wall $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include ${milib_CFLAGS} ${NETCDF_CPPFLAGS} ${UDUNITS_CPPFLAGS} ${PROJ4_CPPFLAGS} ${XML_CPPFLAGS}

lib_LTLIBRARIES = libfimex.la
libfimex_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) felt/libfelt.la
libfimex_la_SOURCES = interpolation.c \
                     c_fimex.cc \
                     C_CDMReader.cc \
					 CachedInterpolation.cc \
					 CachedForwardInterpolation.cc \
					 CachedVectorReprojection.cc \
					 CDM.cc \
					 CDMAttribute.cc \
					 CDMconstants.c \
					 CDMDimension.cc \
					 CDMDataType.cc \
					 CDMExtractor.cc \
                     CDMFileReaderFactory.cc \
					 CDMInterpolator.cc \
					 CDMNamedEntity.cc \
					 CDMQualityExtractor.cc \
					 CDMReader.cc \
					 CDMTimeInterpolator.cc \
					 CDMVariable.cc \
                     CDM_XMLConfigHelper.cc \
                     CDM_XMLConfigHelper.h \
                     coordSys/CoordinateSystem.cc \
                     coordSys/CoordinateAxis.cc \
                     coordSys/CoordSysImpl.h \
                     coordSys/Projection.cc \
                     coordSys/ProjectionImpl.cc \
                     coordSys/LambertConformalConicProjection.cc \
                     coordSys/LatitudeLongitudeProjection.cc \
                     coordSys/MercatorProjection.cc \
                     coordSys/RotatedLatitudeLongitudeProjection.cc \
                     coordSys/StereographicProjection.cc \
                     coordSys/PolarStereographicProjection.cc \
					 Data.cc \
					 DataTypeChanger.cc \
					 Felt_Array2.cc \
                     FeltCDMReader2.cc \
					 Felt_File2.cc \
					 FeltParameters.cc \
                     GridDefinition.cc \
					 Logger.cc \
					 NcmlCDMReader.cc \
					 Null_CDMWriter.cc \
					 ReplaceStringObject.cc \
					 ReplaceStringTimeObject.cc \
					 SpatialAxisSpec.cc \
                     SliceBuilder.cc \
					 TimeLevelDataSliceFetcher.cc \
					 TimeUnit.cc \
					 TimeSpec.cc \
					 Units.cc \
					 Utils.cc \
					 XMLDoc.cc

if HAVE_NETCDF
  libfimex_la_SOURCES += NetCDF_CDMReader.cc \
                         NetCDF_CDMWriter.cc \
                         NetCDF_Utils.cc \
                         NetCDF_Utils.h
endif

if HAVE_GRIBAPI
  libfimex_la_SOURCES += GribApiCDMWriter.cc \
                         GribApiCDMWriter_ImplAbstract.cc \
                         GribApiCDMWriter_Impl1.cc \
                         GribApiCDMWriter_Impl2.cc \
                         GribCDMReader.cc \
                         GribFileIndex.cc \
                         GribUtils.cc 
endif

if HAVE_MILIB
  libfimex_la_SOURCES += FeltCDMReader.cc Felt_Array.cc Felt_File.cc
endif