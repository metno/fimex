SUBDIRS = felt . binSrc

AM_CFLAGS = 
AM_CXXFLAGS = $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include ${milib_CFLAGS} ${NETCDF_CPPFLAGS} @MIFI_UDUNITS_CPPFLAGS@ @MIFI_UDUNITS2_CPPFLAGS@ @MIFI_PROJ_CPPFLAGS@ @MIFI_NETCDF_CPPFLAGS@ @MIFI_GRIB_API_CPPFLAGS@ @XML_CPPFLAGS@ @BOOST_CPPFLAGS@


lib_LTLIBRARIES = libfimex.la
libfimex_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) @MIFI_UDUNITS2_LDFLAGS@ @MIFI_UDUNITS_LDFLAGS@ @MIFI_PROJ_LDFLAGS@ @MIFI_NETCDF_LDFLAGS@ @MIFI_GRIB_API_LDFLAGS@ ${XML_LDFLAGS} @BOOST_LDFLAGS@
libfimex_la_LIBADD = felt/libfelt.la @MIFI_UDUNITS_LIBS@ @MIFI_UDUNITS2_LIBS@ @MIFI_PROJ_LIBS@ @MIFI_NETCDF_LIBS@ @MIFI_NETCDF_CPP_LIBS@ @MIFI_GRIB_API_LIBS@ @XML_LIBS@ @BOOST_REGEX_LIB@ @BOOST_DATE_TIME_LIB@ @BOOST_FILESYSTEM_LIB@ @BOOST_SYSTEM_LIB@
libfimex_la_SOURCES = interpolation.c \
                     c_fimex.cc \
                     C_CDMReader.cc \
					 CachedInterpolation.cc \
					 CachedForwardInterpolation.cc \
					 CachedVectorReprojection.cc \
					 CDM.cc \
					 CDMAttribute.cc \
					 CDMconstants.cc \
					 CDMDimension.cc \
					 CDMDataType.cc \
					 CDMExtractor.cc \
                     CDMFileReaderFactory.cc \
					 CDMInterpolator.cc \
					 CDMNamedEntity.cc \
					 CDMQualityExtractor.cc \
					 CDMReader.cc \
					 CDMReaderUtils.cc \
					 CDMTimeInterpolator.cc \
					 CDMVariable.cc \
                     CDM_XMLConfigHelper.cc \
                     CDM_XMLConfigHelper.h \
                     CoordinateSystemSliceBuilder.cc \
                     coordSys/CoordinateSystem.cc \
                     coordSys/CoordinateAxis.cc \
                     coordSys/CoordSysImpl.h \
                     coordSys/Projection.cc \
                     coordSys/ProjectionImpl.cc \
                     coordSys/AlbersConicalEqualAreaProjection.cc \
                     coordSys/AzimuthalEquidistantProjection.cc \
                     coordSys/LambertAzimuthalEqualAreaProjection.cc \
                     coordSys/LambertConformalConicProjection.cc \
                     coordSys/LambertCylindricalEqualAreaProjection.cc \
                     coordSys/LatitudeLongitudeProjection.cc \
                     coordSys/MercatorProjection.cc \
                     coordSys/OrthographicProjection.cc \
                     coordSys/PolarStereographicProjection.cc \
                     coordSys/RotatedLatitudeLongitudeProjection.cc \
                     coordSys/StereographicProjection.cc \
                     coordSys/TransverseMercatorProjection.cc \
                     coordSys/VerticalPerspectiveProjection.cc \
					 Data.cc \
					 DataImpl.h \
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

if HAVE_LIBPQ
  libfimex_la_SOURCES += WdbCDMReader.cc
endif

include wdb/make.mk