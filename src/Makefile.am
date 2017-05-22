SUBDIRS = felt . binSrc

AM_CFLAGS = 
AM_CXXFLAGS = $(BOOST_CPPFLAGS)
AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include @MIFI_NETCDF_CPPFLAGS@ @MIFI_UDUNITS_CPPFLAGS@ @MIFI_UDUNITS2_CPPFLAGS@ @MIFI_PROJ_CPPFLAGS@ @MIFI_GRIB_API_CPPFLAGS@ @MIFI_PQ_CPPFLAGS@ @MIFI_METGM_CPPFLAGS@ @proradxml_CFLAGS@ @XML_CPPFLAGS@ @BOOST_CPPFLAGS@ @LOG4CPP_CFLAGS@


lib_LTLIBRARIES = libfimex.la
libfimex_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) @MIFI_UDUNITS2_LDFLAGS@ @MIFI_UDUNITS_LDFLAGS@ @MIFI_PROJ_LDFLAGS@ @MIFI_NETCDF_LDFLAGS@ @MIFI_GRIB_API_LDFLAGS@ @MIFI_PQ_LDFLAGS@ @MIFI_METGM_LDFLAGS@ @proradxml_LIBS@ ${XML_LDFLAGS} @BOOST_LDFLAGS@
libfimex_la_LIBADD = felt/libfelt.la @MIFI_UDUNITS_LIBS@ @MIFI_UDUNITS2_LIBS@ @MIFI_PROJ_LIBS@ @MIFI_NETCDF_LIBS@ @MIFI_GRIB_API_LIBS@ @MIFI_PQ_LIBS@ @MIFI_METGM_LIBS@ @XML_LIBS@ @BOOST_REGEX_LIB@ @BOOST_DATE_TIME_LIB@ @BOOST_FILESYSTEM_LIB@ @BOOST_SYSTEM_LIB@ @LOG4CPP_LIBS@
libfimex_la_SOURCES = interpolation.c \
                     c_fimex.cc \
                     C_CDMReader.cc \
					 CachedInterpolation.cc \
                     CachedForwardInterpolation.h \
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
                     CDMMergeUtils.h \
                     CDMMergeUtils.cc \
                     CDMBorderSmoothing.cc \
                     CDMBorderSmoothing_Linear.cc \
                     CDMOverlay.cc \
                     CDMMerger.cc \
					 CDMPressureConversions.cc \
					 CDMProcessor.cc \
					 CDMQualityExtractor.cc \
					 CDMReader.cc \
					 CDMReaderUtils.cc \
					 CDMReaderWriter.cc \
					 CDMTimeInterpolator.cc \
					 CDMVariable.cc \
					 CDMVerticalInterpolator.cc \
                     CDM_XMLConfigHelper.cc \
                     CDM_XMLConfigHelper.h \
                     convert/CDMReaderVar.h \
                     convert/Converter.cc \
                     convert/Converter.h \
                     convert/ConverterSrc.h \
                     convert/ConverterSrc.cc \
                     convert/ConverterImpl.h \
                     convert/ConverterImpl.cc \
                     convert/RelativeToSpecificHumidityConverter.h \
                     convert/RelativeToSpecificHumidityConverter.cc \
                     CoordinateSystemSliceBuilder.cc \
                     coordSys/CoordinateSystem.cc \
                     coordSys/CoordinateAxis.cc \
                     coordSys/CoordSysBuilder.h \
                     coordSys/CoordSysUtils.cc \
                     coordSys/CF1_xCoordSysBuilder.h \
                     coordSys/CF1_xCoordSysBuilder.cc \
                     coordSys/WRFCoordSysBuilder.h \
                     coordSys/WRFCoordSysBuilder.cc \
                     coordSys/CoordSysImpl.h \
                     coordSys/CoordSysUtils.h \
                     coordSys/Projection.cc \
                     coordSys/ProjectionImpl.cc \
                     coordSys/AlbersConicalEqualAreaProjection.cc \
                     coordSys/AzimuthalEquidistantProjection.cc \
                     coordSys/GeostationaryProjection.cc \
                     coordSys/LambertAzimuthalEqualAreaProjection.cc \
                     coordSys/LambertConformalConicProjection.cc \
                     coordSys/LambertCylindricalEqualAreaProjection.cc \
                     coordSys/LatitudeLongitudeProjection.cc \
                     coordSys/MercatorProjection.cc \
                     coordSys/ObliqueMercatorProjection.cc \
                     coordSys/OrthographicProjection.cc \
                     coordSys/PolarStereographicProjection.cc \
                     coordSys/RotatedLatitudeLongitudeProjection.cc \
                     coordSys/StereographicProjection.cc \
                     coordSys/TransverseMercatorProjection.cc \
                     coordSys/VerticalPerspectiveProjection.cc \
                     coordSys/UnknownToFgdcProjection.cc \
                     coordSys/verticalTransform/AltitudeHeightConverter.cc \
                     coordSys/verticalTransform/AltitudeStandardToPressureConverter.cc \
                     coordSys/verticalTransform/AtmosphereSigma.cc \
                     coordSys/verticalTransform/GeopotentialToAltitudeConverter.cc \
                     coordSys/verticalTransform/Height.cc \
                     coordSys/verticalTransform/HybridSigmaApToPressureConverter.cc \
                     coordSys/verticalTransform/HybridSigmaPressure1.cc \
                     coordSys/verticalTransform/HybridSigmaPressure2.cc \
                     coordSys/verticalTransform/HybridSigmaToPressureConverter.cc \
                     coordSys/verticalTransform/IdentityConverter.cc \
                     coordSys/verticalTransform/LnPressure.cc \
                     coordSys/verticalTransform/LnPressureToPressureConverter.cc \
                     coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.cc \
                     coordSys/verticalTransform/OceanSG1.cc \
                     coordSys/verticalTransform/OceanSG2.cc \
                     coordSys/verticalTransform/Pressure.cc \
                     coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.cc \
                     coordSys/verticalTransform/PressureToStandardAltitudeConverter.cc \
                     coordSys/verticalTransform/SigmaToPressureConverter.cc \
                     coordSys/verticalTransform/ToVLevelConverter.cc \
                     coordSys/verticalTransform/ToVLevelConverterAdapter.cc \
                     coordSys/verticalTransform/VerticalConverter.cc \
                     coordSys/verticalTransform/VerticalTransformation.cc \
                     coordSys/verticalTransform/VerticalTransformationUtils.cc \
					 Data.cc \
					 DataImpl.h \
                     DataIndex.cc \
                     leap_iterator.h \
                     ArrayLoop.h \
                     ArrayLoop.cc \
					 DataTypeChanger.cc \
					 Felt_Array2.h \
					 Felt_Array2.cc \
					 FeltCDMReader2.h \
                     FeltCDMReader2.cc \
                     Felt_File_Error.h \
                     Felt_File2.h \
					 Felt_File2.cc \
					 FeltParameters.h \
					 FeltParameters.cc \
					 FillWriter.cc \
                     GridDefinition.cc \
                     IndexedData.cc \
					 Logger.cc \
					 Log4cppLogger.cc \
					 Log4cppLogger.h \
					 MutexLock.h \
					 mifi_mpi.c \
					 NcmlCDMReader.cc \
					 NcmlAggregationReader.h \
					 NcmlAggregationReader.cc \
					 Null_CDMWriter.cc \
					 ReplaceStringObject.cc \
					 ReplaceStringTimeObject.cc \
					 SpatialAxisSpec.cc \
                     SliceBuilder.cc \
                     ThreadPool.c \
					 TimeUnit.cc \
					 TimeSpec.cc \
					 Units.cc \
					 Utils.cc \
					 vertical_coordinate_transformations.c \
					 XMLDoc.cc \
					 XMLInput.cc


if HAVE_NETCDF
  libfimex_la_SOURCES += NetCDF_CDMReader.cc \
                         NetCDF_CDMWriter.cc \
                         NetCDF_Utils.cc \
                         NetCDF_Utils.h
endif

if HAVE_GRIBAPI
  libfimex_la_SOURCES += GribApiCDMWriter.cc \
                         GribApiCDMWriter_ImplAbstract.h \
                         GribApiCDMWriter_ImplAbstract.cc \
                         GribApiCDMWriter_Impl1.h \
                         GribApiCDMWriter_Impl1.cc \
                         GribApiCDMWriter_Impl2.cc \
                         GribApiCDMWriter_Impl2.h \
                         GribCDMReader.cc \
                         GribFileIndex.cc \
                         GribUtils.cc 
endif

if HAVE_METGM
  libfimex_la_SOURCES += MetGmCDMReader.cc \
                         metgm/MetGmCDMReaderImpl.cc \
                         metgm/MetGmCDMReaderSlicedImpl.cc \
                         MetGmCDMWriter.cc \
                         metgm/MetGmCDMWriterImpl.cc \
                         metgm/MetGmCDMWriterSlicedImpl.cc \
                         metgm/MetGmHorizontalTag.cc \
                         metgm/MetGmTimeTag.cc \
                         metgm/MetGmVerticalTag.cc \
                         metgm/MetGmGroup1Ptr.cc \
                         metgm/MetGmGroup2Ptr.cc \
                         metgm/MetGmGroup3Ptr.cc \
                         metgm/MetGmGroup5Ptr.cc \
                         metgm/MetGmCommentAttributeParser.cc \
                         metgm/MetGmDimensionsTag.cc \
                         metgm/MetGmTags.cc metgm/MetGmHandlePtr.cc \
                         metgm/MetGmGroup5Ptr.h \
                         metgm/MetGmConfigurationMappings.h \
                         metgm/MetGmVersion.h \
                         metgm/MetGmGroup2Ptr.h \
                         metgm/MetGmUtils.h \
                         metgm/MetGmTags.h \
                         metgm/MetGmCDMWriterImpl.h \
                         metgm/MetGmCDMWriterSlicedImpl.h \
                         metgm/MetGmCommentAttributeParser.h \
                         metgm/MetGmGroup1Ptr.h \
                         metgm/MetGmGroup3Ptr.h \
                         metgm/MetGmCDMReaderImpl.h \
                         metgm/MetGmCDMReaderSlicedImpl.h \
                         metgm/MetGmVerticalTag.h \
                         metgm/MetGmHorizontalTag.h \
                         metgm/MetGmHandlePtr.h \
                         metgm/MetGmTimeTag.h \
                         metgm/MetGmCDMVariableProfile.h \
                         metgm/MetGmFileHandlePtr.h \
                         metgm/MetGmDimensionsTag.h
endif

if HAVE_PRORADXML
  libfimex_la_SOURCES +=  ProradXMLCDMReader.cc \
	   				      ProradXMLCDMReader.h
endif

include wdb/make.mk
