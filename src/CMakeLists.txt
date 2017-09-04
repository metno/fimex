
SET(libfimex_INCLUDE_DIRECTORIES
  "${CMAKE_SOURCE_DIR}/include"
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${PC_INCLUDE_DIRS}
  ${udunits2_INC_DIR}
  ${proj_INC_DIR}
  ${grib_api_INC_DIR}
  ${ECCODES_INCLUDE_DIRS}
  ${Boost_INCLUDE_DIRS}
)
SET(libfimex_COMPILE_DEFINITIONS
  ${PC_CFLAGS_OTHER}
  -DPKGDATADIR="${DATADIR}/fimex${MINUS_FIMEX_VERSION}"
  )
LINK_DIRECTORIES(${PC_LIBRARY_DIRS} ${Boost_LIBRARY_DIRS})

SET(libfimex_SOURCES
  c_fimex.cc
  C_CDMReader.cc
  CachedInterpolation.cc
  CachedForwardInterpolation.cc
  CachedVectorReprojection.cc
  CDM.cc
  CDMAttribute.cc
  CDMconstants.cc
  CDMDimension.cc
  CDMDataType.cc
  CDMExtractor.cc
  CDMFileReaderFactory.cc
  CDMInterpolator.cc
  CDMNamedEntity.cc
  CDMMergeUtils.cc
  CDMBorderSmoothing.cc
  CDMBorderSmoothing_Linear.cc
  CDMOverlay.cc
  CDMMerger.cc
  CDMPressureConversions.cc
  CDMProcessor.cc
  CDMQualityExtractor.cc
  CDMReader.cc
  CDMReaderUtils.cc
  CDMReaderWriter.cc
  CDMTimeInterpolator.cc
  CDMVariable.cc
  CDMVerticalInterpolator.cc
  CDM_XMLConfigHelper.cc
  CoordinateSystemSliceBuilder.cc
  Data.cc
  DataIndex.cc
  ArrayLoop.cc
  DataTypeChanger.cc
  FillWriter.cc
  GridDefinition.cc
  IndexedData.cc
  Logger.cc
  Log4cppLogger.cc
  MutexLock.h
  NcmlCDMReader.cc
  NcmlAggregationReader.cc
  Null_CDMWriter.cc
  ReplaceStringObject.cc
  ReplaceStringTimeObject.cc
  SpatialAxisSpec.cc
  SliceBuilder.cc
  TimeUnit.cc
  TimeSpec.cc
  Units.cc
  Utils.cc
  XMLDoc.cc
  XMLInput.cc

  coordSys/CoordinateSystem.cc
  coordSys/CoordinateAxis.cc
  coordSys/CoordSysUtils.cc
  coordSys/CF1_xCoordSysBuilder.cc
  coordSys/WRFCoordSysBuilder.cc
  coordSys/Projection.cc
  coordSys/ProjectionImpl.cc
  coordSys/AlbersConicalEqualAreaProjection.cc
  coordSys/AzimuthalEquidistantProjection.cc
  coordSys/GeostationaryProjection.cc
  coordSys/LambertAzimuthalEqualAreaProjection.cc
  coordSys/LambertConformalConicProjection.cc
  coordSys/LambertCylindricalEqualAreaProjection.cc
  coordSys/LatitudeLongitudeProjection.cc
  coordSys/MercatorProjection.cc
  coordSys/ObliqueMercatorProjection.cc
  coordSys/OrthographicProjection.cc
  coordSys/PolarStereographicProjection.cc
  coordSys/RotatedLatitudeLongitudeProjection.cc
  coordSys/StereographicProjection.cc
  coordSys/TransverseMercatorProjection.cc
  coordSys/VerticalPerspectiveProjection.cc
  coordSys/UnknownToFgdcProjection.cc

  coordSys/verticalTransform/AltitudeHeightConverter.cc
  coordSys/verticalTransform/AltitudeStandardToPressureConverter.cc
  coordSys/verticalTransform/AtmosphereSigma.cc
  coordSys/verticalTransform/GeopotentialToAltitudeConverter.cc
  coordSys/verticalTransform/Height.cc
  coordSys/verticalTransform/HybridSigmaApToPressureConverter.cc
  coordSys/verticalTransform/HybridSigmaPressure1.cc
  coordSys/verticalTransform/HybridSigmaPressure2.cc
  coordSys/verticalTransform/HybridSigmaToPressureConverter.cc
  coordSys/verticalTransform/IdentityConverter.cc
  coordSys/verticalTransform/LnPressure.cc
  coordSys/verticalTransform/LnPressureToPressureConverter.cc
  coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.cc
  coordSys/verticalTransform/OceanSG1.cc
  coordSys/verticalTransform/OceanSG2.cc
  coordSys/verticalTransform/Pressure.cc
  coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.cc
  coordSys/verticalTransform/PressureToStandardAltitudeConverter.cc
  coordSys/verticalTransform/SigmaToPressureConverter.cc
  coordSys/verticalTransform/ToVLevelConverter.cc
  coordSys/verticalTransform/ToVLevelConverterAdapter.cc
  coordSys/verticalTransform/VerticalConverter.cc
  coordSys/verticalTransform/VerticalTransformation.cc
  coordSys/verticalTransform/VerticalTransformationUtils.cc

  interpolation.c
  mifi_mpi.c
  ThreadPool.c
  vertical_coordinate_transformations.c

  CachedForwardInterpolation.h
  CDMMergeUtils.h
  CDM_XMLConfigHelper.h
  DataImpl.h
  leap_iterator.h
  Log4cppLogger.h
  MutexLock.h
  NcmlAggregationReader.h

  coordSys/CF1_xCoordSysBuilder.h
  coordSys/CoordSysBuilder.h
  coordSys/CoordSysImpl.h
  coordSys/CoordSysUtils.h
  coordSys/WRFCoordSysBuilder.h
)

# not functional yet
## LIST(APPEND libfimex_SOURCES
##   convert/Converter.cc
##   convert/ConverterSrc.cc
##   convert/ConverterImpl.cc
##   convert/RelativeToSpecificHumidityConverter.cc
##   convert/CDMReaderVar.h
##   convert/Converter.h
##   convert/ConverterSrc.h
##   convert/ConverterImpl.h
##   convert/RelativeToSpecificHumidityConverter.h
##   )

IF(ENABLE_LOG4CPP)
  SET(HAVE_LOG4CPP 1)
ENDIF()

IF(MPI_CXX_FOUND)
  SET(HAVE_MPI 1)
ENDIF()

IF(ENABLE_NETCDF)
  SET(HAVE_NETCDF_H 1)

  SET(libfimex_netcdf_SOURCES
    NetCDF_CDMReader.cc
    NetCDF_CDMWriter.cc
    NetCDF_Utils.cc

    NetCDF_Utils.h
  )
ENDIF(ENABLE_NETCDF)

IF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))
  SET(HAVE_GRIB_API_H 1)

  IF(ENABLE_GRIBAPI)
    INCLUDE_DIRECTORIES(${GRIBAPI_INC_DIR})
  ENDIF()

  SET(libfimex_grib_SOURCES
    GribApiCDMWriter.cc
    GribApiCDMWriter_ImplAbstract.cc
    GribApiCDMWriter_Impl1.cc
    GribApiCDMWriter_Impl2.cc
    GribCDMReader.cc
    GribFileIndex.cc
    GribUtils.cc

    GribApiCDMWriter_ImplAbstract.h
    GribApiCDMWriter_Impl1.h
    GribApiCDMWriter_Impl2.h
    )
ENDIF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))

IF(ENABLE_FELT)
  ADD_SUBDIRECTORY(felt)
  SET(FELT_LIBRARY libfelt)

  SET(HAVE_FELT 1)

  SET(libfimex_felt_SOURCES
    Felt_Array2.cc
    FeltCDMReader2.cc
    Felt_File2.cc
    FeltParameters.cc

    Felt_Array2.h
    FeltCDMReader2.h
    Felt_File_Error.h
    Felt_File2.h
    FeltParameters.h
    )

ENDIF(ENABLE_FELT)

if(ENABLE_METGM)
  SET(HAVE_METGM_H 1)

  SET(libfimex_metgm_SOURCES
    MetGmCDMWriter.cc
    MetGmCDMReader.cc

    metgm/MetGmCDMReaderImpl.cc
    metgm/MetGmCDMReaderSlicedImpl.cc
    metgm/MetGmCDMWriterImpl.cc
    metgm/MetGmCDMWriterSlicedImpl.cc
    metgm/MetGmHorizontalTag.cc
    metgm/MetGmTimeTag.cc
    metgm/MetGmVerticalTag.cc
    metgm/MetGmGroup1Ptr.cc
    metgm/MetGmGroup2Ptr.cc
    metgm/MetGmGroup3Ptr.cc
    metgm/MetGmGroup5Ptr.cc
    metgm/MetGmCommentAttributeParser.cc
    metgm/MetGmDimensionsTag.cc
    metgm/MetGmTags.cc metgm/MetGmHandlePtr.cc

    metgm/MetGmConfigurationMappings.h
    metgm/MetGmGroup5Ptr.h
    metgm/MetGmVersion.h
    metgm/MetGmGroup2Ptr.h
    metgm/MetGmUtils.h
    metgm/MetGmTags.h
    metgm/MetGmCDMWriterImpl.h
    metgm/MetGmCDMWriterSlicedImpl.h
    metgm/MetGmCommentAttributeParser.h
    metgm/MetGmGroup1Ptr.h
    metgm/MetGmGroup3Ptr.h
    metgm/MetGmCDMReaderImpl.h
    metgm/MetGmCDMReaderSlicedImpl.h
    metgm/MetGmVerticalTag.h
    metgm/MetGmHorizontalTag.h
    metgm/MetGmHandlePtr.h
    metgm/MetGmTimeTag.h
    metgm/MetGmCDMVariableProfile.h
    metgm/MetGmFileHandlePtr.h
    metgm/MetGmDimensionsTag.h
  )
ENDIF(ENABLE_METGM)

if(ENABLE_PRORADXML)
  SET(HAVE_PRORADXML 1)

  SET(libfimex_proradxml_SOURCES
    ProradXMLCDMReader.cc
    ProradXMLCDMReader.h
    )
ENDIF(ENABLE_PRORADXML)

IF(ENABLE_WDB)
  SET(HAVE_LIBPQ_FE_H 1)

  # include wdb/make.mk
  SET(libfimex_wdb_SOURCES
    WdbCDMReader.cc
    wdb/WdbIndex.cpp
    wdb/DataSummary.cpp
    wdb/ParameterData.cpp
    wdb/Wdb2CdmBuilder.cpp
    wdb/TimeHandler.cpp
    wdb/LevelHandler.cpp
    wdb/VersionHandler.cpp
    wdb/GridHandler.cpp

    wdb/WdbIndex.h
    )

  # include wdb/gridInformation/make.mk
  LIST(APPEND libfimex_wdb_SOURCES
    wdb/gridInformation/GridInformation.cpp
    wdb/gridInformation/LatLonGridInformation.cpp
    wdb/gridInformation/RotatedLatLonGridInformation.cpp
    wdb/gridInformation/MetricGridInformation.cpp
    )
  # include wdb/database_access/make.mk
  LIST(APPEND libfimex_wdb_SOURCES
    wdb/database_access/Parameter.cpp
    wdb/database_access/Level.cpp
    wdb/database_access/WdbConnection.cpp
    wdb/database_access/WciReadQuerySpecification.cpp
    wdb/database_access/DataSanitizer.cpp
    wdb/database_access/GridData.cpp
    )
  # include wdb/config/make.mk
  LIST(APPEND libfimex_wdb_SOURCES
    wdb/config/WdbConfiguration.cpp
    wdb/config/GlobalWdbConfiguration.cpp
    )

  FIMEX_HEADERS(libfimex_wdb_HEADERS libfimex_wdb_SOURCES ".cpp" ".h")
ENDIF()

CONFIGURE_FILE(fimex_config.h.in fimex_config.h)

SET(libfimex_ALL_SOURCES
  ${libfimex_SOURCES}
  ${libfimex_netcdf_SOURCES}
  ${libfimex_grib_SOURCES}
  ${libfimex_felt_SOURCES}
  ${libfimex_metgm_SOURCES}
  ${libfimex_proradxml_SOURCES}
  ${libfimex_wdb_SOURCES}
)

SET(libfimex_LIBS
  ${FELT_LIBRARY}
  ${PC_LIBRARIES}
  ${udunits2_LIB}
  ${proj_LIB}
  ${grib_api_LIB}
  ${eccodes_LIB}
  ${Boost_LIBRARIES}
)

FIMEX_ADD_LIBRARY(fimex "${libfimex_ALL_SOURCES}" "${libfimex_LIBS}" "${libfimex_INCLUDE_DIRECTORIES}" "${libfimex_COMPILE_DEFINITIONS}")

ADD_SUBDIRECTORY(binSrc)
