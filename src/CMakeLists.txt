
SET(libfimex_INCLUDE_DIRECTORIES
  "${CMAKE_SOURCE_DIR}/include"
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${PC_INCLUDE_DIRS}
  ${grib_api_INC_DIR}
  ${libxml2_INC_DIR}
  ${netcdf_INC_DIR}
  ${proj_INC_DIR}
  ${udunits2_INC_DIR}
  ${ECCODES_INCLUDE_DIRS}
  ${Boost_INCLUDE_DIRS}
  ${date_INC_DIR}
)

SET(libfimex_COMPILE_OPTIONS
  ${PC_CFLAGS_OTHER}
  )
SET(libfimex_COMPILE_DEFINITIONS
  -DPKGDATADIR="${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/fimex${MINUS_FIMEX_VERSION}"
  )
LINK_DIRECTORIES(${PC_LIBRARY_DIRS} ${Boost_LIBRARY_DIRS})

SET(INCF "${CMAKE_SOURCE_DIR}/include/fimex")

SET(libfimex_SOURCES
  c_fimex.cc
  ${INCF}/c_fimex.h
  C_CDMReader.cc
  ${INCF}/C_CDMReader.h
  CachedInterpolation.cc
  ${INCF}/CachedInterpolation.h
  CachedForwardInterpolation.cc
  CachedForwardInterpolation.h
  CachedVectorReprojection.cc
  ${INCF}/CachedVectorReprojection.h
  CDM.cc
  ${INCF}/CDM.h
  CDMAttribute.cc
  ${INCF}/CDMAttribute.h
  CDMconstants.cc
  ${INCF}/CDMconstants.h
  CDMDimension.cc
  ${INCF}/CDMDimension.h
  CDMDataType.cc
  ${INCF}/CDMDataType.h
  CDMExtractor.cc
  ${INCF}/CDMExtractor.h
  CDMFileReaderFactory.cc
  ${INCF}/CDMFileReaderFactory.h
  CDMInterpolator.cc
  ${INCF}/CDMInterpolator.h
  CDMNamedEntity.cc
  ${INCF}/CDMNamedEntity.h
  CDMMergeUtils.cc
  CDMMergeUtils.h
  CDMBorderSmoothing.cc
  ${INCF}/CDMBorderSmoothing.h
  CDMBorderSmoothing_Linear.cc
  ${INCF}/CDMBorderSmoothing_Linear.h
  CDMOverlay.cc
  ${INCF}/CDMOverlay.h
  CDMMerger.cc
  ${INCF}/CDMMerger.h
  CDMPressureConversions.cc
  ${INCF}/CDMPressureConversions.h
  CDMProcessor.cc
  ${INCF}/CDMProcessor.h
  CDMQualityExtractor.cc
  ${INCF}/CDMQualityExtractor.h
  CDMReader.cc
  ${INCF}/CDMReader.h
  CDMReaderUtils.cc
  ${INCF}/CDMReaderUtils.h
  CDMReaderWriter.cc
  ${INCF}/CDMReaderWriter.h
  CDMTimeInterpolator.cc
  ${INCF}/CDMTimeInterpolator.h
  CDMVariable.cc
  ${INCF}/CDMVariable.h
  CDMVerticalInterpolator.cc
  ${INCF}/CDMVerticalInterpolator.h
  CDM_XMLConfigHelper.cc
  CDM_XMLConfigHelper.h
  CoordinateSystemSliceBuilder.cc
  ${INCF}/CoordinateSystemSliceBuilder.h
  Data.cc
  ${INCF}/Data.h
  DataImpl.h
  DataIndex.cc
  ${INCF}/DataIndex.h
  ArrayLoop.cc
  ${INCF}/ArrayLoop.h
  FillWriter.cc
  ${INCF}/FillWriter.h
  FimexTime.cc
  ${INCF}/FimexTime.h
  GridDefinition.cc
  ${INCF}/GridDefinition.h
  IndexedData.cc
  ${INCF}/IndexedData.h
  Logger.cc
  ${INCF}/Logger.h
  Log4cppLogger.cc
  Log4cppLogger.h
  MutexLock.h
  NcmlCDMReader.cc
  ${INCF}/NcmlCDMReader.h
  NcmlAggregationReader.cc
  NcmlAggregationReader.h
  Null_CDMWriter.cc
  ${INCF}/Null_CDMWriter.h
  ReplaceStringObject.cc
  ${INCF}/ReplaceStringObject.h
  ReplaceStringTimeObject.cc
  ${INCF}/ReplaceStringTimeObject.h
  SpatialAxisSpec.cc
  ${INCF}/SpatialAxisSpec.h
  SliceBuilder.cc
  ${INCF}/SliceBuilder.h
  StringData.cc
  StringData.h
  TimeUnit.cc
  ${INCF}/TimeUnit.h
  TimeUtils.cc
  ${INCF}/TimeUtils.h
  TimeSpec.cc
  ${INCF}/TimeSpec.h
  Units.cc
  ${INCF}/Units.h
  ${INCF}/UnitsException.h
  Utils.cc
  ${INCF}/Utils.h
  XMLDoc.cc
  ${INCF}/XMLDoc.h
  XMLInput.cc
  ${INCF}/XMLInput.h
  XMLUtils.cc
  ${INCF}/XMLUtils.h

  coordSys/CoordinateSystem.cc
  ${INCF}/coordSys/CoordinateSystem.h
  coordSys/CoordinateAxis.cc
  ${INCF}/coordSys/CoordinateAxis.h
  coordSys/CoordSysImpl.h
  coordSys/CoordSysUtils.cc
  coordSys/CoordSysUtils.h
  coordSys/CF1_xCoordSysBuilder.cc
  coordSys/CF1_xCoordSysBuilder.h
  coordSys/WRFCoordSysBuilder.cc
  coordSys/WRFCoordSysBuilder.h
  coordSys/Projection.cc
  ${INCF}/coordSys/Projection.h
  coordSys/ProjectionImpl.cc
  ${INCF}/coordSys/ProjectionImpl.h
  coordSys/AlbersConicalEqualAreaProjection.cc
  ${INCF}/coordSys/AlbersConicalEqualAreaProjection.h
  coordSys/AzimuthalEquidistantProjection.cc
  ${INCF}/coordSys/AzimuthalEquidistantProjection.h
  coordSys/GeostationaryProjection.cc
  ${INCF}/coordSys/GeostationaryProjection.h
  coordSys/LambertAzimuthalEqualAreaProjection.cc
  ${INCF}/coordSys/LambertAzimuthalEqualAreaProjection.h
  coordSys/LambertConformalConicProjection.cc
  ${INCF}/coordSys/LambertConformalConicProjection.h
  coordSys/LambertCylindricalEqualAreaProjection.cc
  ${INCF}/coordSys/LambertCylindricalEqualAreaProjection.h
  coordSys/LatitudeLongitudeProjection.cc
  ${INCF}/coordSys/LatitudeLongitudeProjection.h
  coordSys/MercatorProjection.cc
  ${INCF}/coordSys/MercatorProjection.h
  coordSys/ObliqueMercatorProjection.cc
  ${INCF}/coordSys/ObliqueMercatorProjection.h
  coordSys/OrthographicProjection.cc
  ${INCF}/coordSys/OrthographicProjection.h
  coordSys/PolarStereographicProjection.cc
  ${INCF}/coordSys/PolarStereographicProjection.h
  coordSys/RotatedLatitudeLongitudeProjection.cc
  ${INCF}/coordSys/RotatedLatitudeLongitudeProjection.h
  coordSys/StereographicProjection.cc
  ${INCF}/coordSys/StereographicProjection.h
  coordSys/TransverseMercatorProjection.cc
  ${INCF}/coordSys/TransverseMercatorProjection.h
  coordSys/VerticalPerspectiveProjection.cc
  ${INCF}/coordSys/VerticalPerspectiveProjection.h
  coordSys/UnknownToFgdcProjection.cc
  ${INCF}/coordSys/UnknownToFgdcProjection.h

  coordSys/verticalTransform/AltitudeHeightConverter.cc
  ${INCF}/coordSys/verticalTransform/AltitudeHeightConverter.h
  coordSys/verticalTransform/AltitudeStandardToPressureConverter.cc
  ${INCF}/coordSys/verticalTransform/AltitudeStandardToPressureConverter.h
  coordSys/verticalTransform/AtmosphereSigma.cc
  ${INCF}/coordSys/verticalTransform/AtmosphereSigma.h
  coordSys/verticalTransform/Depth.cc
  ${INCF}/coordSys/verticalTransform/Depth.h
  coordSys/verticalTransform/GeopotentialToAltitudeConverter.cc
  ${INCF}/coordSys/verticalTransform/GeopotentialToAltitudeConverter.h
  coordSys/verticalTransform/Height.cc
  ${INCF}/coordSys/verticalTransform/Height.h
  coordSys/verticalTransform/HybridSigmaApToPressureConverter.cc
  ${INCF}/coordSys/verticalTransform/HybridSigmaApToPressureConverter.h
  coordSys/verticalTransform/HybridSigmaPressure1.cc
  ${INCF}/coordSys/verticalTransform/HybridSigmaPressure1.h
  coordSys/verticalTransform/HybridSigmaPressure2.cc
  ${INCF}/coordSys/verticalTransform/HybridSigmaPressure2.h
  coordSys/verticalTransform/HybridSigmaToPressureConverter.cc
  ${INCF}/coordSys/verticalTransform/HybridSigmaToPressureConverter.h
  coordSys/verticalTransform/IdentityConverter.cc
  ${INCF}/coordSys/verticalTransform/IdentityConverter.h
  coordSys/verticalTransform/LnPressure.cc
  ${INCF}/coordSys/verticalTransform/LnPressure.h
  coordSys/verticalTransform/LnPressureToPressureConverter.cc
  ${INCF}/coordSys/verticalTransform/LnPressureToPressureConverter.h
  coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.cc
  ${INCF}/coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.h
  coordSys/verticalTransform/OceanSG1.cc
  ${INCF}/coordSys/verticalTransform/OceanSG1.h
  coordSys/verticalTransform/OceanSG2.cc
  ${INCF}/coordSys/verticalTransform/OceanSG2.h
  coordSys/verticalTransform/Pressure.cc
  ${INCF}/coordSys/verticalTransform/Pressure.h
  coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.cc
  ${INCF}/coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.h
  coordSys/verticalTransform/PressureToStandardAltitudeConverter.cc
  ${INCF}/coordSys/verticalTransform/PressureToStandardAltitudeConverter.h
  coordSys/verticalTransform/SigmaToPressureConverter.cc
  ${INCF}/coordSys/verticalTransform/SigmaToPressureConverter.h
  coordSys/verticalTransform/ToVLevelConverter.cc
  ${INCF}/coordSys/verticalTransform/ToVLevelConverter.h
  coordSys/verticalTransform/ToVLevelConverterAdapter.cc
  ${INCF}/coordSys/verticalTransform/ToVLevelConverterAdapter.h
  coordSys/verticalTransform/VerticalConverter.cc
  ${INCF}/coordSys/verticalTransform/VerticalConverter.h
  coordSys/verticalTransform/VerticalTransformation.cc
  ${INCF}/coordSys/verticalTransform/VerticalTransformation.h
  coordSys/verticalTransform/VerticalTransformationUtils.cc
  ${INCF}/coordSys/verticalTransform/VerticalTransformationUtils.h

  interpolation.c
  ${INCF}/interpolation.h
  mifi_mpi.c
  ${INCF}/mifi_mpi.h
  ThreadPool.c
  ${INCF}/ThreadPool.h
  vertical_coordinate_transformations.c
  ${INCF}/vertical_coordinate_transformations.h

  leap_iterator.h
)

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
    ${INCF}/NetCDF_CDMReader.h
    NetCDF_CDMWriter.cc
    ${INCF}/NetCDF_CDMWriter.h
    NetCDF_Utils.cc
    NetCDF_Utils.h
  )
ENDIF(ENABLE_NETCDF)

IF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))
  SET(HAVE_GRIB_API_H 1)

  IF(ENABLE_GRIBAPI)
    LIST(APPEND libfimex_INCLUDE_DIRECTORIES ${GRIBAPI_INC_DIR})
  ENDIF()

  SET(libfimex_grib_SOURCES
    GribApiCDMWriter.cc
    ${INCF}/GribApiCDMWriter.h
    GribApiCDMWriter_ImplAbstract.cc
    GribApiCDMWriter_ImplAbstract.h
    GribApiCDMWriter_Impl1.cc
    GribApiCDMWriter_Impl1.h
    GribApiCDMWriter_Impl2.cc
    GribApiCDMWriter_Impl2.h
    GribCDMReader.cc
    ${INCF}/GribCDMReader.h
    GribFileIndex.cc
    ${INCF}/GribFileIndex.h
    GribUtils.cc
    ${INCF}/GribUtils.h
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
    ${INCF}/MetGmCDMWriter.h
    MetGmCDMReader.cc
    ${INCF}/MetGmCDMReader.h

    metgm/MetGmCDMReaderImpl.cc
    metgm/MetGmCDMReaderImpl.h
    metgm/MetGmCDMReaderSlicedImpl.cc
    metgm/MetGmCDMReaderSlicedImpl.h
    metgm/MetGmCDMVariableProfile.h
    metgm/MetGmCDMWriterImpl.cc
    metgm/MetGmCDMWriterImpl.h
    metgm/MetGmCDMWriterSlicedImpl.cc
    metgm/MetGmCDMWriterSlicedImpl.h
    metgm/MetGmCommentAttributeParser.cc
    metgm/MetGmCommentAttributeParser.h
    metgm/MetGmConfigurationMappings.h
    metgm/MetGmDimensionsTag.cc
    metgm/MetGmDimensionsTag.h
    metgm/MetGmFileHandlePtr.h
    metgm/MetGmGroup1Ptr.cc
    metgm/MetGmGroup1Ptr.h
    metgm/MetGmGroup2Ptr.cc
    metgm/MetGmGroup2Ptr.h
    metgm/MetGmGroup3Ptr.cc
    metgm/MetGmGroup3Ptr.h
    metgm/MetGmGroup5Ptr.cc
    metgm/MetGmGroup5Ptr.h
    metgm/MetGmHandlePtr.cc
    metgm/MetGmHandlePtr.h
    metgm/MetGmHorizontalTag.cc
    metgm/MetGmHorizontalTag.h
    metgm/MetGmTags.cc
    metgm/MetGmTags.h
    metgm/MetGmTimeTag.cc
    metgm/MetGmTimeTag.h
    metgm/MetGmUtils.cc
    metgm/MetGmUtils.h
    metgm/MetGmVersion.h
    metgm/MetGmVerticalTag.cc
    metgm/MetGmVerticalTag.h
  )
ENDIF(ENABLE_METGM)

if(ENABLE_PRORADXML)
  SET(HAVE_PRORADXML 1)

  SET(libfimex_proradxml_SOURCES
    ProradXMLCDMReader.cc
    ProradXMLCDMReader.h
    )
ENDIF(ENABLE_PRORADXML)

CONFIGURE_FILE(fimex_config.h.in fimex_config.h)

SET(libfimex_ALL_SOURCES
  ${libfimex_SOURCES}
  ${libfimex_netcdf_SOURCES}
  ${libfimex_grib_SOURCES}
  ${libfimex_felt_SOURCES}
  ${libfimex_metgm_SOURCES}
  ${libfimex_proradxml_SOURCES}
)

SET(libfimex_LIBS
  ${FELT_LIBRARY}
  ${PC_LIBRARIES}
  ${grib_api_LIB}
  ${libxml2_LIB}
  ${netcdf_LIB}
  ${proj_LIB}
  ${udunits2_LIB}
  ${eccodes_LIB}
  ${Boost_LIBRARIES}
  ${date_LIB}
)

FIMEX_ADD_LIBRARY(fimex "${libfimex_ALL_SOURCES}" "${libfimex_LIBS}" "${libfimex_INCLUDE_DIRECTORIES}" "${libfimex_COMPILE_DEFINITIONS}" "${libfimex_COMPILE_OPTIONS}")

ADD_SUBDIRECTORY(binSrc)
