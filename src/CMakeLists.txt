
SET(FIMEX_DATADIR "${CMAKE_INSTALL_FULL_DATADIR}/fimex${MINUS_FIMEX_VERSION}")

SET(INCF "${CMAKE_SOURCE_DIR}/include/fimex")

SET(libfimex_ncml_SOURCES
  NcmlCDMReader.cc
  ${INCF}/NcmlCDMReader.h
  NcmlCDMWriter.cc
  ${INCF}/NcmlCDMWriter.h
  NcmlIoFactory.cc
  NcmlIoFactory.h
  NcmlAggregationReader.cc
  NcmlAggregationReader.h
  NcmlUtils.cc
  NcmlUtils.h
)

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
  CDMWriter.cc
  ${INCF}/CDMWriter.h
  CDM_XMLConfigHelper.cc
  ${INCF}/CDM_XMLConfigHelper.h
  CoordinateSystemSliceBuilder.cc
  ${INCF}/CoordinateSystemSliceBuilder.h
  Data.cc
  ${INCF}/Data.h
  DataUtils.cc
  ${INCF}/DataUtils.h
  DataImpl.h
  DataIndex.cc
  ${INCF}/DataIndex.h
  AggregationReader.cc
  ${INCF}/AggregationReader.h
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
  IoFactory.cc
  ${INCF}/IoFactory.h
  IoPlugin.cc
  ${INCF}/IoPlugin.h

  Logger.cc
  ${INCF}/Logger.h
  Log4cppLogger.cc
  Log4cppLogger.h
  ${INCF}/MutexLock.h
  NativeData.cc
  NativeData.h
  Null_CDMWriter.cc
  ${INCF}/Null_CDMWriter.h
  NullIoFactory.cc
  NullIoFactory.h
  ${INCF}/RecursiveSliceCopy.h
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
  String2Type.cc
  ${INCF}/String2Type.h
  Type2String.cc
  ${INCF}/Type2String.h
  ${INCF}/TokenizeDotted.h
  ${INCF}/FindNeighborElements.h
  StringUtils.cc
  ${INCF}/StringUtils.h
  MathUtils.cc
  ${INCF}/MathUtils.h
  FileUtils.cc
  ${INCF}/FileUtils.h
  ${INCF}/min_max.h
  XMLDoc.cc
  ${INCF}/XMLDoc.h
  ${INCF}/XMLInput.h
  ${INCF}/XMLInputDoc.h
  XMLInputFile.cc
  ${INCF}/XMLInputFile.h
  XMLInputString.cc
  ${INCF}/XMLInputString.h
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
  ${INCF}/coordSys/SinusoidalProjection.h
  coordSys/SinusoidalProjection.cc
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

  reproject.cc
  ${INCF}/reproject.h

  leap_iterator.h
)
LIST (APPEND libfimex_SOURCES ${libfimex_ncml_SOURCES})

IF(ENABLE_LOG4CPP)
  SET(HAVE_LOG4CPP 1)
ENDIF()

IF(MPI_CXX_FOUND)
  SET(HAVE_MPI 1)
ENDIF()

SET(FIMEX_IO_PLUGINS_PATH "${CMAKE_INSTALL_FULL_LIBDIR}")

CONFIGURE_FILE(fimex_config.h.in fimex_config.h)

SET(libfimex_PACKAGES
  ${date_PACKAGE}
  ${libxml2_PACKAGE}
  ${log4cpp_PACKAGE}
  ${proj_PACKAGE}
  ${udunits2_PACKAGE}
  ${openmp_CXX_PACKAGE}
)

FIMEX_ADD_LIBRARY(fimex "${libfimex_SOURCES}" "${libfimex_PACKAGES}")

SET(IO_PACKAGES "libfimex;${libxml2_PACKAGE};${openmp_CXX_PACKAGE}")
IF (ENABLE_NETCDF)
ADD_SUBDIRECTORY(io/netcdf)
ENDIF ()
IF (ENABLE_FELT)
ADD_SUBDIRECTORY(io/felt)
ENDIF ()
IF (ENABLE_GRIBAPI OR ENABLE_ECCODES)
ADD_SUBDIRECTORY(io/grib)
ENDIF ()

ADD_SUBDIRECTORY(binSrc)
