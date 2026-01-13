
IF (NOT TEST_EXTRADATA_DIR)
  SET (TEST_EXTRADATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/fimex-test-data)
ENDIF ()
MESSAGE (STATUS "Tests will try to find extra data in '${TEST_EXTRADATA_DIR}'")

SET (CURRENT_TEST_EXTRADATA_VERSION 20220211T1614)
SET(TEST_EXTRADATA_PAGE "https://wiki.met.no/fimex/download")
SET(TEST_EXTRADATA_URL "https://wiki.met.no/_media/fimex/fimex-test-data-${CURRENT_TEST_EXTRADATA_VERSION}.tar.gz")
SET(TEST_EXTRADATA_MESSAGE "in '${TEST_EXTRADATA_DIR}', please consider downloading version ${CURRENT_TEST_EXTRADATA_VERSION} from ${TEST_EXTRADATA_PAGE}")

IF (EXISTS "${TEST_EXTRADATA_DIR}/VERSION")
  FILE (STRINGS "${TEST_EXTRADATA_DIR}/VERSION" TEST_EXTRADATA_VERSION)
  IF ("${TEST_EXTRADATA_VERSION}" STRLESS "${CURRENT_TEST_EXTRADATA_VERSION}")
    MESSAGE (WARNING "Unxpected test data version '${TEST_EXTRADATA_VERSION}' ${TEST_EXTRADATA_MESSAGE}")
  ENDIF ()
ELSE ()
  MESSAGE (WARNING "No extra test data seem to be present ${TEST_EXTRADATA_MESSAGE}")
ENDIF ()

SET(CC_TESTS
  testBinaryConstants
  testCDM
  testData
  testFileReaderFactory
  testInterpolation
  testInterpolator
  testProcessor
  testProjections
  testQualityExtractor
  testSliceBuilder
  testSpatialAxisSpec
  testTimeSpec
  testUnits
  testUtils
  testVerticalCoordinates
  testXMLDoc
)

IF(ENABLE_FELT)
  LIST(APPEND CC_TESTS
    testExtractor
    testFeltReader
  )

  SET(NETCDF_MI_TESTS
    testNetcdfWriter
    testTimeInterpolator
    )

  SET(GRIBAPI_MI_TESTS
    testGribWriter
    )

  SET(NETCDF_MI_SH_TESTS
    testFeltReadNetCDFWrite.sh
    testNcmlRead.sh
    testNetCDFReadWrite.sh
    testPrintNcmlNAT.sh
    test_ncmlagg_with_config.sh
    )
ENDIF(ENABLE_FELT)

IF((ENABLE_FELT) AND (ENABLE_NETCDF))
  SET(C_TESTS
    test_c_consumer
    )
ENDIF()

IF(ENABLE_NETCDF)
  LIST(APPEND CC_TESTS
    ${NETCDF_MI_TESTS}
    testCoordSys
    testCoordinateSystemSliceBuilder
    testNcmlReader
    testNcmlAggregationReader
    testMerger
    testNetCDFReaderWriter
    testFillWriter
    testVerticalVelocity
    testVLevelConverter
    )

  LIST(APPEND SH_TESTS
    ${NETCDF_MI_SH_TESTS}
    testCharNc.sh
    testInterpolatorFill.sh
    testInterpolatorLonLat1D.sh
    testInterpolatorLonLat2D.sh
    testInterpolation2DataFillValue.sh
    testPrintSize.sh
    testVerticalInterpolatorPressureToPressure.sh
    testVerticalInterpolatorSigmaHybridToPressure.sh
    testVerticalInterpolatorSigmaHybridFromPressure.sh
    testVerticalInterpolatorSigmaHybridReduced.sh
    testVerticalInterpolatorHeightToAltitude.sh
    testVerticalInterpolatorEnsembleSigmaHybridToPressure.sh
    testVerticalInterpolatorAltitude.sh
    testVerticalInterpolatorConversion.sh
    testVerticalInterpolatorOceanSG2ToDepth.sh
    testVerticalInterpolatorOceanSG2FromDepth.sh
    testVerticalInterpolatorOceanSG2FromDepthReplace.sh
    testWriteNcStrings.sh
    testQualityExtractorFimex.sh
    testMerger.sh
    testMergerTarget.sh
    )

  IF(HAVE_NETCDF_HDF5_LIB)
    LIST(APPEND SH_TESTS testNcString.sh)
  ENDIF()
ENDIF(ENABLE_NETCDF)

IF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))
  LIST(APPEND CC_TESTS 
    ${GRIBAPI_MI_TESTS}
    ${GRIBAPI_NC_TESTS}
    testGribReader
    )
ENDIF()
IF(ENABLE_NETCDF AND (ENABLE_GRIBAPI OR ENABLE_ECCODES))
  CONFIGURE_FILE(testGrib2SomeUndefined.sh.in testGrib2SomeUndefined.sh @ONLY)
  LIST(APPEND SH_BIN_TESTS testGrib2SomeUndefined.sh)
  LIST(APPEND CC_TESTS testGribWriterVertical)
ENDIF()
IF (ENABLE_ECCODES)
  FIND_PROGRAM(GRIB_COUNT_PROGRAM
    NAMES grib_count
    HINTS "${eccodes_DIR}/bin"
  )
  IF (GRIB_COUNT_PROGRAM)
    MESSAGE (STATUS "Found grib_count: ${GRIB_COUNT_PROGRAM}")
    CONFIGURE_FILE(test-grib-omit-empty.sh.in test-grib-omit-empty.sh @ONLY)
    LIST(APPEND SH_BIN_TESTS test-grib-omit-empty.sh)
  ELSE ()
    MESSAGE (STATUS "Found no grib_count, omitting test")
  ENDIF()
ENDIF()

IF(ENABLE_FELT AND ((ENABLE_GRIBAPI) OR (ENABLE_ECCODES)))
  LIST(APPEND SH_TESTS
    testFiIndexGribs.sh
    testFiGrbmlCat.sh
    )
ENDIF()

IF (ENABLE_NETCDF)
  FIND_PROGRAM(NCDUMP_PROGRAM
    NAMES ncdump
    HINTS "${netcdf_DIR}/bin"
  )
  IF (NCDUMP_PROGRAM)
    MESSAGE (STATUS "Found ncdump: ${NCDUMP_PROGRAM}")
  ELSE ()
    MESSAGE (STATUS "Found no ncdump, trying without absolute path")
    SET(NCDUMP_PROGRAM "ncdump")
  ENDIF()
ENDIF()

CONFIGURE_FILE(fimex_test_config.h.in fimex_test_config.h)

CONFIGURE_FILE(fimex.sh.in fimex.sh @ONLY)

CONFIGURE_FILE(fiIndexGribs.sh.in fiIndexGribs.sh @ONLY)
CONFIGURE_FILE(fiGrbmlCat.sh.in   fiGrbmlCat.sh   @ONLY)
CONFIGURE_FILE(testQEmask.xml.in testQEmask.xml @ONLY)

ADD_EXECUTABLE(cdmcmp
  cdmcmp.cc
)
TARGET_LINK_LIBRARIES(cdmcmp PRIVATE
  mi-programoptions
  libfimex
)

ADD_LIBRARY(testinghelpers STATIC
  testinghelpers.cc
  testinghelpers.h
)
TARGET_INCLUDE_DIRECTORIES(testinghelpers
 PUBLIC
  "${CMAKE_CURRENT_BINARY_DIR}" # for fimex_test_config.h
 PRIVATE
  "${CMAKE_SOURCE_DIR}/include"
)
TARGET_COMPILE_DEFINITIONS(testinghelpers PRIVATE
  -DTOP_SRCDIR="${CMAKE_SOURCE_DIR}"
  -DTEST_EXTRADATA_DIR="${TEST_EXTRADATA_DIR}"
)
TARGET_LINK_LIBRARIES(testinghelpers PUBLIC
  mi-cpptest
  libfimex
)

FOREACH(T ${CC_TESTS})
  ADD_EXECUTABLE(${T} "${T}.cc")
  TARGET_LINK_LIBRARIES(${T}
    testinghelpers
  )
  ADD_TEST(NAME ${T} COMMAND ${T})
ENDFOREACH()

TARGET_LINK_LIBRARIES(testXMLDoc ${libxml2_PACKAGE})

FOREACH(T ${C_TESTS})
  ADD_EXECUTABLE(${T} "${T}.c")
  TARGET_COMPILE_DEFINITIONS(${T} PRIVATE
    -DTOP_SRCDIR="${CMAKE_SOURCE_DIR}"
    -DTEST_EXTRADATA_DIR="${TEST_EXTRADATA_DIR}"
  )
  TARGET_LINK_LIBRARIES(${T} libfimex)
  ADD_TEST(NAME ${T} COMMAND ${T})
ENDFOREACH()

TARGET_INCLUDE_DIRECTORIES(testUtils
  PRIVATE
    "${CMAKE_SOURCE_DIR}/src" # for leap_iterator.h
)
IF(ENABLE_FELT)
  TARGET_INCLUDE_DIRECTORIES(testFeltReader
    PRIVATE
      "${CMAKE_SOURCE_DIR}/src" # for FeltCDMReader2.h
  )
ENDIF(ENABLE_FELT)

FOREACH(T ${SH_TESTS})
  ADD_TEST(NAME ${T} COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/${T}")
ENDFOREACH()
FOREACH(T ${SH_BIN_TESTS})
  ADD_TEST(NAME ${T} COMMAND "${T}")
ENDFOREACH()

SET_TESTS_PROPERTIES(${SH_TESTS};${SH_BIN_TESTS} PROPERTIES DEPENDS fimex)

SET_PROPERTY(
  TEST ${SH_TESTS} ${SH_BIN_TESTS} ${C_TESTS} ${CC_TESTS}
  APPEND PROPERTY ENVIRONMENT "TEST_EXTRADATA_DIR=${TEST_EXTRADATA_DIR}"
)

SET_PROPERTY(
  TEST ${SH_TESTS} ${SH_BIN_TESTS} ${C_TESTS} ${CC_TESTS}
  APPEND PROPERTY ENVIRONMENT "FIMEX_IO_PLUGINS_PATH=${FIMEX_IO_PLUGINS_PATH}"
)

IF(ENABLE_FELT AND (ENABLE_GRIBAPI OR ENABLE_ECCODES))
  SET_PROPERTY(
    TEST testGribReader testFiIndexGribs.sh testFiGrbmlCat.sh
    APPEND PROPERTY DEPENDS testGribWriter
  )
  TARGET_LINK_LIBRARIES(testGribWriter libfimex-io-grib ${libxml2_PACKAGE})
  TARGET_LINK_LIBRARIES(testGribWriterVertical ${libxml2_PACKAGE})
ENDIF()
IF(ENABLE_GRIBAPI OR ENABLE_ECCODES)
  TARGET_LINK_LIBRARIES(testGribReader libfimex-io-grib)
ENDIF()

IF(ENABLE_FELT AND ENABLE_NETCDF)
  SET_PROPERTY(
    TEST testNetCDFReadWrite.sh
    APPEND PROPERTY DEPENDS testNetcdfWriter
  )
  TARGET_LINK_LIBRARIES(testNetcdfWriter libfimex-io-netcdf ${libxml2_PACKAGE})
ENDIF()

IF(ENABLE_NETCDF)
  TARGET_LINK_LIBRARIES(testNcmlAggregationReader libfimex-io-netcdf)
ENDIF()

IF(ENABLE_FELT)
  TARGET_LINK_LIBRARIES(testFeltReader libfimex-io-felt)
ENDIF()

IF(ENABLE_NETCDF AND (ENABLE_GRIBAPI OR ENABLE_ECCODES))
  TARGET_LINK_LIBRARIES(testGribWriterVertical libfimex-io-grib libfimex-io-netcdf)
ENDIF()
