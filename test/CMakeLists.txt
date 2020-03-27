
LINK_DIRECTORIES(${PC_LIBRARY_DIRS}
  ${BOOST_LIBRARY_DIRS} # if using boost unit_test_framework
)

IF (NOT TEST_EXTRADATA_DIR)
  SET (TEST_EXTRADATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/fimex-test-data)
ENDIF ()
MESSAGE (STATUS "Tests will try to find extra data in '${TEST_EXTRADATA_DIR}'")

IF (EXISTS "${TEST_EXTRADATA_DIR}/VERSION")
  FILE (STRINGS "${TEST_EXTRADATA_DIR}/VERSION" TEST_EXTRADATA_VERSION)
  SET (CURRENT_TEST_EXTRADATA_VERSION 20191030T0830)
  IF ("${TEST_EXTRADATA_VERSION}" STRLESS "${CURRENT_TEST_EXTRADATA_VERSION}")
    MESSAGE (WARNING "Unxpected test data version '${TEST_EXTRADATA_VERSION}' in '${TEST_EXTRADATA_DIR}', please consider downloading from https://wiki.met.no/fimex/download")
  ENDIF ()
ELSE ()
  MESSAGE (WARNING "No extra test data seem to be present '${TEST_EXTRADATA_DIR}', please consider downloading from https://wiki.met.no/fimex/download")
ENDIF ()

SET(CC_TESTS
  testBinaryConstants
  testCDM
  testData
  testFeltReader
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
    testNetCDFReadWrite.sh
    testNcmlRead.sh
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

  SET(METGM_NC_TESTS
    testMetGmWriter
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
    testVerticalInterpolatorAltitudeToHeight.sh
    testVerticalInterpolatorEnsembleSigmaHybridToPressure.sh
    testVerticalInterpolatorAltitude.sh
    testVerticalInterpolatorConversion.sh
    testVerticalInterpolatorOceanSG2ToDepth.sh
    testVerticalInterpolatorOceanSG2FromDepth.sh
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
  CONFIGURE_FILE(testGrib2SomeUndefined.sh.in testGrib2SomeUndefined.sh @ONLY)
  ADD_TEST(NAME testGrib2SomeUndefined.sh COMMAND testGrib2SomeUndefined.sh)
ENDIF()

IF((ENABLE_FELT) AND (ENABLE_NETCDF) AND ((ENABLE_GRIBAPI) OR (ENABLE_ECCODES)))
  LIST(APPEND SH_TESTS
    testFiIndexGribs.sh
    testFiGrbmlCat.sh
    )
ENDIF()

IF(ENABLE_METGM)
  LIST(APPEND CC_TESTS
    testMetGmReader
    ${METGM_NC_TESTS}
    )
ENDIF(ENABLE_METGM)

IF (ENABLE_NETCDF)
  FIND_PROGRAM(NCDUMP_PROGRAM
    NAMES ncdump
    HINTS "${PC_netcdf_PREFIX}/bin" "${netcdf_DIR}/bin"
  )
  IF (NCDUMP_PROGRAM)
    MESSAGE (STATUS "Found ncdump: ${NCDUMP_PROGRAM}")
  ELSE ()
    MESSAGE (STATUS "Found no ncdump, trying without absolute path")
    SET(NCDUMP_PROGRAM "ncdump")
  ENDIF()
ENDIF()

CONFIGURE_FILE(nccmp.sh.in nccmp.sh @ONLY)
CONFIGURE_FILE(fimex.sh.in fimex.sh @ONLY)

CONFIGURE_FILE(fiIndexGribs.sh.in fiIndexGribs.sh @ONLY)
CONFIGURE_FILE(fiGrbmlCat.sh.in   fiGrbmlCat.sh   @ONLY)
CONFIGURE_FILE(testQEmask.xml.in testQEmask.xml @ONLY)

CONFIGURE_FILE(test_config.h.in test_config.h)

ADD_LIBRARY(testinghelpers STATIC
  testinghelpers.cc
  testinghelpers.h
)
TARGET_INCLUDE_DIRECTORIES(testinghelpers
 PUBLIC
  "${CMAKE_BINARY_DIR}/src"   # for fimex_config.h
  ${CMAKE_CURRENT_BINARY_DIR} # for test_config.h
 PRIVATE
  "${CMAKE_SOURCE_DIR}/include"
)
TARGET_COMPILE_DEFINITIONS(testinghelpers PRIVATE
  -DTOP_SRCDIR="${CMAKE_SOURCE_DIR}"
  -DTEST_EXTRADATA_DIR="${TEST_EXTRADATA_DIR}"
)
IF(NOT USE_BOOST_UNIT_TEST)
  TARGET_LINK_LIBRARIES(testinghelpers PUBLIC
    mi-cpptest-main
  )
ENDIF()

FOREACH(T ${CC_TESTS})
  ADD_EXECUTABLE(${T} "${T}.cc")
  TARGET_INCLUDE_DIRECTORIES(${T} PRIVATE ${Boost_INCLUDE_DIRS} ${PC_INCLUDE_DIRS})
  TARGET_LINK_LIBRARIES(${T} libfimex testinghelpers ${Boost_LIBRARIES} ${PC_LIBRARIES})
  ADD_TEST(NAME ${T} COMMAND ${T})
ENDFOREACH()

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
TARGET_INCLUDE_DIRECTORIES(testFeltReader
  PRIVATE
    "${CMAKE_SOURCE_DIR}/src" # for FeltCDMReader2.h
)

FOREACH(T ${SH_TESTS})
  ADD_TEST(NAME ${T} COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/${T}")
  SET_TESTS_PROPERTIES(${T}
    PROPERTIES
    ENVIRONMENT "TEST_EXTRADATA_DIR=${TEST_EXTRADATA_DIR}"
    DEPENDS fimex
  )
ENDFOREACH()

IF(ENABLE_FELT AND ENABLE_NETCDF AND (ENABLE_GRIBAPI OR ENABLE_ECCODES))
  SET_TESTS_PROPERTIES(testGribReader testFiIndexGribs.sh testFiGrbmlCat.sh
    PROPERTIES DEPENDS testGribWriter
  )
ENDIF()

IF(ENABLE_FELT AND ENABLE_NETCDF)
  SET_TESTS_PROPERTIES(testNetCDFReadWrite.sh
    PROPERTIES DEPENDS testNetcdfWriter
  )
ENDIF()
