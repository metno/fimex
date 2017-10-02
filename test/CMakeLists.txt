
INCLUDE_DIRECTORIES(
  "${CMAKE_SOURCE_DIR}/include"
  "${CMAKE_SOURCE_DIR}/src" ## for FeltCDMReader2.h
  )
LINK_DIRECTORIES(${PC_LIBRARY_DIRS} ${BOOST_LIBRARY_DIRS})

SET(CC_TESTS
  testInterpolation
  testVerticalCoordinates
  testBinaryConstants
  testUtils
  testTimeSpec
  testSpatialAxisSpec
  testData
  testProjections
  testUnits
  testFileReaderFactory
  testCDM
  testSliceBuilder
  testXMLDoc
  testFeltReader
)

IF(ENABLE_FELT)
  LIST(APPEND CC_TESTS
    testQualityExtractor
    )
  SET(NETCDF_MI_TESTS
    testNetcdfWriter
    testExtractor
    testInterpolator
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
  SET(GRIBAPI_NC_TESTS
    testGribReader
    )
ENDIF()

IF(ENABLE_NETCDF)
  LIST(APPEND CC_TESTS
    ${NETCDF_MI_TESTS}
    testCoordSys
    testCoordinateSystemSliceBuilder
    testNcmlReader
    testNcmlAggregationReader
    testProcessor
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
    testInterpolation2DataFillValue.sh
    testVerticalInterpolator.sh
    testVerticalInterpolator2.sh
    testVerticalInterpolator3.sh
    testVerticalInterpolator4.sh
    testVerticalInterpolatorHeight.sh
    testVerticalInterpolatorAltitude.sh
    testVerticalInterpolatorConversion.sh
    testVerticalInterpolatorOceanSG2ToDepth.sh
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
    )
ENDIF()

IF((ENABLE_FELT) AND (ENABLE_NETCDF) AND ((ENABLE_GRIBAPI) OR (ENABLE_ECCODES)))
  LIST(APPEND SH_TESTS
    testFiIndexGribs.sh
    )
ENDIF()

IF(ENABLE_METGM)
  LIST(APPEND CC_TESTS
    testMetGmReader
    ${METGM_NC_TESTS}
    )
ENDIF(ENABLE_METGM)

IF(ENABLE_WDB)
  INCLUDE_DIRECTORIES("${CMAKE_SOURCE_DIR}/src"
    "${CMAKE_BINARY_DIR}/src"
    ${PC_INCLUDE_DIRS}
    )
  ADD_DEFINITIONS(-DWDB_TEST_DIR="${CMAKE_CURRENT_SOURCE_DIR}/wdb")
  ADD_COMPILE_OPTIONS(${PC_CFLAGS_OTHER})
  LINK_DIRECTORIES(${PC_LIBRARY_DIRS})

  ADD_EXECUTABLE(wdbTest
    wdb/TestingGridData.cpp
    wdb/GridDataFactory.cpp
    wdb/TestingGridData.h
    wdb/GridDataFactory.h

    wdb/Wdb2CdmBuilderTest.cc
    wdb/WdbIndexTest.cc
    wdb/ConfigurationTest.cpp
    wdb/GlobalWdbConfigurationTest.cpp
    wdb/TimeHandlerTest.cpp
    )
  TARGET_LINK_LIBRARIES(wdbTest libfimex)
  ADD_TEST(NAME wdbTest COMMAND wdbTest)
ENDIF()

CONFIGURE_FILE(fimex.sh.in fimex.sh @ONLY)
CONFIGURE_FILE(fiIndexGribs.sh.in fiIndexGribs.sh @ONLY)
CONFIGURE_FILE(testQEmask.xml.in testQEmask.xml @ONLY)

FOREACH(T ${CC_TESTS})
  ADD_EXECUTABLE(${T} "${T}.cc")
  TARGET_COMPILE_DEFINITIONS(${T} PRIVATE -DTOP_SRCDIR="${CMAKE_SOURCE_DIR}")
  TARGET_LINK_LIBRARIES(${T} libfimex)
  ADD_TEST(NAME ${T} COMMAND ${T})
ENDFOREACH()

FOREACH(T ${C_TESTS})
  ADD_EXECUTABLE(${T} "${T}.c")
  TARGET_COMPILE_DEFINITIONS(${T} PRIVATE -DTOP_SRCDIR="${CMAKE_SOURCE_DIR}")
  TARGET_LINK_LIBRARIES(${T} libfimex)
  ADD_TEST(NAME ${T} COMMAND ${T})
ENDFOREACH()

FOREACH(T ${SH_TESTS})
  ADD_TEST(NAME ${T} COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/${T}")
ENDFOREACH()

IF((ENABLE_FELT) AND (ENABLE_NETCDF) AND ((ENABLE_GRIBAPI) OR (ENABLE_ECCODES)))
  SET_TESTS_PROPERTIES(testGribReader testFiIndexGribs.sh
    PROPERTIES DEPENDS testGribWriter
    )
ENDIF()
