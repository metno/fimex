
#FIND_PACKAGE(PythonInterp REQUIRED)
## search for same version of python libs
#FIND_PACKAGE(PythonLibs REQUIRED)
#
#EXECUTE_PROCESS(OUTPUT_VARIABLE NUMPY_INCLUDE_DIR
#  RESULT_VARIABLE NUMPY_CHECK_EXIT_STATUS
#  COMMAND ${PYTHON_EXECUTABLE} -c "import numpy, sys; sys.stdout.write(numpy.get_include())")
#IF(${NUMPY_CHECK_EXIT_STATUS})
#  MESSAGE(FATAL_ERROR "Could not find numpy #include directory")
#ENDIF()

EXECUTE_PROCESS(OUTPUT_VARIABLE PYFIMEX_INSTALL_LIBDIR
  RESULT_VARIABLE PYFIMEX_INSTALL_EXIT_STATUS
  COMMAND ${PYTHON_EXECUTABLE} -c "import distutils.sysconfig as ds; import sys; sys.stdout.write(ds.get_python_lib(plat_specific=True, prefix='${CMAKE_INSTALL_PREFIX}'))")
IF (PYFIMEX_INSTALL_EXIT_STATUS)
  MESSAGE(FATAL_ERROR "Could not find python library install directory")
ELSE ()
  MESSAGE (STATUS "Python library install directory=${PYFIMEX_INSTALL_LIBDIR}")
ENDIF()

## search boost python package matching the python version
#IF (NOT BOOST_PYTHON_COMPONENT)
#  IF (${PYTHON_VERSION_MAJOR} GREATER 2)
#    SET(BOOST_PYTHON_COMPONENT "python${PYTHON_VERSION_MAJOR}")
#  ELSE ()
#    SET(BOOST_PYTHON_COMPONENT "python")
#  ENDIF ()
#ENDIF ()
#FIND_PACKAGE(Boost REQUIRED COMPONENTS
#  ${BOOST_PYTHON_COMPONENT}
#)

LINK_DIRECTORIES(
  ${PC_LIBRARY_DIRS}
#  ${Boost_LIBRARY_DIRS}
)

INCLUDE_DIRECTORIES(
  "${CMAKE_SOURCE_DIR}/include"
#  ${PYTHON_INCLUDE_DIRS}
#  ${NUMPY_INCLUDE_DIR}
  )

SET(pyfimex0_SOURCES
  pyfimex0_logging.cc
  pyfimex0_CDM.cc
  pyfimex0_CDMInterpolator.cc
  pyfimex0_CDMTimeInterpolator.cc
  pyfimex0_CDMExtractor.cc
  pyfimex0_CDMMerger.cc
  pyfimex0_CDMReader.cc
  pyfimex0_CDMReaderWriter.cc
  pyfimex0_CDMWriter.cc
  pyfimex0_CoordinateSystem.cc
  pyfimex0_Data.cc
  pyfimex0_NetCDF_CDMWriter.cc
  pyfimex0_AggregationReader.cc
  pyfimex0.cc
)

PYBIND11_ADD_MODULE(pyfimex0 ${pyfimex0_SOURCES})
TARGET_LINK_LIBRARIES(pyfimex0
  PRIVATE
  libfimex
#  ${Boost_LIBRARIES}
#  ${PYTHON_LIBRARIES}
  )
#SET_TARGET_PROPERTIES(pyfimex0 PROPERTIES
#  PREFIX ""
#)

#IF (NOT PYTHON_EXT_SUFFIX AND (PYTHON_VERSION_MAJOR GREATER 3) OR ((PYTHON_VERSION_MAJOR EQUAL 3) AND (PYTHON_VERSION_MINOR GREATER 2)))
#  EXECUTE_PROCESS(OUTPUT_VARIABLE PYTHON_EXT_SUFFIX
#    RESULT_VARIABLE PYFIMEX_EXT_EXIT_STATUS
#    COMMAND ${PYTHON_EXECUTABLE} -c "import distutils.sysconfig as ds; import sys; sys.stdout.write(ds.get_config_var('EXT_SUFFIX'))")
#  IF (PYFIMEX_EXT_EXIT_STATUS)
#    MESSAGE (FATAL_ERROR "Could not find python EXT_SUFFIX")
#  ENDIF()
#ENDIF ()
#IF (PYTHON_EXT_SUFFIX)
#  SET_TARGET_PROPERTIES(pyfimex0 PROPERTIES
#    SUFFIX "${PYTHON_EXT_SUFFIX}"
#  )
#ENDIF ()

INSTALL(TARGETS pyfimex0
  LIBRARY DESTINATION "${PYFIMEX_INSTALL_LIBDIR}"
)

SET(pyfimex0_TESTS
  test_pyfimex0.py
  test_pyfimex0_coordinatesystem.py
  test_pyfimex0_extractor.py
  test_pyfimex0_interpolator.py
  test_pyfimex0_merger.py
  test_pyfimex0_netcdf_cdmwriter.py
  test_pyfimex0_netcdf_cdmreaderwriter.py
  test_pyfimex0_PyCDMReader.py
  )

FOREACH(T ${pyfimex0_TESTS})
  ADD_TEST(NAME ${T}
    COMMAND ${PYTHON_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/${T}"
    )
ENDFOREACH()
SET_TESTS_PROPERTIES(${pyfimex0_TESTS}
  PROPERTIES ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}"
  )

# dummy target to tell IDEs that these files are part of the project
ADD_CUSTOM_TARGET(dummy_testfiles SOURCES ${pyfimex0_TESTS})
