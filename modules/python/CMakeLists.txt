
EXECUTE_PROCESS(OUTPUT_VARIABLE PYFIMEX_INSTALL_LIBDIR
  RESULT_VARIABLE PYFIMEX_INSTALL_EXIT_STATUS
  COMMAND ${Python_EXECUTABLE} -c "
import sys
if sys.version_info >= (3, 10):
  import sysconfig as s
  pl = s.get_path('platlib', 'posix_prefix')
  pl = pl.replace(s.get_config_var('prefix'), '${CMAKE_INSTALL_PREFIX}')
else:
  from distutils import sysconfig as ds
  pl = ds.get_python_lib(plat_specific=True, prefix='${CMAKE_INSTALL_PREFIX}')
sys.stdout.write(pl)
")
IF (PYFIMEX_INSTALL_EXIT_STATUS)
  MESSAGE(FATAL_ERROR "Could not find python library install directory")
ELSE ()
  MESSAGE (STATUS "Python library install directory=${PYFIMEX_INSTALL_LIBDIR}")
ENDIF()

LINK_DIRECTORIES(
  ${PC_LIBRARY_DIRS}
)

INCLUDE_DIRECTORIES(
  "${CMAKE_SOURCE_DIR}/src" # for CDMMergeUtils.h, ...
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
  pyfimex0_NcmlCDMReader.cc
  pyfimex0_AggregationReader.cc
  pyfimex0.cc
)

PYBIND11_ADD_MODULE(pyfimex0 ${pyfimex0_SOURCES})
TARGET_LINK_LIBRARIES(pyfimex0
  PRIVATE
  libfimex
)

INSTALL(TARGETS pyfimex0
  LIBRARY DESTINATION "${PYFIMEX_INSTALL_LIBDIR}"
)

SET(pyfimex0_TESTS
  test_pyfimex0.py
  test_pyfimex0_PyCDMReader.py
  )

IF (ENABLE_NETCDF)
  LIST (APPEND pyfimex0_TESTS
    test_pyfimex0_coordinatesystem.py
    test_pyfimex0_extractor.py
    test_pyfimex0_interpolator.py
    test_pyfimex0_merger.py
    test_pyfimex0_netcdf.py
    test_pyfimex0_netcdf_cdmwriter.py
    test_pyfimex0_netcdf_cdmreaderwriter.py
  )
ENDIF()

FOREACH(T ${pyfimex0_TESTS})
  ADD_TEST(NAME ${T}
    COMMAND ${Python_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/${T}"
    )
ENDFOREACH()
SET(TESTS_PYTHONPATH "${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}" )
SET_PROPERTY(
  TEST ${pyfimex0_TESTS}
  APPEND PROPERTY ENVIRONMENT "PYTHONPATH=${TESTS_PYTHONPATH}"
)
SET_PROPERTY(
  TEST ${pyfimex0_TESTS}
  APPEND PROPERTY ENVIRONMENT "FIMEX_IO_PLUGINS_PATH=${FIMEX_IO_PLUGINS_PATH}"
)

# dummy target to tell IDEs that these files are part of the project
ADD_CUSTOM_TARGET(dummy_testfiles SOURCES ${pyfimex0_TESTS})
