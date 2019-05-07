# Fimex, CMakeLists.txt
#
# Copyright (C) 2018-2019 met.no
#
# Contact information:
# Norwegian Meteorological Institute
# Box 43 Blindern
# 0313 OSLO
# NORWAY
# email: diana@met.no
#
# Project Info:  https://wiki.met.no/fimex/start
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
# USA.

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12)
PROJECT(fimex)

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
INCLUDE(FimexUtils)

FIMEX_CMAKE_SETUP()
FIMEX_VERSION_DEFINES(FIMEX "include/fimex/CDMconstants.h")

FIMEX_FIND_PACKAGE("libxml2" "libxml-2.0>=2.6.0" "xml2" "libxml/xmlversion.h")
LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE ${libxml2_PC})

FIMEX_FIND_PACKAGE("udunits2" "udunits>=2" "udunits2" "udunits2.h")
LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE ${udunits2_PC})

FIMEX_FIND_PACKAGE("proj" "proj" "proj" "proj_api.h")
LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE ${proj_PC})

OPTION(BUILD_SHARED_LIBS "Select ON (default) for shared lib, OFF for static lib, BOTH for both" ON)

#OPTION(ENABLE_PYTHON "Use python" ON)

FIND_PACKAGE(Boost REQUIRED COMPONENTS
  date_time
  program_options
  #unit_test_framework
)
IF ((Boost_UNIT_TEST_FRAMEWORK_FOUND) AND ((Boost_VERSION EQUAL 103400) OR (Boost_VERSION GREATER 103400)))
  SET (HAVE_BOOST_UNIT_TEST_FRAMEWORK TRUE)
ENDIF ()

OPTION(ENABLE_MPI "Use MPI" OFF)
IF(ENABLE_MPI)
  FIND_PACKAGE(MPI)
ENDIF()

OPTION(ENABLE_FIMEX_OMP "Use OpenMP" OFF)
IF(ENABLE_FIMEX_OMP)
  FIND_PACKAGE(OpenMP)
  IF(OPENMP_FOUND)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  ENDIF()
ENDIF()

OPTION(ENABLE_LOG4CPP "Use Log4Cpp" OFF)
IF(ENABLE_LOG4CPP)
  LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE log4cpp)
ENDIF()

OPTION(ENABLE_NETCDF "Use NetCDF" ON)
IF(ENABLE_NETCDF)
  FIMEX_FIND_PACKAGE("netcdf" "netcdf" "netcdf" "netcdf.h")
  LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE ${netcdf_PC})
ENDIF()

OPTION(ENABLE_FELT "Use Felt library" ON)

OPTION(ENABLE_ECCODES "Use ecCodes library" OFF)
OPTION(ENABLE_GRIBAPI "Use GribAPI library" OFF)

IF((ENABLE_GRIBAPI) AND (ENABLE_ECCODES))
  MESSAGE(FATAL_ERROR "Cannot use both ecCodes libraries")
ELSEIF(ENABLE_ECCODES)
  FIND_PACKAGE(eccodes REQUIRED)
  SET(eccodes_LIB "eccodes")
  IF(ECCODES_HAVE_ECCODES_THREADS)
    SET(HAVE_GRIB_API_THREADSAFE 1)
  ENDIF()
ELSEIF(ENABLE_GRIBAPI)
  FIMEX_FIND_PACKAGE("grib_api" "" "grib_api" "grib_api.h")
  OPTION(ENABLE_GRIBAPIPTHREAD "Use GribAPI library with pthread support" ON)
  IF(ENABLE_GRIBAPIPTHREAD)
    SET(HAVE_GRIB_API_THREADSAFE 1)
  ENDIF()
ENDIF()

OPTION(ENABLE_METGM "Use metgm" OFF)
IF(ENABLE_METGM)
  LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE metgm)
ENDIF()

OPTION(ENABLE_PRORADXML "Use proradxml" OFF)
IF(ENABLE_PRORADXML)
  LIST(APPEND FIMEX_PC_REQUIRES_PRIVATE libproradxmlrw)
ENDIF()

OPTION(ENABLE_FORTRAN "Use fortran" OFF)
IF(ENABLE_FORTRAN)
  ENABLE_LANGUAGE(Fortran)
ENDIF()

OPTION(ENABLE_FIMEX_VERSIONNUMBERED "Use version numbers in filenames" ON)

PKG_CHECK_MODULES(PC REQUIRED ${FIMEX_PC_REQUIRES};${FIMEX_PC_REQUIRES_PRIVATE})

IF(ENABLE_NETCDF)
  CHECK_NETCDF_HAS_HDF5(HAVE_NETCDF_HDF5_LIB)
ENDIF()


IF(ENABLE_FIMEX_VERSIONNUMBERED)
  SET(MINUS_FIMEX_VERSION "-${FIMEX_VERSION}")
  SET(pc_fimex_includedir "/fimex${MINUS_FIMEX_VERSION}")
  CONFIGURE_FILE("fimex-noversion.pc.in" "fimex.pc" @ONLY)
  INSTALL(FILES "${CMAKE_BINARY_DIR}/fimex.pc"    DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")
  SET(lib_soversion 0)
  SET(lib_version "${lib_soversion}.${FIMEX_VERSION_PATCH}.${FIMEX_VERSION_STATUS_DEC}")
ELSE()
  SET(MINUS_FIMEX_VERSION "")
  SET(pc_fimex_includedir "")
  SET(lib_soversion 0)
  SET(lib_version "${lib_soversion}.0.0")
ENDIF()
SET(lib_name "fimex${MINUS_FIMEX_VERSION}")
SET(FIMEX_INSTALL_INCLUDEDIR "${CMAKE_INSTALL_INCLUDEDIR}${pc_fimex_includedir}")


SET(FIMEX_PC "fimex${MINUS_FIMEX_VERSION}.pc")
FIMEX_GEN_PKGCONFIG(fimex.pc.in "${FIMEX_PC}"
  "${FIMEX_PC_REQUIRES}"
  "${grib_api_LIB};${libxml2_LIB};${netcdf_LIB};${proj_LIB};${udunits2_LIB};${eccodes_LIB}"
  "${CMAKE_INSTALL_LIBDIR}"
  "${FIMEX_INSTALL_INCLUDEDIR};${udunits2_INC_DIR};${proj_INC_DIR};${grib_api_INC_DIR}"
)
INSTALL(FILES "${CMAKE_BINARY_DIR}/${FIMEX_PC}" DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")

ENABLE_TESTING()

ADD_SUBDIRECTORY(include)
ADD_SUBDIRECTORY(src)
IF(ENABLE_FORTRAN)
  ADD_SUBDIRECTORY(modules/F90)
ENDIF()
#IF(ENABLE_PYTHON)
#  ADD_SUBDIRECTORY(modules/python)
#ENDIF()

IF (NOT USE_BOOST_UNIT_TEST)
  SET (MI_CPPTEST_3RDPARTY "third_party/mi-cpptest")
  IF ((NOT mi-cpptest_DIR) AND (EXISTS "${CMAKE_SOURCE_DIR}/${MI_CPPTEST_3RDPARTY}/CMakeLists.txt"))
    ADD_SUBDIRECTORY(${MI_CPPTEST_3RDPARTY})
  ELSE ()
    FIND_PACKAGE(mi-cpptest REQUIRED)
  ENDIF ()
ENDIF ()

ADD_SUBDIRECTORY(test)
ADD_SUBDIRECTORY(share/etc)

FIND_PACKAGE(Doxygen)
IF(DOXYGEN_FOUND)
  CONFIGURE_FILE("Doxyfile.in" "Doxyfile" @ONLY)
  ADD_CUSTOM_TARGET(doc
    ${DOXYGEN_EXECUTABLE} "${CMAKE_BINARY_DIR}/Doxyfile"
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Generating documentation with Doxygen" VERBATIM
    )
ENDIF()
