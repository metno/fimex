# Fimex, cmake/FimexUtils.cmake
#
# Copyright (C) 2018-2022 met.no
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


MACRO(FIMEX_CMAKE_SETUP)
  IF(CMAKE_VERSION VERSION_LESS "3.1")
    IF(CMAKE_C_COMPILER_ID STREQUAL "GNU")
      SET(CMAKE_C_FLAGS "--std=c99 ${CMAKE_C_FLAGS}")
    ELSEIF (CMAKE_C_COMPILER_ID STREQUAL "Intel")
      SET(CMAKE_C_FLAGS "-std=c99 ${CMAKE_C_FLAGS}")
    ELSE()
      MESSAGE(WARNING "Do not know how to set C99 for compiler '${CMAKE_C_COMPILER_ID}'. Please set the CFLAGS environment variable before running cmake.")
    ENDIF()
  ELSE()
    SET(CMAKE_C_STANDARD 99)
  ENDIF()

  INCLUDE(CMakePackageConfigHelpers)
  INCLUDE(GNUInstallDirs)
  IF (${CMAKE_VERSION} VERSION_LESS "3.12.0")
    # see https://github.com/Kitware/CMake/commit/ac5731a7e380349f19dc319e6c31e189b5faba93
    INCLUDE(FindPkgConfigBugfixLibraryPaths)
  ELSE ()
    INCLUDE(FindPkgConfig)
  ENDIF ()

  SET(CMAKE_CXX_STANDARD 11)
  SET(CMAKE_CXX_STANDARD_REQUIRED ON)
  IF ((CMAKE_VERSION VERSION_LESS 3.6) AND (CMAKE_CXX_COMPILER_ID STREQUAL "Intel"))
    SET(CMAKE_CXX_FLAGS "-std=c++11 ${CMAKE_CXX_FLAGS}")
  ENDIF()

  # see https://gitlab.kitware.com/cmake/community/wikis/doc/cmake/RPATH-handling
  SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
  LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}" INSTALL_LIBDIR_IS_SYSTEM_DIR)
  IF("${INSTALL_LIBDIR_IS_SYSTEM_DIR}" STREQUAL "-1")
    LIST(APPEND CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
  ENDIF()
ENDMACRO()

#########################################################################

FUNCTION(FIMEX_GEN_PKGCONFIG pc_in pc_out pc_deps pc_libs_ pc_libdirs_ pc_includedirs_)
  STRING(REPLACE ";" ", " pc_deps "${pc_deps}")
  STRING(REPLACE ">=" " >= " pc_deps "${pc_deps}")

  FOREACH(X ${pc_libs_})
    SET(pc_libs "${pc_libs} -l${X}")
  ENDFOREACH()

  FOREACH(X ${pc_libdirs_})
    IF((${X} STREQUAL "/usr/lib") OR (${X} STREQUAL "/usr/lib64"))
      # skip
    ELSE()
      SET(pc_libdirs "${pc_libdirs} -L${X}")
    ENDIF()
  ENDFOREACH()

  FOREACH(X ${pc_includedirs_})
    IF(${X} STREQUAL "/usr/include")
      # skip
    ELSE()
      SET(pc_includedirs "${pc_includedirs} -I${X}")
    ENDIF()
  ENDFOREACH()

  CONFIGURE_FILE(${pc_in} ${pc_out} @ONLY)
  INSTALL(FILES ${CMAKE_BINARY_DIR}/${pc_out} DESTINATION "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/pkgconfig")
ENDFUNCTION()

########################################################################

FUNCTION(FIMEX_VERSION_DEFINES pack header_file)
  FILE(STRINGS "${header_file}" version_definitions REGEX "^#define.*_VERSION_(MAJOR|MINOR|PATCH|STATUS) +")
  FOREACH(v_def ${version_definitions})
    STRING(REGEX REPLACE "^#define.*_VERSION_(MAJOR|MINOR|PATCH|STATUS) +([0-9A-Fx]+) *$" \\1 v_type   "${v_def}")
    STRING(REGEX REPLACE "^#define.*_VERSION_(MAJOR|MINOR|PATCH|STATUS) +([0-9A-Fx]+) *$" \\2 v_number "${v_def}")
    SET(version_${v_type} "${v_number}")
  ENDFOREACH()

  SET(${pack}_VERSION_MAJOR "${version_MAJOR}" PARENT_SCOPE)
  SET(${pack}_VERSION_MINOR "${version_MINOR}" PARENT_SCOPE)
  SET(${pack}_VERSION_PATCH "${version_PATCH}" PARENT_SCOPE)


  # cmake does not understand hex numbers
  STRING(REGEX REPLACE "0x([ABCF])[0-9]" \\1 v_status_type   "${version_STATUS}")
  STRING(REGEX REPLACE "0x[ABCF]([0-9])" \\1 v_status_number "${version_STATUS}")
  IF(${v_status_type} STREQUAL "A")
    MATH(EXPR v_status_dec 10*16)
    SET(v_status_string "~alpha${v_status_number}")
  ELSEIF(${v_status_type} STREQUAL "B")
    MATH(EXPR v_status_dec 11*16)
    SET(v_status_string "~beta${v_status_number}")
  ELSEIF(${v_status_type} STREQUAL "C")
    MATH(EXPR v_status_dec 12*16)
    SET(v_status_string "~rc${v_status_number}")
  ELSEIF(${v_status_type} STREQUAL "F")
    MATH(EXPR v_status_dec 15*16)
    SET(v_status_string "")
  ENDIF()
  MATH(EXPR v_status_dec ${v_status_dec}+${v_status_number})

  SET(${pack}_VERSION_STATUS_HEX "${version_STATUS}"  PARENT_SCOPE)
  SET(${pack}_VERSION_STATUS_DEC "${v_status_dec}"    PARENT_SCOPE)
  SET(${pack}_VERSION_STATUS_STR "${v_status_string}" PARENT_SCOPE)


  SET(${pack}_VERSION "${version_MAJOR}.${version_MINOR}" PARENT_SCOPE)
  SET(${pack}_VERSION_FULL "${version_MAJOR}.${version_MINOR}.${version_PATCH}${v_status_string}" PARENT_SCOPE)
ENDFUNCTION()

########################################################################

FUNCTION(FIMEX_HEADERS headers sources source_suffix header_suffix)
  FOREACH(src ${${sources}})
    STRING(REPLACE ${source_suffix} ${header_suffix} hdr ${src})
    LIST(APPEND headers ${hdr})
  ENDFOREACH()
ENDFUNCTION()


FUNCTION(FIMEX_ADD_LIBRARY name sources packages)
  MESSAGE(STATUS "adding shared lib '${name}' using packages '${packages}'")
  SET(shared_lib lib${name})
  ADD_LIBRARY(${shared_lib} SHARED ${sources})
  TARGET_LINK_LIBRARIES(${shared_lib} PRIVATE ${packages})
  TARGET_INCLUDE_DIRECTORIES(${shared_lib}
    PUBLIC
    $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${FIMEX_INSTALL_INCLUDEDIR}>
    PRIVATE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    )
  IF(options)
    TARGET_COMPILE_OPTIONS(${shared_lib} PUBLIC ${options})
  ENDIF()
  SET_TARGET_PROPERTIES(${shared_lib} PROPERTIES
    VERSION ${lib_version}
    SOVERSION ${lib_soversion}
    OUTPUT_NAME "${name}${MINUS_FIMEX_VERSION}"
    )
  INSTALL(TARGETS ${shared_lib}
    EXPORT ${name}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
ENDFUNCTION()


INCLUDE(CheckFunctionExists)

FUNCTION(CHECK_NETCDF_HAS_HDF5 found)
  SET(CMAKE_REQUIRED_FLAGS "")
  SET(CMAKE_REQUIRED_DEFINITIONS "")
  SET(CMAKE_REQUIRED_INCLUDES "")
  SET(CMAKE_REQUIRED_LIBRARIES ${netCDF_PACKAGE})
  CHECK_FUNCTION_EXISTS("nc_def_var_deflate" ${found})
ENDFUNCTION()

FUNCTION(FIMEX_ADD_IMPORTED_LIBRARY name libraries includes definitions)
  add_library(${name} INTERFACE IMPORTED)
  set_target_properties(
    ${name}
    PROPERTIES
      INTERFACE_LINK_LIBRARIES "${libraries}"
      INTERFACE_INCLUDE_DIRECTORIES "${includes}"
      INTERFACE_COMPILE_DEFINITIIONS "${definitions}"
  )
ENDFUNCTION()

FUNCTION(FIMEX_FIND_PACKAGE ffp)
  set(ARGS0
    # options
  )
  set(ARGS1
    # arguments with one value
    PACKAGE
    VERSION_MIN
    CMAKE_NAME
    PKGCONFIG_NAME
    LIBRARY_NAME
  )
  set(ARGSN
    # arguments with multiple values
    CMAKE_TARGETS
    INCLUDE_HDR
  )
  CMAKE_PARSE_ARGUMENTS(_ffp "${ARGS0}" "${ARGS1}" "${ARGSN}" ${ARGN})

  # try to find via cmake
  IF (_ffp_CMAKE_NAME)
    FIND_PACKAGE(${_ffp_CMAKE_NAME} ${_ffp_VERSION_MIN} QUIET)
    IF(${_ffp_CMAKE_NAME}_FOUND)
      SET(${ffp}_PACKAGE_VERSION "${${_ffp_CMAKE_NAME}_VERSION}" PARENT_SCOPE)
      IF (NOT(DEFINED _ffp_CMAKE_TARGETS))
        SET(_ffp_CMAKE_TARGET ${_ffp_CMAKE_NAME})
      ENDIF()
      FOREACH(_ffp_target ${_ffp_CMAKE_TARGETS})
        IF(TARGET ${_ffp_target})
          GET_TARGET_PROPERTY(_ffp_is_imported "${_ffp_target}" IMPORTED)
          IF(_ffp_is_imported)
            SET(${ffp}_PACKAGE "${_ffp_target}" PARENT_SCOPE)
            MESSAGE(STATUS "Found ${ffp} via cmake imported")
            RETURN()
          ENDIF()
        ENDIF()
      ENDFOREACH ()

      FIMEX_ADD_IMPORTED_LIBRARY(${ffp}_IMP
        "${${_ffp_CMAKE_TARGET}_LIBRARIES}"
        "${${_ffp_CMAKE_TARGET}_INCLUDE_DIRS}"
        "${${_ffp_CMAKE_TARGET}_DEFINITIONS}"
      )
      SET(${ffp}_PACKAGE "${ffp}_IMP" PARENT_SCOPE)
      MESSAGE(STATUS "Found ${ffp} via cmake")
      RETURN()
    ENDIF()
  ENDIF()


  # try to find via pkg-config
  IF(_ffp_PKGCONFIG_NAME)
    IF(_ffp_VERSION_MIN)
      SET(_ffp_pc_version "${_ffp_PKGCONFIG_NAME}>=${_ffp_VERSION_MIN}")
    ELSE()
      SET(_ffp_pc_version "${_ffp_PKGCONFIG_NAME}")
    ENDIF()
    SET (_ffp_pc ${_ffp_PKGCONFIG_NAME})
    UNSET(${_ffp_pc}_FOUND CACHE)
    PKG_CHECK_MODULES(${_ffp_pc} IMPORTED_TARGET QUIET "${_ffp_pc_version}")
    IF(${_ffp_pc}_FOUND)
      SET(${ffp}_PACKAGE_VERSION "${${_ffp_pc}_VERSION}" PARENT_SCOPE)
      SET(${ffp}_PACKAGE "PkgConfig::${_ffp_pc}" PARENT_SCOPE)
      MESSAGE(STATUS "Found ${ffp} via pkg-config '${_ffp_pc_version}'")
      RETURN()
    ENDIF()
  ENDIF()

  # try to find via library and header
  UNSET(_ffp_inc_dir CACHE)
  FIND_PATH(_ffp_inc_dir
    NAMES ${_ffp_INCLUDE_HDR}
    HINTS "${${ffp}_INC_DIR}" "${${ffp}_DIR}/include"
  )
  IF (NOT _ffp_inc_dir)
    MESSAGE(FATAL_ERROR "Cannot find ${ffp}, include header '${_ffp_INCLUDE_HDR}' not found")
  ENDIF()
  UNSET(_ffp_lib CACHE)
  IF (_ffp_LIBRARY_NAME)
    FIND_LIBRARY(_ffp_lib
      NAMES ${_ffp_LIBRARY_NAME}
      HINTS "${${ffp}_LIB_DIR}" "${${ffp}_DIR}/lib"
    )
    IF (NOT _ffp_lib)
      MESSAGE(FATAL_ERROR "Cannot find ${ffp}, library '${_ffp_LIBRARY_NAME}' not found")
    ENDIF()
  ENDIF()
  FIMEX_ADD_IMPORTED_LIBRARY(${ffp}_LIB_INC "${_ffp_lib}" "${_ffp_inc_dir}" "")
  SET(${ffp}_PACKAGE "${ffp}_LIB_INC" PARENT_SCOPE)
  SET(${ffp}_PACKAGE_VERSION "" PARENT_SCOPE)
  MESSAGE(STATUS "Found ${ffp} via find_package/find_path, lib='${_ffp_lib}', include='${_ffp_inc_dir}'")
ENDFUNCTION()
