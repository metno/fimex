# Fimex, cmake/FimexUtils.cmake
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
  INCLUDE(FindPkgConfig)

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
    IF((${X} STREQUAL "/usr/lib") OR (${X} STREQUAL "/usr/lib/${CMAKE_INSTALL_LIBDIR}"))
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

  SET(pc_prefix "${CMAKE_INSTALL_PREFIX}")
  SET(pc_libdir "\${prefix}/${CMAKE_INSTALL_LIBDIR}")
  SET(pc_includedir "\${prefix}/${CMAKE_INSTALL_INCLUDEDIR}")

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


FUNCTION(FIMEX_ADD_LIBRARY name sources libs includes definitions options)
  IF(BUILD_SHARED_LIBS OR (BUILD_SHARED_LIBS MATCHES "[Bb][Oo][Tt][Hh]"))
    SET(shared_lib lib${name})
    ADD_LIBRARY(${shared_lib} SHARED ${sources})
    TARGET_LINK_LIBRARIES(${shared_lib} PRIVATE ${libs})
    TARGET_INCLUDE_DIRECTORIES(${shared_lib}
      PUBLIC
      $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
      $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
      $<INSTALL_INTERFACE:${FIMEX_INSTALL_INCLUDEDIR}>
      PRIVATE
      ${includes}
      )
    IF(definitions)
      TARGET_COMPILE_DEFINITIONS(${shared_lib} PUBLIC ${definitions})
    ENDIF()
    IF(options)
      TARGET_COMPILE_OPTIONS(${shared_lib} PUBLIC ${options})
    ENDIF()
    SET_TARGET_PROPERTIES(${shared_lib} PROPERTIES
      VERSION ${lib_version}
      SOVERSION ${lib_soversion}
      OUTPUT_NAME "${name}${MINUS_FIMEX_VERSION}"
      )
    INSTALL(TARGETS ${shared_lib}
      EXPORT fimex
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      )
  ENDIF()

  IF((NOT (BUILD_SHARED_LIBS)) OR (BUILD_SHARED_LIBS MATCHES "[Bb][Oo][Tt][Hh]"))
    SET(static_lib lib${name}-static)
    SET(lib${name}_STATICLIBS ${libs})
    ADD_LIBRARY(${static_lib} STATIC ${sources})
    TARGET_LINK_LIBRARIES(${static_lib} PRIVATE ${libs})
    TARGET_INCLUDE_DIRECTORIES(${static_lib}
      PUBLIC
      $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
      $<INSTALL_INTERFACE:${FIMEX_INSTALL_INCLUDEDIR}>
      PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_CURRENT_SOURCE_DIR}
      ${includes}
      )
    IF(definitions)
      TARGET_COMPILE_DEFINITIONS(${static_lib} PUBLIC ${definitions})
    ENDIF()
    SET_TARGET_PROPERTIES(${static_lib} PROPERTIES
      OUTPUT_NAME "${name}${MINUS_FIMEX_VERSION}"
      )
    INSTALL(TARGETS ${static_lib}
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      )
  ENDIF()
ENDFUNCTION()


INCLUDE(CheckFunctionExists)

FUNCTION(CHECK_NETCDF_HAS_HDF5 found)
  IF (netcdf_PC)
    # we want only netcdf
    PKG_CHECK_MODULES(PC_NETCDF QUIET REQUIRED ${netcdf_PC})
    SET(CMAKE_REQUIRED_FLAGS ${PC_NETCDF_LDFLAGS})
    SET(CMAKE_REQUIRED_DEFINITIONS ${PC_NETCDF_CFLAGS_OTHER})
    SET(CMAKE_REQUIRED_INCLUDES ${PC_NETCDF_INCLUDE_DIRS})
    SET(CMAKE_REQUIRED_LIBRARIES ${PC_NETCDF_LIBRARIES})
  ELSE ()
    SET(CMAKE_REQUIRED_INCLUDES ${netcdf_INC_DIR})
    SET(CMAKE_REQUIRED_LIBRARIES ${netcdf_LIB})
  ENDIF ()
  CHECK_FUNCTION_EXISTS("nc_def_var_deflate" ${found})
ENDFUNCTION()


FUNCTION(FIMEX_FIND_PACKAGE name
    pkg_pc
    pkg_libname
    pkg_hdr
    )

  UNSET(p_pc_FOUND CACHE)

  IF(pkg_pc)
    PKG_CHECK_MODULES(p_pc QUIET "${pkg_pc}")
  ENDIF()
  IF(p_pc_FOUND)
    MESSAGE(STATUS "Found ${name}: pkg-config '${pkg_pc}'")
    SET(${name}_PC ${pkg_pc} PARENT_SCOPE)
  ELSE()
    FIND_PATH(${name}_INC_DIR
      ${pkg_hdr}
      HINTS "${${name}_INCLUDE_DIR}" "${${name}_DIR}/include"
    )
    FIND_LIBRARY(${name}_LIB
      NAMES ${pkg_libname}
      HINTS "${${name}_LIB_DIR}" "${${name}_DIR}/lib"
    )
    IF((${name}_INC_DIR) AND (${name}_LIB))
      MESSAGE(STATUS "Found ${name}: include: '${${name}_INC_DIR}/${pkg_hdr}'  library: '${${name}_LIB}'")
    ELSE()
      MESSAGE(FATAL_ERROR "Required ${name} include/library not found")
    ENDIF()
  ENDIF()
ENDFUNCTION()
