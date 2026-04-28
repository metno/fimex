# Fimex, cmake/MetnoProtobuf.cmake
#
# Copyright (C) 2018-2026 met.no
#
# Contact information:
# Norwegian Meteorological Institute
# Box 43 Blindern
# 0313 OSLO
# NORWAY
# email: diana@met.no
#
# Project Info:  https://github.com/metno/fimex/wiki
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


FUNCTION (protoc_generated out bindir)
  SET (_out ${${out}})
  FOREACH (f IN LISTS ARGN)
    LIST (APPEND _out "${bindir}/${f}.pb.cc" "${bindir}/${f}.pb.h")
    SET_PROPERTY(SOURCE "${bindir}/${f}.pb.cc" "${bindir}/${f}.pb.h" PROPERTY SKIP_AUTOGEN ON)
  ENDFOREACH()
  SET("${out}" "${_out}" PARENT_SCOPE)
ENDFUNCTION ()

FUNCTION (list_prepend_dir lst dir)
  SET (_lst ${${lst}})
  FOREACH (p IN LISTS ARGN)
    LIST (APPEND _lst "${dir}/${p}")
  ENDFOREACH ()
  SET("${lst}" "${_lst}" PARENT_SCOPE)
ENDFUNCTION ()

FUNCTION (protoc_generate gen protos includes)
  FOREACH (inc ${includes})
    LIST (APPEND iincludes "-I/${inc}")
  ENDFOREACH ()
  FOREACH (p ${protos})
    LIST (APPEND dprotos "${metno_protos_dir}/${p}")
  ENDFOREACH ()

  ADD_CUSTOM_COMMAND(
    OUTPUT ${gen}
    COMMAND "${_PROTOBUF_PROTOC}"
    ARGS --cpp_out "${CMAKE_CURRENT_BINARY_DIR}"
         ${iincludes}
         ${protos}
    DEPENDS ${dprotos}
    WORKING_DIRECTORY "${metno_protos_dir}"
  )
ENDFUNCTION()
