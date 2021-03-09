
SET(PACKAGES
  libfimex
  mi-programoptions
  ${libxml2_PACKAGE}
  ${log4cpp_PACKAGE}
)

FUNCTION(ADD_EXE name_ packages_)
  ADD_EXECUTABLE("${name_}" "${name_}.cc")
  SET_TARGET_PROPERTIES("${name_}" PROPERTIES
    OUTPUT_NAME "${name_}${MINUS_FIMEX_VERSION}"
    )
  TARGET_LINK_LIBRARIES("${name_}" PRIVATE ${PACKAGES} ${packages_})
  INSTALL(TARGETS "${name_}" DESTINATION bin)
ENDFUNCTION()

INCLUDE_DIRECTORIES(
  "${CMAKE_SOURCE_DIR}/src" # for CDMMergeUtils.h, ...
  "${CMAKE_BINARY_DIR}/src" # for fimex_config.h
)

ADD_EXE(fimex "")
ADD_EXE(fiXYcontents "")

IF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))
  SET(GRIB_PACKAGES ${eccodes_PACKAGE} ${grib_api_PACKAGE})

  ADD_EXE(fiIndexGribs "${GRIB_PACKAGES}")
  ADD_EXE(fiGribCut    "${GRIB_PACKAGES}")
  ADD_EXE(fiGrbmlCat   "")
ENDIF()
