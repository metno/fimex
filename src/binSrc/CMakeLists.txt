
SET(LIBRARIES
  libfimex
  mi-programoptions
  ${libxml2_LIB}
)

FUNCTION(ADD_EXE name_ includes_ libs_)
  ADD_EXECUTABLE("${name_}" "${name_}.cc")
  SET_TARGET_PROPERTIES("${name_}" PROPERTIES
    OUTPUT_NAME "${name_}${MINUS_FIMEX_VERSION}"
    )
  TARGET_LINK_LIBRARIES("${name_}" ${LIBRARIES} ${libs_})
  TARGET_INCLUDE_DIRECTORIES("${name_}" PRIVATE ${includes_})
  INSTALL(TARGETS "${name_}" DESTINATION bin)
ENDFUNCTION()

LINK_DIRECTORIES(${PC_LIBRARY_DIRS})
INCLUDE_DIRECTORIES(${PC_INCLUDE_DIRS})

ADD_EXE(fimex "" "${PC_LIBRARIES}")
ADD_EXE(fiXYcontents "" "")

IF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))
  SET(GRIB_INCDIRS ${grib_api_INC_DIR} ${ECCODES_INCLUDE_DIRS})
  SET(GRIB_LIBS ${grib_api_LIB} ${eccodes_LIB})

  ADD_EXE(fiIndexGribs "${GRIB_INCDIRS}" "${GRIB_LIBS}")
  ADD_EXE(fiGribCut "${GRIB_INCDIRS}" "${GRIB_LIBS}")
  ADD_EXE(fiGrbmlCat "" "${PC_LIBRARIES}")
ENDIF()
