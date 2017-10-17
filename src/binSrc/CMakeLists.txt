
FUNCTION(ADD_EXE
    name_
    libs_
    )
  ADD_EXECUTABLE(${name_} "${name_}.cc")
  SET_TARGET_PROPERTIES("${name_}" PROPERTIES
    OUTPUT_NAME "${name_}${MINUS_FIMEX_VERSION}"
    )
  TARGET_LINK_LIBRARIES("${name_}" ${libs_})
  INSTALL(TARGETS "${name_}" DESTINATION bin)
ENDFUNCTION()


SET(LIBRARIES_A
  libfimex
  ${PC_LIBRARIES}
  ${grib_api_LIB}
  ${libxml2_LIB}
  ${netcdf_LIB}
  ${proj_LIB}
  ${udunits2_LIB}
  ${eccodes_LIB}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  )
SET(LIBRARIES_B
  ${LIBRARIES_A}
  ${Boost_DATE_TIME_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_REGEX_LIBRARY}
  )

ADD_EXE(fimex        ${LIBRARIES_B})
ADD_EXE(fiXYcontents ${LIBRARIES_B})

IF((ENABLE_GRIBAPI) OR (ENABLE_ECCODES))
  ADD_EXE(fiIndexGribs ${LIBRARIES_B};${Boost_IOSTREAMS_LIBRARY};${Boost_SYSTEM_LIBRARY})
  ADD_EXE(fiGribCut    ${LIBRARIES_B};${Boost_IOSTREAMS_LIBRARY})
  ADD_EXE(fiGrbmlCat   ${LIBRARIES_A};${Boost_IOSTREAMS_LIBRARY})
ENDIF()
