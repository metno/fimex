
LINK_DIRECTORIES(
)

LIST(APPEND libfelt_SOURCES
  FeltField.cpp
  FeltFile.cpp
  FeltTime.cpp
  FeltGridDefinition.cpp
  )

SET (libfelt_INCLUDE_DIRS
  "${date_INC_DIR}"
)

SET (libfelt_LIBS
  ${date_LIB}
)

FIMEX_ADD_LIBRARY(felt "${libfelt_SOURCES}" "${libfelt_LIBS}" "${libfelt_INCLUDE_DIRS}" "")

IF(TARGET libfelt)
  TARGET_INCLUDE_DIRECTORIES(libfelt PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
IF(TARGET libfelt-static)
  TARGET_INCLUDE_DIRECTORIES(libfelt-static PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
