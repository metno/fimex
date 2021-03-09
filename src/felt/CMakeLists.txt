
LIST(APPEND libfelt_SOURCES
  FeltField.cpp
  FeltFile.cpp
  FeltTime.cpp
  FeltGridDefinition.cpp
  )

SET(libfelt_PACKAGES
  ${date_PACKAGE}
  ${proj_PACKAGE}
)
FIMEX_ADD_LIBRARY(felt "${libfelt_SOURCES}" "${libfelt_PACKAGES}")

IF(TARGET libfelt)
  TARGET_INCLUDE_DIRECTORIES(libfelt PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
IF(TARGET libfelt-static)
  TARGET_INCLUDE_DIRECTORIES(libfelt-static PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
