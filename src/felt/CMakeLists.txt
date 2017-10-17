
LIST(APPEND libfelt_SOURCES
  FeltField.cpp
  FeltFile.cpp
  FeltGridDefinition.cpp
  FeltTypeConversion.cpp
  )

FIMEX_ADD_LIBRARY(felt "${libfelt_SOURCES}" "${proj_LIB}" "${CMAKE_SOURCE_DIR}/include;${proj_INC_DIR}" "" "")

IF(TARGET libfelt)
  TARGET_INCLUDE_DIRECTORIES(libfelt PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
IF(TARGET libfelt-static)
  TARGET_INCLUDE_DIRECTORIES(libfelt-static PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
