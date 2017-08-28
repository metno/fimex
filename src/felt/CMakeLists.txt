
LIST(APPEND libfelt_SOURCES
  FeltField.cpp
  FeltFile.cpp
  FeltGridDefinition.cpp
  FeltTypeConversion.cpp
  )

FIMEX_ADD_LIBRARY(felt "${libfelt_SOURCES}" "" "${CMAKE_SOURCE_DIR}/include" "")

IF(TARGET libfelt)
  TARGET_INCLUDE_DIRECTORIES(libfelt PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
IF(TARGET libfelt-static)
  TARGET_INCLUDE_DIRECTORIES(libfelt-static PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
