
LINK_DIRECTORIES(${PC_LIBRARY_DIRS} # for proj
  ${Boost_LIBRARY_DIRS})

LIST(APPEND libfelt_SOURCES
  FeltField.cpp
  FeltFile.cpp
  FeltGridDefinition.cpp
  FeltTypeConversion.cpp
  )

SET (libfelt_INCLUDE_DIRS
  ${Boost_INCLUDE_DIRS}
  ${PC_INCLUDE_DIRS} # for proj
  ${proj_INC_DIR}
  ${CMAKE_SOURCE_DIR}/include
)

SET (libfelt_LIBS
  ${PC_LIBRARIES} # for proj
  ${proj_LIB}
  ${Boost_DATE_TIME_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
)

FIMEX_ADD_LIBRARY(felt "${libfelt_SOURCES}" "${libfelt_LIBS}" "${libfelt_INCLUDE_DIRS}" "" "")

IF(TARGET libfelt)
  TARGET_INCLUDE_DIRECTORIES(libfelt PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
IF(TARGET libfelt-static)
  TARGET_INCLUDE_DIRECTORIES(libfelt-static PRIVATE "${CMAKE_BINARY_DIR}/src")
ENDIF()
