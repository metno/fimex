CONFIGURE_FILE(fimex_felt_config.h.in fimex_felt_config.h)

SET(HAVE_FELT 1)

SET(libfimex_felt_SOURCES
  # from libfelt
  FeltField.cpp
  FeltFile.cpp
  FeltTime.cpp
  FeltGridDefinition.cpp

  # from fimex felt reader
  Felt_Array2.cc
  FeltCDMReader2.cc
  Felt_File2.cc
  FeltParameters.cc

  Felt_Array2.h
  FeltCDMReader2.h
  Felt_File_Error.h
  Felt_File2.h
  FeltParameters.h

  FeltIoFactory.cc
  FeltIoFactory.h
  )

FIMEX_ADD_LIBRARY(fimex-io-felt "${libfimex_felt_SOURCES}" "${IO_PACKAGES};${date_PACKAGE};${proj_PACKAGE};${FELT_LIBRARY}")
TARGET_INCLUDE_DIRECTORIES(libfimex-io-felt PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
