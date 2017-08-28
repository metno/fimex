
FILE(GLOB INCLUDE_fimex *.h)
INSTALL(FILES ${INCLUDE_fimex} DESTINATION "${FIMEX_INSTALL_INCLUDEDIR}/fimex")

FILE(GLOB INCLUDE_fimex_cs coordSys/*.h)
INSTALL(FILES ${INCLUDE_fimex_cs} DESTINATION "${FIMEX_INSTALL_INCLUDEDIR}/fimex/coordSys")

FILE(GLOB INCLUDE_fimex_cs_vt coordSys/verticalTransform/*.h)
INSTALL(FILES ${INCLUDE_fimex_cs_vt} DESTINATION "${FIMEX_INSTALL_INCLUDEDIR}/fimex/coordSys/verticalTransform")
