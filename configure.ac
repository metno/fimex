AC_INIT([utplukk], [0.1], [heiko.klein@met.no])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB

AM_PATH_XML2(2.5.0,,AC_MSG_ERROR([
*** Libxml is required to build utplukk; Available at
http://www.libxml.org/.]))
AX_BOOST_BASE([1.33.1])
AX_BOOST_FILESYSTEM
AX_BOOST_REGEX
AX_BOOST_UNIT_TEST_FRAMEWORK

DX_INIT_DOXYGEN('Utplukk', 'Doxyfile', 'share/doc')

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile src/Makefile src/felt_reader/Makefile test/Makefile])
AC_OUTPUT
