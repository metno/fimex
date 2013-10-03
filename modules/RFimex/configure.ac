AC_INIT([RFimex], 0.03) dnl package name, version, check man/RFimex.RD version

dnl A user-specifiable option

dnl Select an optional include path, from a configure option
dnl or from an environment variable.
dnl TODO: use package-config
AC_ARG_WITH([fimex-include],
    AC_HELP_STRING([--with-fimex-include=INCLUDE_PATH],
                   [the location of fimex header files]),
                   [fimex_include_path=$withval])
FIMEX_CPPFLAGS="-I."
if test [ -n "$fimex_include_path" ] ; then
    FIMEX_CPPFLAGS="-I. -I${fimex_include_path}"
else
    FIMEX_CPPFLAGS="-I. "`pkg-config fimex --cflags`
fi

dnl ditto for a library path
AC_ARG_WITH([fimex-lib],
            AC_HELP_STRING([--with-fimex-lib=LIB_PATH],
                           [the location of Fimex libraries]),
            [fimex_lib_path=$withval])
if test [ -n "$fimex_lib_path" ] ; then
    LIBS="-L$fimex_lib_path ${LIBS}"
else
    LIBS=`pkg-config fimex --libs`
fi

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS` $FIMEX_CPPFLAGS
AC_PROG_CC
AC_PROG_CPP

dnl Check the headers can be found
AC_LANG_PUSH(C++)
CPPFLAGS=$FIMEX_CPPFLAGS
AC_CHECK_HEADERS(fimex/CDMReader.h)
AC_LANG_POP(C++)
if test "${ac_cv_header_CDMReader_h}" = no; then
   AC_MSG_ERROR("fimex headers CDMReader.h not found")
fi

dnl search for a library containing an ODBC function
dnl substitute CPPFLAGS and LIBS
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
#AC_CONFIG_HEADERS([src/config.h])
dnl and do substitution in the src/Makevars.in and src/config.h
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

