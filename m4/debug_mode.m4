AC_DEFUN([DEBUG_MODE],
[
	AC_ARG_ENABLE(debug_mode,
     	AC_HELP_STRING( [--enable-debug-mode], [Compile program in debugging mode, using no optimization, debugging flags, and removing try-catch block in main functio.] ),
     			[case "${enableval}" in
       				yes) debug_mode=true ;;
       				no)  debug_mode=false ;;
       				*) AC_MSG_ERROR(bad value ${enableval} for --enable-debug-mode) ;;
     			 esac],
     			 [debug_mode=false])
    if test x$debug_mode = xtrue; then
    	AC_DEFINE(DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN, [], [If defined, this should remove the try-catch block from the main function, causing any exceptions to crash the program.])
    	CXXFLAGS="-g -O0 -Wall"
    fi
])
