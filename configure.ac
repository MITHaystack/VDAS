#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(FULL-PACKAGE-NAME, VERSION, BUG-REPORT-ADDRESS)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lcppunit':
AC_CHECK_LIB([cppunit], [main])
# FIXME: Replace `main' with a function in `-ldl':
AC_CHECK_LIB([dl], [main])
# FIXME: Replace `main' with a function in `-llog4cxx':
AC_CHECK_LIB([log4cxx], [main])
# FIXME: Replace `main' with a function in `-lm':
AC_CHECK_LIB([m], [main])
# FIXME: Replace `main' with a function in `-lpthread':
AC_CHECK_LIB([pthread], [main])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h netinet/in.h stdlib.h string.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_CHECK_FUNCS([gettimeofday memset socket strerror])

AM_INIT_AUTOMAKE(mark6, 0.1)

AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 src/Makefile
		src/common/Makefile
		src/net2disk/Makefile
		src/net2raid/Makefile
		src/disk2vlbi/Makefile
		src/scancheck/Makefile
		src/demo/Makefile
		])

AC_SUBST(BOOST_LIBDIR)
AC_SUBST(BOOST_INCDIR)
AC_SUBST(PFRING_ROOT)

AC_OUTPUT
