AC_INIT(Portable Grammar Format library, 3.0-pre,
		 http://www.grammaticalframework.org/, 
		 libpgf)
AC_PREREQ(2.58)

AC_CONFIG_AUX_DIR([scripts])
AC_CONFIG_MACRO_DIR([m4])

AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE([1.7.9])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

PKG_PROG_PKG_CONFIG

AC_CONFIG_HEADERS([config.h])

AM_MAINTAINER_MODE([enable])
AC_CHECK_LIB(m,nan)
AC_CHECK_LIB(rt,nan)
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_PROG_CXX

[if [ "x$GCC" = "xyes" ]; then
  CFLAGS="$CFLAGS\
 -Wall\
 -Wextra\
 -Wno-missing-field-initializers\
 -Wno-unused-parameter\
 -Wno-unused-value"
fi]

AC_CONFIG_FILES([Makefile
		 libpgf.pc
		 ])

AC_OUTPUT
