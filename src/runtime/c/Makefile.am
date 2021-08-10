lib_LTLIBRARIES = libpgf.la

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libpgf.pc

pgfincludedir=$(includedir)/pgf
pgfinclude_HEADERS = \
	pgf/pgf.h

libpgf_la_SOURCES = \
	pgf/db.cxx \
	pgf/db.h \
	pgf/text.cxx \
	pgf/text.h \
	pgf/pgf.cxx \
	pgf/reader.cxx \
	pgf/reader.h \
	pgf/data.h \
	pgf/expr.cxx \
	pgf/expr.h \
	pgf/namespace.h

libpgf_la_LDFLAGS = "-no-undefined"

bin_PROGRAMS =

AUTOMAKE_OPTIONS = foreign subdir-objects dist-bzip2
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = \
	libpgf.pc.in
