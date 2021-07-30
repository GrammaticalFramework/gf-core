lib_LTLIBRARIES = libpgf.la

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libpgf.pc

pgfincludedir=$(includedir)
pgfinclude_HEADERS = \
	pgf.h

libpgf_la_SOURCES = \
	db.cxx \
	db.h \
	text.cxx \
	text.h \
	pgf.cxx \
	reader.cxx \
	reader.h \
	data.h \
	expr.h \
	namespace.h \
	variant.h

libpgf_la_LDFLAGS = "-no-undefined"

bin_PROGRAMS =

AUTOMAKE_OPTIONS = foreign subdir-objects dist-bzip2
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = \
	libpgf.pc.in
