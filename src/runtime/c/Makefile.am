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
	pgf/vector.cxx \
	pgf/vector.h \
	pgf/heap.h \
	pgf/pgf.cxx \
	pgf/reader.cxx \
	pgf/reader.h \
	pgf/writer.cxx \
	pgf/writer.h \
	pgf/printer.cxx \
	pgf/printer.h \
	pgf/typechecker.cxx \
	pgf/typechecker.h \
	pgf/linearizer.cxx \
	pgf/linearizer.h \
	pgf/aligner.cxx \
	pgf/aligner.h \
	pgf/parser.cxx \
	pgf/parser.h \
	pgf/graphviz.cxx \
	pgf/graphviz.h \
	pgf/data.cxx \
	pgf/data.h \
	pgf/expr.cxx \
	pgf/expr.h \
	pgf/namespace.h \
	pgf/phrasetable.cxx \
	pgf/phrasetable.h \
	pgf/probspace.cxx \
	pgf/probspace.h \
	pgf/generator.cxx \
	pgf/generator.h \
	pgf/md5.cxx \
	pgf/md5.h

libpgf_la_LDFLAGS = -no-undefined -version-info 4:0:0
libpgf_la_CXXFLAGS = -fno-rtti -std=c++11 -DCOMPILING_PGF

bin_PROGRAMS =

AUTOMAKE_OPTIONS = foreign subdir-objects dist-bzip2
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = \
	libpgf.pc.in
