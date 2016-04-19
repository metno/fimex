AM_FCFLAGS = 

if ENABLE_FORTRAN

lib_LTLIBRARIES = libfimexf.la
libfimexf_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) @BOOST_LDFLAGS@ @MIFI_PROJ_LDFLAGS@
libfimexf_la_LIBADD =
libfimexf_la_SOURCES = fimex.f90 fimex2d.F90

fimexfincdir = $(includedir)
fimexfinc_HEADERS= $(top_builddir)/modules/F90/fimex.mod $(top_builddir)/modules/F90/fimex2d.mod

$(top_builddir)/modules/F90/fimex.mod: fimex.o
$(top_builddir)/modules/F90/fimex2d.mod: fimex.mod fimex2d.o

CLEANFILES = fimex.mod fimex2d.mod fimex2d_example_all.nc4 fimex2d_example.nc4 fimex2d_example

endif