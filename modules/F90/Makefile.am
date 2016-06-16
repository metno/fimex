AM_FCFLAGS = 

if ENABLE_FORTRAN

lib_LTLIBRARIES = libfimexf.la
libfimexf_la_LDFLAGS = -version-info 0:0:0 -release $(VERSION) @BOOST_LDFLAGS@ @MIFI_PROJ_LDFLAGS@
libfimexf_la_LIBADD =
libfimexf_la_SOURCES = fimex.f90 fimex2d.F90

fimexfincdir = $(includedir)
nodist_fimexfinc_HEADERS= fimex.mod fimex2d.mod

# it looks like we need to add dependencies manually to make parallel
# builds work: gfortran/ifort write .mod files, but automake does not
# seem to be aware of this
fimex.mod: fimex.o
fimex2d.mod: fimex2d.o
# fimex.mod is required to compile fimex2d.F90
fimex2d.F90: | fimex.mod

CLEANFILES = fimex.mod fimex2d.mod fimex2d_example_all.nc4 fimex2d_example.nc4 fimex2d_example

endif
