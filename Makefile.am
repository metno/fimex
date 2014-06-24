ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src include test share/etc modules/F90

#include doxygen_aminclude.am

EXTRA_DIST = doxydoc.txt libstdc++.tag share/doc/examples \
			 modules/F90 \
			 modules/RFimex \
			 modules/fimex.i \
             share/scripts/fiGribCut.pl \
             share/scripts/fiMisSubsetter.pl \
             share/scripts/newProjection.pl \
             share/scripts/fiConfigOverview.pl
             
dist-hook:
	rm -rf `find $(distdir)/share/doc/examples -type d -name .svn`
	rm -rf `find $(distdir)/modules -type d -name .svn`

DISTCHECK_CONFIGURE_FLAGS=--enable-openmp --enable-fortran --with-udunits2=/opt/udunits2.1.20/ --with-netcdf=/opt/netcdf4.3.0 CFLAGS='-O2 -mfpmath=sse -msse2' CXXFLAGS='-O2 -mfpmath=sse -msse2'

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = @PACKAGE_TARNAME@.pc

$(pkgconfig_DATA): config.status

if HAVE_LIBPQ 
test_wdb:
	cd test; $(MAKE) test_wdb
endif

include m4/doxygen.mk

CLEANFILES = $(DX_CLEANFILES)

include m4/debian.mk
