ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src include test share/etc

#include doxygen_aminclude.am

EXTRA_DIST = doxydoc.txt libstdc++.tag share/doc/examples \
             share/doc/scripts/fiGribCut.pl \
             share/doc/scripts/fiMisSubsetter.pl \
             share/doc/scripts/newProjection.pl \
             share/doc/scripts/fiConfigOverview.pl

DISTCHECK_CONFIGURE_FLAGS=--enable-openmp --with-udunits2=/opt/udunits2.1.20/ --with-netcdf=/opt/netcdf4.1.1 CFLAGS='-O2 -mfpmath=sse -msse2' CXXFLAGS='-O2 -mfpmath=sse -msse2'

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = @PACKAGE_TARNAME@.pc

$(pkgconfig_DATA): config.status

if HAVE_LIBPQ 
test_wdb: 
	cd test; $(MAKE) test_wdb
endif


include m4/debian.mk
