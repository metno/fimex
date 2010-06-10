ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src include test share/etc

#include doxygen_aminclude.am

EXTRA_DIST = Doxyfile doxydoc.txt libstdc++.tag share/doc/examples share/doc/html debian_files

DISTCHECK_CONFIGURE_FLAGS=--with-udunits=/usr/local --with-NetCDF-lib-dir=/usr/lib --with-NetCDF-include-dir=/usr/include --with-proj4-lib-dir=/usr/lib --with-proj4-include-dir=/usr/include --with-gribapi --with-gribapi-lib-dir=/disk1/heiko/local/grib_api1.8/lib/ --with-gribapi-include-dir=/disk1/heiko/local/grib_api1.8/include

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = @PACKAGE_TARNAME@.pc

$(pkgconfig_DATA): config.status

include m4/debian.mk
