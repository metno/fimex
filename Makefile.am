ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src include test share/etc

include doxygen_aminclude.am

EXTRA_DIST = $(DX_CONFIG) libstdc++.tag share/doc/html

DISTCHECK_CONFIGURE_FLAGS=--with-NetCDF-lib-dir=/usr/lib/netcdf-3 --with-NetCDF-include-dir=/usr/include/netcdf-3

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = @PACKAGE_TARNAME@.pc

$(pkgconfig_DATA): config.status
