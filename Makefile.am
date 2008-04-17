ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src test

include doxygen_aminclude.am

EXTRA_DIST = $(DX_CONFIG) liststdc++.tag share/doc/html

dist_data_DATA = share/etc/felt2nc_variables.xml \
				 share/etc/felt2nc_variables_hirlam20.xml \
                 share/etc/damocles_felt2nc_variables.xml share/etc/damocles_felt_axes.xml share/etc/damocles_felt_global_attributes.xml \
				 share/etc/felt_axes.xml \
				 share/etc/felt_global_attributes.xml \
				 share/etc/felt_variables_hirlam20.xml

DISTCHECK_CONFIGURE_FLAGS=--with-NetCDF-lib-dir=/usr/lib/netcdf-3 --with-NetCDF-include-dir=/usr/include/netcdf-3