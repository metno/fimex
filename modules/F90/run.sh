#!/bin/bash
FIMEXDIR=/disk1/fimexTrunk/
set -x
gfortran -g -c mifi.f90 && \
gfortran -g -o fortran_test fortran_test.f90 mifi.o _gfortran_iargc.o -L$FIMEXDIR/lib -lfimex -Wl,-rpath,$FIMEXDIR/lib && \
./fortran_test /opdata/arome_norway25/AROME_Norway25_00.nc netcdf surface_air_pressure hPa
