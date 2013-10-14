#!/bin/bash
FIMEXDIR=/disk1/fimexTrunk/
set -x
gfortran -g -c fimex.f90 && \
gcc -g -c _gfortran_iargc.c && \
gfortran -g -o fortran_test fortran_test.f90 fimex.o _gfortran_iargc.o -L$FIMEXDIR/lib -lfimex -Wl,-rpath,$FIMEXDIR/lib && \
./fortran_test /opdata/arome_norway25/AROME_Norway25_00.nc netcdf surface_air_pressure hPa
