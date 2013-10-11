#!/bin/bash
FIMEXDIR=/disk1/fimexTrunk/
set -x
gfortran -g -c mifi.f90 && \
gfortran -g -o fortran_test fortran_test.f90 mifi.o -L$FIMEXDIR/lib -lfimex -Wl,-rpath,$FIMEXDIR/lib && \
./fortran_test
