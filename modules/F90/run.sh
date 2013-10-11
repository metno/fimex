#!/bin/bash
FIMEXDIR=/disk1/fimexTrunk/
set -x
gcc -g -I$FIMEXDIR/include -c c_mifi.c  && \
gfortran -g -c mifi.f90 && \
gcc -g -o fortran_test fortran_test.f90 mifi.o c_mifi.o -L$FIMEXDIR/lib -lfimex -Wl,-rpath,$FIMEXDIR/lib -lgfortran -lstdc++ && \
./fortran_test
