#!/bin/bash

set -x
fimex=/usr

ncgen -b fimex2d_example.cdl -o fimex2d_example.nc4
fimex --input.file=/opdata/arome2_5_main/AROME_MetCoOp_00_fp.nc --output.file=fimex2d_example_all.nc4 --extract.selectVariables=x --extract.selectVariables=y --extract.selectVariables=longitude --extract.selectVariables=latitude

#gcc -c _gfortran_iargc.c || exit 1
gfortran -O3 -Wall -Werror fimex2d_example.F90 -o fimex2d_example -I$fimex/include -I. -L$fimex/lib/ -lfimexf -lfimex -Wall -Werror -Wl,-rpath=$fimex/lib/ || exit 1

./fimex2d_example /opdata/arome2_5_main/AROME_MetCoOp_00_fp.nc fimex2d_example.nc4 netcdf || exit 1

vars="air_temperature_2m"
for var in $vars; do
  ncks -A -v $var fimex2d_example.nc4 fimex2d_example_all.nc4
done

ncview2 fimex2d_example_all.nc4
