#!/bin/bash

set -x
fimex=/usr

testfile=/lustre/storeB/project/metproduction/products/meps/meps_det_2_5km_20200310T12Z.ncml

ncgen -b fimex2d_example.cdl -o fimex2d_example.nc4
fimex --input.file=${testfile} --output.file=fimex2d_example_all.nc4 --extract.selectVariables=air_temperature_2m

# use 
# pkg-config --cflags fimex
# pkg-config --libs fimex
#   and guess -lfimexf version
#gfortran -O3 -Wall -Werror fimex2d_example.F90 -o fimex2d_example `pkg-config --cflags fimex` `pkg-config --libs fimex` -Wall -Werror -Wl,-rpath=$fimex/lib/ || exit 1

./fimex2d_example ${testfile} fimex2d_example.nc4 ncml || exit 1

vars="air_temperature_2m"
for var in $vars; do
  ncks -A -v $var fimex2d_example.nc4 fimex2d_example_all.nc4
done

ncview fimex2d_example_all.nc4
