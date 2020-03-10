#!/bin/bash

set -ex

prefix=/usr

testfile="https://thredds.met.no/thredds/dodsC/mepslatest/meps_lagged_6_h_latest_2_5km_latest.nc"

EX2D="./fimex2d_example"

# download field (one variable, 4 time steps, 1 member)
if test ! -r "${EX2D}_all.nc4" ; then
    fimex-1.4 \
        --input.file "${testfile}" \
        --output.file "${EX2D}_all.nc4" \
        --extract.selectVariables air_temperature_2m \
        --extract.reduceDimension.name time --extract.reduceDimension.start 0 --extract.reduceDimension.end   3 \
        --extract.pickDimension.name ensemble_member --extract.pickDimension.list 0
fi

if test ! -f "$EX2D" ; then
    gfortran -O3 -Wall -Werror "$EX2D.F90" -o "$EX2D" `pkg-config --cflags --libs fimexf` -Wall -Werror -Wl,-rpath="$prefix/lib"
fi

# create empty template nc file
ncgen -b "$EX2D.cdl" -o "$EX2D.nc4"

# fill
"$EX2D" "${testfile}" "$EX2D.nc4" nc

#ncks -A -v air_temperature_2m "$EX2D.nc4" "${EX2D}_all.nc4"

ncview "$EX2D.nc4"
