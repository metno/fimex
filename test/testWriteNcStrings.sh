#!/bin/bash
TEST_SRCDIR=$(dirname $0)
OUTFILE="writeNcString_out$$"
./fimex.sh \
    --input.file "$TEST_SRCDIR/writeNcStrings.ncml" \
    --output.file "$OUTFILE.nc" \
    --output.config '<?xml version="1.0"?><cdm_ncwriter_config><default filetype="netcdf4" /></cdm_ncwriter_config>'
if [ $? != 0 ]; then
    echo "failed to write NetCDF-4 file with strings"
    rm -vf "$OUTFILE.nc"
    exit 1
fi

./fimex.sh --input.file "$OUTFILE.nc" --input.printNcML="$OUTFILE.ncml"
if [ $? != 0 ]; then
    echo "failed to write ncml file with strings"
    rm -vf "$OUTFILE.nc" "$OUTFILE.ncml"
    exit 1
fi

cmp "$TEST_SRCDIR/writeNcStrings_ex.ncml" "$OUTFILE.ncml"
if [ $? != 0 ]; then
    echo "mismatch of expected and actual output ncml file with strings"
    rm -vf "$OUTFILE.nc" "$OUTFILE.ncml"
    exit 1
fi

rm -vf "$OUTFILE.nc" "$OUTFILE.ncml"
exit 0
