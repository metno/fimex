#!/bin/bash
set -e

TEST_SRCDIR=$(dirname $0)
TOP_SRCDIR="${TEST_SRCDIR}/.."

TOOLS="`dirname $0`/../run"
TOOLS="`realpath $TOOLS`"

FILE_GRBML=gefs_gribfx_0p5_20240717.grbml
FILE_GRBFP=gefs_gribfx_0p5_20240717.grbfp

GRIB_ROOT_PATH="$TEST_EXTRADATA_DIR/grib/gefs/gefs_0p5_20240717_00"
CONFIG_GRBFP="<?xml version=\"1.0\" encoding=\"UTF-8\"?><cdm_fimex_index_reader_config><root_path>$GRIB_ROOT_PATH</root_path></cdm_fimex_index_reader_config>"

GRBML_CONFIG="$TEST_EXTRADATA_DIR/grib/gefs/cdmGribReaderConfigGEFS.xml"
if test ! -r "$GRBML_CONFIG"; then
   echo "SKIP missing optional GEFS FimexIndex test case"
   exit 0
fi

./fiIndexGribs.sh \
    -c "$GRBML_CONFIG" \
    --output.file "$FILE_GRBFP" \
    "$GRIB_ROOT_PATH/"*

EXP_NC="$TEST_EXTRADATA_DIR/grib/gefs/gefs_0p5_20240717_00.nc"
if ./cdmcmp -c1 "$CONFIG_GRBFP" "$FILE_GRBFP" "$EXP_NC" ; then
    echo "success"
    E=0
else
    echo "failed diff $TESTCASE for $IN"
    E=1
fi
rm -f "$FILE_GRBML" "$FILE_GRBFP"
exit $E
