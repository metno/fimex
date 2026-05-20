#! /bin/sh

TEST_SRCDIR=$(dirname $0)
TOP_SRCDIR="${TEST_SRCDIR}/.."

TEST="fiIndexNc"
echo "testing $TEST"

E=0

# ---- NetCDF 4 (HDF5) test ---------------------------------------------------

NCSRC="grib/gfs/gfs_0p25_20240717_00.nc"
NCFILE="$TEST_EXTRADATA_DIR/$NCSRC"
NCIDX="$(basename $NCFILE)fp"

if test ! -f "$NCFILE" ; then
    echo "SKIP test $TEST (NC4) as input file '$NCFILE' is not present"
else
    NCDIR="$(dirname $NCFILE)"
    CONFIG_XML="<?xml version=\"1.0\" encoding=\"UTF-8\"?><cdm_fimex_index_reader_config><root_path>$TEST_EXTRADATA_DIR</root_path></cdm_fimex_index_reader_config>"

    rm -f "$NCIDX"
    ./fiIndexNc.sh --input.file "$NCFILE" --input.src "$NCSRC" --output.file "$NCIDX"
    if [ $? != 0 ]; then
        echo "FAIL error writing '$NCIDX'"
        E=1
    elif [ ! -f "$NCIDX" ]; then
        echo "FAIL missing output file '$NCIDX'"
        E=1
    elif ./cdmcmp -c2 "$CONFIG_XML" "$NCFILE" "$NCIDX" ; then
        echo "success NC4"
    else
        echo "FAIL diff $TEST (NC4)"
        E=1
    fi
    rm -f "$NCIDX"
fi

# ---- NetCDF 3 (CDF-1) test --------------------------------------------------

NC3FILE="${TEST_SRCDIR}/verticalOceanSG2.nc"
NC3IDX="verticalOceanSG2.ncfp"
NC3DIR="${TEST_SRCDIR}"

rm -f "$NC3IDX"
./fiIndexNc.sh --input.file "$NC3FILE" --output.file "$NC3IDX"
if [ $? != 0 ]; then
    echo "FAIL error writing '$NC3IDX'"
    E=1
elif [ ! -f "$NC3IDX" ]; then
    echo "FAIL missing output file '$NC3IDX'"
    E=1
elif ./cdmcmp "$NC3FILE" "$NC3IDX" ; then
    echo "success NC3"
else
    echo "FAIL diff $TEST (NC3)"
    E=1
fi
rm -f "$NC3IDX"

exit $E
