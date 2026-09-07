#! /bin/sh

set -e

TEST_SRCDIR=$(dirname $0)
TESTCASE="grib reader output content"

echo "testing $TESTCASE"

if test -n "$TEST_EXTRADATA_DIR"; then
    TEST_EXTRADATADIR="${TEST_SRCDIR}"
fi

TEST_GRIB="${TEST_EXTRADATA_DIR}/grib"
if [ ! -d "${TEST_GRIB}" ]; then
   echo "SKIP missing optional '${TEST_GRIB}'"
   exit 0
fi

test1() {
    IN="$1" ; shift
    CFG="$1" ; shift

    EXP_NC="${TEST_GRIB}/$IN.nc"
    if test ! -f "$EXP_NC" ; then
        echo "skip testcase $IN as $EXP_NC is not found"
        return 0
    fi

    if ./cdmcmp -t1 grib -c1 "${TEST_GRIB}/$CFG" "glob:${TEST_GRIB}/$IN/*" "$EXP_NC" ; then
        echo "success"
        E=0
    else
        echo "failed diff $TESTCASE for $IN"
        E=1
    fi
    return $E
}

test1 "gefs/gefs_0p5_20240717_00" "gefs/cdmGribReaderConfigGEFS.xml"
test1 "gfs/gfs_0p25_20240717_00" "gfs/cdmGribReaderConfigGFS.xml"

test1 "meps/meps_fc2024071906_small" "meps/AromeEPSGribReaderConfig.xml"
