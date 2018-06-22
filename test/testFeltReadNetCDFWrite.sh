#! /bin/sh
echo "testing conversion Felt to NetCDF"

TEST_SRCDIR=`dirname $0`
TEST_BINDIR=`pwd`
cd "${TEST_SRCDIR}"

if test -n "$TEST_EXTRADATA_DIR"; then
    TEST_EXTRADATADIR="${TEST_SRCDIR}"
fi

TEST_DAT="${TEST_EXTRADATA_DIR}/flth00.dat"
if [ ! -f "${TEST_DAT}" ]; then
   echo "no input data: '${TEST_DAT}', skipping test..."
   exit 0
fi

file1="${TEST_BINDIR}/test1_$$.nc"
file2="${TEST_BINDIR}/test2_$$.nc"

"${TEST_BINDIR}/fimex.sh" \
    -c felt2netcdf.cfg \
    --input.file "${TEST_DAT}" \
    --output.file "${file1}" \
    --output.config=../share/etc/cdmWriterConfigDeprecated.xml

if [ $? != 0 ]; then
  echo "failed converting felt to nc"
  rm -f "${file1}" "${file2}"
  exit 1
fi
if [ ! -f" ${file1}" ]; then
  echo "failed find nc-outputfile"
  exit 1
fi

"${TEST_BINDIR}/fimex.sh" \
    -c felt2netcdf.cfg \
    --input.file "${TEST_DAT}" \
    --output.file "${file2}" \
    --output.config=../share/etc/cdmWriterConfig.xml

if [ $? != 0 ]; then
  echo "failed converting felt to nc with ncml"
  rm -f "${file1}" "${file2}"
  exit 1
fi
if [ ! -f "${file2}" ]; then
  echo "failed find nc-outputfile"
  rm -f "${file1}" "${file2}"
  exit 1
fi

if "${TEST_BINDIR}/nccmp.sh" "${file1}" "${file2}" ; then
    echo "success"
    E=0
else
    echo "failed diff $file $file2";
    E=1
fi
rm -f "${file1}" "${file2}"
exit $E
