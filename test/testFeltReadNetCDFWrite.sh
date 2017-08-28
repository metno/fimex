#! /bin/sh
echo "testing conversion Felt to NetCDF"
echo $0
TEST_SRCDIR=`dirname $0`
TEST_BINDIR=`pwd`
cd $TEST_SRCDIR
if [ ! -f flth00.dat ]; then
   echo "no input data: flth00.dat, skipping test..."
   exit 0;
fi
file=${TEST_BINDIR}/test.nc
file2=${TEST_BINDIR}/test2.nc
"${TEST_BINDIR}/fimex.sh" -c felt2netcdf.cfg --output.file=${file} --output.config=../share/etc/cdmWriterConfigDeprecated.xml
if [ $? != 0 ]; then
  echo "failed converting felt to nc"
  exit 1
fi
if [ ! -f ${file} ]; then
  echo "failed find nc-outputfile"
  exit 1
fi
"${TEST_BINDIR}/fimex.sh" -c felt2netcdf.cfg --output.file=${file2} --output.config=../share/etc/cdmWriterConfig.xml
if [ $? != 0 ]; then
  echo "failed converting felt to nc with ncml"
  exit 1
fi
if [ ! -f ${file2} ]; then
  echo "failed find nc-outputfile"
  exit 1
fi

if [ -f /usr/bin/diff ]; then
   if ! /usr/bin/diff ${file} ${file2}; then
      echo "failed diff $file $file2";
      exit 1;
   fi
fi

echo "success"
exit 0
