#! /bin/sh
echo "testing conversion Felt to NetCDF"
echo $0
srcdir=`dirname $0`
curdir=`pwd`
cd $srcdir
file=${curdir}/test.nc
${curdir}/../src/binSrc/fimex -c felt2netcdf.cfg --output.file=${file} --output.config=../share/etc/cdmWriterConfig.xml
if [ $? != 0 ]; then
  echo "failed converting felt to nc"
  exit 1
fi
if [ ! -f ${file} ]; then
  echo "failed find nc-outputfile"
  exit 1
fi
echo "success"
exit 0
