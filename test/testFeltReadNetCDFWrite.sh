#! /bin/sh
echo "testing conversion NetCDF to NetCDF"
file=../../test/test.nc
cd ../src/binSrc
./utplukk -c utplukk.cfg --output.file=${file}
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
