#! /bin/sh
echo "testing conversion NetCDF to NetCDF"
file=/tmp/testfile.nc
cd ../src/binSrc
./utplukk -c utplukk.cfg --output.file=${file}
if [ $? != 0 ]; then
  echo "failed converting felt to nc"
  rm -f ${file}
  exit 1
fi
if [ ! -f ${file} ]; then
  echo "failed find nc-outputfile"
  exit 1
fi
rm -f ${file}
echo "success"
exit 0
