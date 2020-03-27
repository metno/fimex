#! /bin/sh
echo "testing conversion NetCDF to NetCDF"

FILE1=test_feltNetcdfWrite.nc
if [ ! -f "${FILE1}" ]; then
   echo "SKIP because '${FILE1}' is absent (generated from optional test data)"
   exit 0
fi

./fimex.sh "${FILE1}" testNcRdWr.nc

if [ $? != 0 ]; then
  echo "failed converting nc to nc"
  rm -f testNcRdWr.nc "${FILE1}"
  exit 1
fi

if ./nccmp.sh "${FILE1}" testNcRdWr.nc ; then
  echo "success"
  E=0
else
  echo "failed diff nc to nc"
  E=1
fi
rm -f testNcRdWr.nc "${FILE1}"
exit $E
