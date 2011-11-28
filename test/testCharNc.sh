#! /bin/sh
echo "test writing netcdf byte and char"
if [ ! -f char.nc ]; then
   echo "no input data: char.nc, skipping test..."
   exit 0;
fi
../src/binSrc/fimex --input.file=char.nc --output.file=out.nc
if [ $? != 0 ]; then
  echo "failed writing char/byte to nc-file"
  exit 1
fi
echo "success"
exit 0
