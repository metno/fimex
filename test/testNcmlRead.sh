#! /bin/sh
if [ ! -f coordTest.nc ]; then
	exit 0;
fi
echo "test reading with ncml"
if [ ! -f test.ncml ]; then
   echo "no input data: test.ncml, skipping test..."
   exit 0;
fi
../src/binSrc/fimex --input.file=test.ncml --output.file=x --output.type=null
if [ $? != 0 ]; then
  echo "failed reading ncml"
  exit 1
fi
echo "success"
exit 0
