#! /bin/sh
if [ ! -f coordTest.nc ]; then
	exit 0;
fi
echo "testing interpolator with fill"
../src/binSrc/fimex -c fillInterpolator.cfg
if [ $? != 0 ]; then
  echo "failed fillInterpolate"
  exit 1
fi
echo "success"
exit 0
