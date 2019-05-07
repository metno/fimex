#! /bin/sh

TEST_SRCDIR=$(dirname $0)
TOP_SRCDIR=${TEST_SRCDIR}/..

echo "test fiIndexGribs"
if [ ! -f test.grb1 ]; then
   echo "no input data: test.grb1, skipping test..."
   exit 0;
fi
rm -f test.grb1.grbml
./fiIndexGribs.sh -i test.grb1
if [ $? != 0 ]; then
  echo "failed writing test.grb1.grbml"
  exit 1
fi
if [ ! -f test.grb1.grbml ]; then
  echo "failed writing test.grb1.grbml"
  exit 1
fi

rm -f test.grb1.grbml
./fiIndexGribs.sh -i test.grb1 --extraKey=localDefinitionNumber
if [ $? != 0 ]; then
  echo "failed writing test.grb1.grbml with extraKey"
  exit 1
fi
if [ ! -f test.grb1.grbml ]; then
  echo "failed writing test.grb1.grbml with extraKey"
  exit 1
fi
if grep -q localDefinitionNumber test.grb1.grbml; then
  echo "success"
else
  echo "no localDefinitionNumber in test.grb1.grbml"
  exit 1;
fi

rm -f test.grb1.grbml2
./fiIndexGribs.sh -i test.grb1 --readerConfig="${TEST_SRCDIR}/cdmGribReaderConfig_newEarth.xml" -a test.grb1.grbml2
if [ $? != 0 ]; then
  echo "failed writing test.grb1.grbml2 with readerConfig -- failure"
  exit 1
fi
if [ ! -f test.grb1.grbml2 ]; then
  echo "failed writing test.grb1.grbml2 with readerConfig -- no output"
  exit 1
fi
if grep -q localDefinitionNumber test.grb1.grbml2; then
  echo "success"
else
  echo "no localDefinitionNumber in test.grb1.grbml2"
  exit 1;
fi
if cmp -s test.grb1.grbml test.grb1.grbml2 ; then
  echo "earthFigure not different between test.grb1.grbml and test.grb1.grbml2"
  exit 1;
else
  echo "success"
fi
rm -f test.grb1.grbml2

# read grbml with fimex
./fimex.sh --input.file=test.grb1.grbml --input.config "${TOP_SRCDIR}/share/etc/cdmGribReaderConfig.xml" --input.printNcML | grep x_wind_10m > /dev/null
if [ $? != 0 ]; then
  echo "failed reading test.grb1.grbml with fimex"
  exit 1
else
  echo "success"
fi

exit 0

