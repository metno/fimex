#! /bin/sh

TEST_SRCDIR=$(dirname $0)
TOP_SRCDIR=${TEST_SRCDIR}/..

echo "test fiGrbmlCat"
if [ ! -f test.grb1 ]; then
   echo "no input data: test.grb1, skipping test..."
   exit 0
fi

rm -f test.grb1.grbml

./fiIndexGribs.sh -i test.grb1
if [ $? != 0 -o ! -f test.grb1.grbml ]; then
  echo "failed writing test.grb1.grbml"
  exit 1
fi

./fiGrbmlCat.sh -o cat.grbml test.grb1.grbml test.grb1.grbml
if [ $? != 0 -o ! -f cat.grbml ]; then
  echo "failed writing cat.grbml"
  rm -f test.grb1.grbml
  exit 1
fi

NC=`grep -c '</gribFileIndex>' cat.grbml`
rm -f test.grb1.grbml cat.grbml
if [ "$NC" != 1 ]; then
  echo "unexpected count $NC of closing gribFileIndex tags"
  exit 1
else
    echo "success"
    exit 0
fi
