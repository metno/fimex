#! /bin/sh

TEST_SRCDIR=$(dirname $0)
TOP_SRCDIR="${TEST_SRCDIR}/.."

echo "test fiGrbmlCat"
if [ ! -f test.grb1 ]; then
   echo "SKIP missing 'test.grb1' (generated from optional test data)"
   exit 0
fi

cp test.grb1 cat.grb1
rm -f cat.grb1.grbml

./fiIndexGribs.sh -i cat.grb1
if [ $? != 0 -o ! -f cat.grb1.grbml ]; then
  echo "failed writing cat.grb1.grbml"
  exit 1
fi

./fiGrbmlCat.sh -o cat.grbml cat.grb1.grbml cat.grb1.grbml
if [ $? != 0 -o ! -f cat.grbml ]; then
  echo "failed writing cat.grbml"
  rm -f cat.grb1.grbml
  exit 1
fi

NC=`grep -c '</gribFileIndex>' cat.grbml`
rm -f cat.grb1 cat.grb1.grbml cat.grbml
if [ "$NC" != 1 ]; then
  echo "unexpected count $NC of closing gribFileIndex tags"
  exit 1
else
    echo "success"
    exit 0
fi
