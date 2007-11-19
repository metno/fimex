F2CLIB=-lg2c
CC=gcc
CXX=g++
CXXFLAGS=-Wall -pedantic -g
CPPFLAGS=-I../src -I/disk1/metno/local/include/milib
CFLAGS=-std=c99 -Wall -pedantic -g
LDLIBS=-L../src -lmiutplukk -L/disk1/metno/local/lib -lmic -lmi $(F2CLIB) -ludunits -lproj -lboost_regex -lboost_filesystem -lboost_unit_test_framework -lm
LDFLAGS=
RANLIB=ranlib

.SUFFIXES:
.SUFFIXES: .cc .o

all: build

build: testInterpolation testFeltReader testTime testTimeSliceData

../src/libmiutplukk.a:
	cd ../src && $(MAKE) libmiutplukk

test: build
	./testInterpolation
	./testFeltReader
	./testTime
	./testTimeSliceData
	
testInterpolation: ../src/libmiutplukk.a

testFeltReader: ../src/libmiutplukk.a

testTime: ../src/libmiutplukk.a

testTimeSliceData: ../src/libmiutplukk.a


.PHONY : clean
clean :
	-$(RM) *.o testInterpolation testFeltReader testTime testTimeSliceData