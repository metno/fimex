F2CLIB=-lg2c
CC=gcc
CXX=g++
CXXFLAGS=-Wall -pedantic -g
CPPFLAGS=-I../src -I../src/felt_reader -I/disk1/metno/local/include/milib -I/usr/include/netcdf-3
CFLAGS=-std=c99 -Wall -pedantic -g
LDLIBS=-L../src -lmiutplukk -L/disk1/metno/local/lib -lmic -lmi $(F2CLIB) -L/usr/lib/netcdf-3 -lnetcdf_c++ -lnetcdf -ludunits -lproj -lboost_regex -lboost_filesystem -lboost_unit_test_framework -lm -lxml2 -lz
LDFLAGS=
RANLIB=ranlib

PRGS=testInterpolation testFeltReader testTime testTimeSliceData testNetcdfWriter

.SUFFIXES:
.SUFFIXES: .cc .o

all: build

build: $(PRGS)

../src/libmiutplukk.a:
	cd ../src && $(MAKE) libmiutplukk

test: build
	./testInterpolation
	./testFeltReader
	./testTime
	./testTimeSliceData
	./testNetcdfWriter
	
testInterpolation: ../src/libmiutplukk.a

testFeltReader: ../src/libmiutplukk.a

testTime: ../src/libmiutplukk.a

testTimeSliceData: ../src/libmiutplukk.a

testNetcdfWriter: ../src/libmiutplukk.a

.PHONY : clean
clean :
	-$(RM) *.o $(PRGS)