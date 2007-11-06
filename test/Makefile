F2CLIB=-lg2c
CC=gcc
CXX=g++
CXXFLAGS=-Wall -pedantic -g
CPPFLAGS=-I../src -I/disk1/metno/local/include/milib
CFLAGS=-std=c99 -Wall -pedantic -g
LDLIBS=-L../src -lmiutplukk -L/disk1/metno/local/lib -lmic -lmi $(F2CLIB) -lproj -lboost_regex -lboost_filesystem -lboost_unit_test_framework -lm
LDFLAGS=
RANLIB=ranlib

.SUFFIXES:
.SUFFIXES: .cc .o

all: build

build: testInterpolation testFeltReader

../src/libmiutplukk.a:
	cd ../src && $(MAKE) libmiutplukk

test: build
	./testInterpolation
	./testFeltReader
	
testInterpolation: ../src/libmiutplukk.a

testFeltReader: ../src/libmiutplukk.a


.PHONY : clean
clean :
	-$(RM) *.o testInterpolation testFeltReader