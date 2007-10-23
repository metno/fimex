CC=gcc
CXX=g++
CXXFLAGS=-Wall -pedantic -g
CPPFLAGS=
CFLAGS=-Wall -pedantic
LDLIBS=-L../src -lmiutplukk -lproj -lboost_unit_test_framework -lm
LDFLAGS=
RANLIB=ranlib

.SUFFIXES:
.SUFFIXES: .cc .o

sources = testInterpolation.c

all: build

build: testInterpolation

test: testInterpolation
	./testInterpolation
	
.PHONY : clean
clean :
	-$(RM) *.o testInterpolation