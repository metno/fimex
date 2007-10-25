F2CLIB=-lg2c
CC=gcc
CXX=g++
CXXFLAGS=-Wall -pedantic -g
CPPFLAGS=-I. -I/disk1/metno/local/include/milib
CFLAGS=-std=c99 -Wall -pedantic -g
LDLIBS=-lmiutplukk -L/disk1/metno/local/lib -lmic -lmi $(F2CLIB) -lproj -lboost_regex -lboost_filesystem -lboost_unit_test_framework -lm
LDFLAGS=
RANLIB=ranlib

.SUFFIXES:
.SUFFIXES: .c .cc .o .d

include felt_reader/Make.inc

sources = interpolation.c $(addprefix felt_reader/,$(FELT_READER_SRC))
c_sources = $(filter %.c,$(sources))
cc_sources = $(filter %.cc,$(sources))
objects = $(c_sources:.c=.o) $(cc_sources:.cc=.o)

all: libmiutplukk


# depend, needed ???
#%.d: %.c
#	@set -e; rm -f $@; \
#	$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
#	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
#	$(RM) -f $@.$$$$
#	
#include $(sources:.c=.d)

libmiutplukk: libmiutplukk.a

# prevent deletion of intermediate objects
.SECONDARY: $(objects)

libmiutplukk.a: libmiutplukk.a($(objects))
	$(RANLIB) $@

.PHONY : clean
clean :
	-$(RM) *.o libmiutplukk.a felt_reader/*.o
