CC=gcc
CXX=gcc
CXXFLAGS=
CPPFLAGS=
CFLAGS=-std=c99 -Wall -pedantic -g
LIBS=-lproj -lm
LDFLAGS=
RANLIB=ranlib

.SUFFIXES:
.SUFFIXES: .c .o .a .d

sources = interpolation.c

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

libmiutplukk.a: libmiutplukk.a(interpolation.o)
	$(RANLIB) $@

.PHONY : clean
clean :
	-$(RM) *.o libmiutplukk.a