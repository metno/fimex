F2CLIB=-lg2c
CC=gcc
CXX=g++
CXXFLAGS=-Wall -pedantic -g
CPPFLAGS=-I. -I/disk1/metno/local/include/milib
CFLAGS=-std=c99 -Wall -pedantic -g
LDLIBS=-lmiutplukk -L/disk1/metno/local/lib -lmic -lmi $(F2CLIB) -lproj -lboost_regex -lboost_filesystem -lboost_unit_test_framework -lm
LDFLAGS=
RANLIB=ranlib
DEPDIR = .deps
df = $(DEPDIR)/$(*)
MAKEDEPEND=gcc -M $(CPPFLAGS) -o $(df).d $<
SED=/bin/sed
CP=/bin/cp
MKDIR=mkdir -p

.SUFFIXES:
.SUFFIXES: .c .cc .o .h

%.o : %.c
	$(MAKEDEPEND); \
    $(CP) $(df).d $(df).P; \
	$(SED) -e 's,#.*,,' -e 's,^[^:]*: *,,' -e 's, *\\$$,,' \
	    -e '/^$$/ d' -e 's,$$, :,' < $(df).d >> $(df).P; \
	$(RM) $(df).d
	$(COMPILE.c) -o $@ $<

%.o : %.cc
	$(MAKEDEPEND); \
    $(SED) -e 's,^[^:]*:,$@:,' < $(df).d > $(df).P; \
	$(SED) -e 's,#.*,,' -e 's,^[^:]*: *,,' -e 's, *\\$$,,' \
	    -e '/^$$/ d' -e 's,$$, :,' < $(df).d >> $(df).P; \
	$(RM) $(df).d
	$(COMPILE.cc) -o $@ $<


#	$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
#	$(SED) 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \


include felt_reader/Make.inc

depdirs = $(DEPDIR) $(DEPDIR)/felt_reader
sources = interpolation.c TimeException.cc Time.cc TimeSliceData.cc $(addprefix felt_reader/,$(FELT_READER_SRC))
c_sources = $(filter %.c,$(sources))
cc_sources = $(filter %.cc,$(sources))
objects = $(c_sources:.c=.o) $(cc_sources:.cc=.o)

all: $(depdirs) libmiutplukk

$(DEPDIR):
	$(MKDIR) $@
	
$(DEPDIR)/felt_reader:
	$(MKDIR) $@

-include $(c_sources:%.c=$(DEPDIR)/%.P)
-include $(cc_sources:%.cc=$(DEPDIR)/%.P)

libmiutplukk: libmiutplukk.a

# prevent deletion of intermediate objects
.SECONDARY: $(objects)

libmiutplukk.a: libmiutplukk.a($(objects))
	$(RANLIB) $@

.PHONY : clean
clean :
	-$(RM) *.o libmiutplukk.a felt_reader/*.o
	-$(RM) -r $(depdirs)
