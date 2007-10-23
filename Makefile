SHELL = /bin/sh

all: src

src:
	cd src && $(MAKE) all

test: 
	cd test && $(MAKE) test

.PHONY : clean doc
clean :
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
	cd doc/latex && $(MAKE) clean
	
doc :
	doxygen Doxyfile
	cd doc/latex && $(MAKE)
