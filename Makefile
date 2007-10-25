SHELL = /bin/sh


all: src

src: src/libutplukk.a

src/libutplukk.a:
	cd src && $(MAKE) all


test: test/testInterpolation

test/testInterpolation:
	cd test && $(MAKE) all

.PHONY : clean doc
clean :
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
	cd doc/latex && $(MAKE) clean
	
doc :
	doxygen Doxyfile
	cd doc/latex && $(MAKE)
