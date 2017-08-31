# Fimex

Fimex is a library and a program to convert gridded geospatial data
between different formats and projections.


## Dependencies

Fimex requires at least the following libraries to be installed for
compilation

 * c99/c++ compiler
 * libxml2 >= 2.5.0
 * boost library >= 1.33
 * proj-4 >= 4.4.9
 * udunits2 (>= 2.1.9)

To configure the different file formats it requires:

 * NetCDF (netcdf > 3.5)
 * Grib_API (grib_api > 1.4)


## Installation

Since version 0.64.0~rc1, fimex is built with
[cmake](https://cmake.org/). The minimum required cmake version is
2.8.12.

These commands illustrate how fimex might be compiled:

    SRC=/fimex/source
    BLD=/fimex/build
    INS=/fimex/install
    
    GENERATOR="Unix Makefiles" # unix
    GENERATOR="CodeBlocks - $GENERATOR" # for qtcreator and others
    #GENERATOR="Eclipse CDT4 - $GENERATOR" # for eclipse
    
    mkdir $BLD
    cd $BLD
    cmake -G"$GENERATOR" \
        -DBUILD_SHARED_LIBS=ON \
        -DENABLE_GRIBAPI=YES -DGRIBAPI_INSTALL_DIR=/path/to/grib_api \
        -DENABLE_THIS=YES \
        -DENABLE_THAT=NO \
        -DCMAKE_INSTALL_PREFIX="$INS" \
        "$SRC"
    
    make all test install

Of course, `ENABLE_THIS` and `ENABLE_THAT` should be replaced with
actual config options.

There are several ways to edit available config options

  1. read the top-level `CMakeLists.txt` and pass values to `cmake`
     via `-D...`
  2. run `cmake` without options, then use either
     * `ccmake`, a text-based configuration editor, or
     * `cmake-gui` a Qt-based configuration editor, or
     * some IDE editor (e.g. qtcreator)

Please note that some packages are searched via `pkg-config` and that
it might be necessary to have `PKG_CONFIG_PATH` set properly if
packages are installed in non-standard locations or if you wrote your
own `.pc` files.

It is recommended to build fimex outside the source directory, ie BLD
!= SRC in the above example.

Since Fimex makes some floating-point calculations in large loops, it
is advisable to switch on SIMD/SSE operations in your compiler. On a
Xeon machine with a x386 CPU and gcc, the following flags might help
(those are default for x86-64)

    CFLAGS='-O2 -mfpmath=sse -msse2' CXXFLAGS='-O2 -mfpmath=sse -msse2' cmake ...


### Testing the Installation

Some tests require download of
[test-data](https://wiki.met.no/_media/fimex/flth00.dat.gz) which must
be uncompressed and added to the `fimex-source/test` directory. Then
run `make -C /fimex/build test`. Following the above example, command
like these might be used

    curl https://wiki.met.no/_media/fimex/flth00.dat.gz | gunzip > $SRC/test/flth00.dat
    make -C $BLD  test

USAGE EXAMPLE
-------------
Converting a model output from felt to NetCDF

a) get an overview of times, layers and grid used in the felt-file
b) group the parameters so that all parameters within an group use the
   same grid, times and the same layers (its possible to have a file
   with both sigma and pressure layers, but then all sigma-variables
   should have the same number of sigma layers and all pressure-variables
   should have the same number of pressure layers. Surface only variable,
   usually sigma=1000 felt variables, are layer-group of their own)
c) create a felt2nc_variables.xml file for each variable group.
   Copy parameters from existing examples, or lookup variable and parameter
   names in the CF standard name documentation.
   If you have to generate several groups, it might be useful to
   split the file into axes, global attributes and variables, see i.e.
   damocles_felt2nc_variables.xml
   If you have two paramters which map to the same standard_name, i.e. make
   sure to give them different variable names if they are in the same group
   Adjust the datatype according to your data. float will always work fine,
   use short if you know, that the variable even fits as 4-dimensional data
   into shorts. Use eventually a cdmWriterConfig.xml to process data as float
   and write the data as short.
d) validate the felt2nc_variables.xml file, i.e. with
	xmllint --xinclude --postvalid --noout ../../share/etc/felt2nc_variables.xml
e) adjust the projection and area you want to extract in fimex_example.cfg (or
   use command line arguments to fimex)
d) run 'fimex -c fimex_example.cfg'
