# Fimex

Fimex is a library and a program to convert gridded geospatial data
between different formats and projections.


## NcML Changes in version 2.0

In fimex 2.0, there are some major changesd in the behaviour of ncml
in fimex.
1. In fimex before 2.0, the order of the dimensions for the shape of
   variables in ncml was reversed with respect to the ncml
   description. In 2.0, the dimension order in fimex and the ncml
   description should be the same. As both the old and new ncml text
   are valid, but only one of them yields useful results, this change
   requires manual review. This is the main reason to increase the
   major version number of fimex.
2. There is now support for `joinNew`. As part of this implementation,
   the behaviour of `union` and `joinExisting` has also changed and
   should be closer to the ncml description. Again, for the same input
   ncml this might produce different output compared to fimex before
   2.0.
3. There is now support for writing to ncml, including values. This is
   probably only useful for small datasets, as output files might
   become huge for large datasets.

## Dependencies

Fimex requires at least the following libraries to be installed for
compilation

 * c99/c++11 compiler
 * libxml2 >= 2.6.0
 * proj >= 4.4.9 and < 5.0.0 or >= 6.0.0 -- version 5 does not seem to work
 * udunits2 (>= 2.1.9)
 * [github.com/metno/mi-cpptest](https://github.com/metno/mi-cpptest)
 * [github.com/metno/mi-programoptions](https://github.com/metno/mi-programoptions)
 * [github.com/HowardHinnant/date](https://github.com/HowardHinnant/date)
   tested with [release 2.4.1](https://github.com/HowardHinnant/date/releases/tag/v2.4.1)

To configure the different file formats it requires:

 * NetCDF (netcdf > 3.5)
 * Grib_API (grib_api > 1.4) or ecCodes

To build the python interface, it requires:

 * pybind11 (>= 2.2.4)
 * python numpy (for the unit tests)

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
    
    mkdir -p $BLD
    cd $BLD
    cmake -G"$GENERATOR" \
        -DBUILD_SHARED_LIBS=ON \
        -DENABLE_PYTHON=NO \
        -DCMAKE_INSTALL_PREFIX="$INS" \
        "$SRC"
    
    make all test install

Of course, `ENABLE_PYTHON` can be replaced with other
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

On x86-64 the following options are useful:

    CFLAGS='-O2 -mavx2 -ftree-vectorize -fno-math-errno' CXXFLAGS='-O2 -msse2 -ftree-vectorize -fno-math-errno' cmake ...

### Testing the Installation

Some tests require extra test data which can be downloaded from
https://wiki.met.no/fimex/download. The test data archive must be
unpacked, and the directory containing the file `VERSION` should be
specified to cmake


    cmake  ... -DTEST_EXTRADATA_DIR=/path/to/extra/test/data ...


## Usage Example

Converting a model output from felt to NetCDF:

 1. Get an overview of times, layers and grid used in the felt-file.
 2. Group the parameters so that all parameters within an group use the
    same grid, times and the same layers (its possible to have a file
    with both sigma and pressure layers, but then all sigma-variables
    should have the same number of sigma layers and all pressure-variables
    should have the same number of pressure layers. Surface only variables,
    usually sigma=1000 felt variables, are a layer-group of their own).
 3. Create a `felt2nc_variables.xml` file for each variable group.
    Copy parameters from existing examples, or lookup variable and parameter
    names in the CF standard name documentation.
    If you have to generate several groups, it might be useful to
    split the file into axes, global attributes and variables, see, e.g.,
    `damocles_felt2nc_variables.xml`.
    If you have two parameters which map to the same `standard_name`, make
    sure to give them different variable names if they are in the same group
    Adjust the datatype according to your data. float will always work fine,
    use short if you know that the variable even fits as 4-dimensional data
    into shorts. Use eventually a `cdmWriterConfig.xml` to process data as float
    and write the data as short.
 4. Validate the `felt2nc_variables.xml file` with, e.g.,

        xmllint --xinclude --postvalid --noout ../../share/etc/felt2nc_variables.xml

 5. Adjust the projection and area you want to extract in `fimex_example.cfg` (or
    use command line arguments to fimex)
 6. Run

        fimex -c fimex_example.cfg


### Library usage

To use the fimex >= 1.2 from CMake projects, please use

    -Dfimex_DIR=.../lib/cmake/fimex

and

    find_package(fimex "1.2" REQUIRED)

or, if you want a specific version

    -Dfimex-1.0_DIR=.../lib/cmake/fimex-1.0
    find_package(fimex-1.0 REQUIRED)

In both cases, include paths etc are set implicitly when using

    target_link_libraries(libfimex)

This functionality is not yet supported for static fimex libraries.
