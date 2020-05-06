# Transition from 0.x to 1.x

As you can see from the NEWS file, there have been significant changes
in fimex 1.0 compared to previous versions. Most of the changes affect
the C++ code, but there are also some changes affecting the
commandline programs.

## General changes

The conversion from text to numbers has become more strict. While text
after numbers had previously been ignored sometimes (sometimes not),
now the text may not contain anything else but the number. In
particular, `f` or `L` suffixes that had previously been ignored in
GRIB reader configuration files will cause errors in fimex 1.0.
Using a command like

    find . -name "*ml" | xargs -r sed -i -e 's,\([0-9]\+\)[fL]",\1",g'

on the configuration files should do most of the work, but the
original files should be backed up before starting these automatic
changes, and the result should be checked before use.

As part of the transition to `std::regex`, the regex format has
changed slightly. They are no longer 'perl' format but 'modified
ECMAscript', the default in C++11. This may affect a few configuration
files when using rather advanced regular expressions.

The data types 'string' and 'byte' are now treated separately.

Compression in `fiIndexGribs` and `fiGribCut` is no longer
supported. The `fiIndexGribs` command line has changed in version 1.0
to make it more clear which file is written or appended to.

Previously, options specified in a file could be overwritten on the
command line. This is now rejected (by the mi-programoptions
library). Adding back this feature requires major changes in
mi-programoptions, therefore it might not come back. There are some
other subtle changes in reading options from files.

## C++ library changes

Boost was removed from fimex 1.0 C++ code in favour of C++11 and some
small helper libraries. This requires changes in C++ code using fimex:

 - `boost::shared_ptr` is replaced by `std::shared_ptr`
 - `boost::shared_array` is replaced by `MetNoFimex::shared_array`
   (this is a wrapper around `std::shared_ptr` with an array deleter
   and index access, as `std::shared_ptr` does not support arrays in
   C++11)
 - many typedefs have been changed. Except for `DataPtr`, all of them
   should now be named `<Class>_p` for `std::shared_ptr<Class>` and
   `<Class>_cp` for `std::shared_ptr<const Class>`. Nested typedefs
   have been removed.
 - `fimex/Utils.h` has been split up into several files
   `fimex/...Utils.h` by topic
 - all times returned by the fimex library are `FimexTime`
   objects. There are conversion functions for `std::chrono` (in
   `fimex/TimeUtils.h`) and for `boost::posix_time` in
   `fimex/boost-posix-time-compat.h` (untested).
 - some deprecated functions have been removed
 - `CDMFileReaderFactory` only supports strings to identify file types
   ('netcdf'). The filetype constants in `fimex/CDMconstants.h` have
   been removed.

## Python binding changes

The python binding has been improved and extended very much in fimex
1.0. In particular, data are usually not copied any more between
python and C++, instead pointers are used. It is now also possible to
implement a `CDMReader` subclass in python and to pass that to C++
code for writing, interpolation, etc.

