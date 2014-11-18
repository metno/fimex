#! /usr/bin/perl

use strict;
use warnings;
use FimexFuncs qw(grib2netcdf);

use Time::Local qw();

my $country = "bangladesh";
my $datadir = "/disk1/myopdata/ec/data_$country/";
my $etcdir = "/disk1/myopdata/ec/etc/";
my $time = Time::Local::timegm(0, 0, 0, 31, 9, 2014);


 grib2netcdf({refTime => $time,
             debug => 1,
             gribConfTemplate => $etcdir."cdmGribReaderConfigEC_template.xml",
             ncmlTemplate => $etcdir."mbv_atmos_template.ncml",
             ncTemplate => $etcdir."${country}_template.nc",
             ncName => $datadir."${country}_atmos_%Y%m%d_%H.nc",
             cleanup => [$datadir."${country}_atmos_????????_??.nc", 0],
             gribFile => $ARGV[0]}) || die "Cannot convert grib via fimex";

