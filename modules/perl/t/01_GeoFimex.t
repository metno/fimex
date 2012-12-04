#! /usr/bin/perl -w
use strict;
use warnings;
use Test::More tests => 5;

BEGIN { use_ok('Geo::Fimex'); };

my $reader = Geo::Fimex::CDMFileReaderFactory::create("felt","../../test/flth00.dat", "../../share/etc/felt2nc_variables.xml");
isa_ok($reader, 'Geo::Fimex::boost__shared_ptrCDMReader');

my $sliceBuilder = new Geo::Fimex::SliceBuilder($reader->getCDM(), "time");
$sliceBuilder->setStartAndSize("time", 0, 3);
my $time = $reader->getSliceVecInUnit("time", $sliceBuilder, "seconds since 1970-01-01 00:00:00");
print "hash time: ", $time->size, ", ", $time->get(2), "\n";
isa_ok($time, "Geo::Fimex::DoubleVector");
ok($time->size == 3, "fetched 3 times");

#new Geo::Fimex::NetCDF_CDMWriter($reader, "out.nc", "", 3);
new Geo::Fimex::NetCDF_CDMWriter($reader, "out.nc", "", 3);
ok(-f "out.nc", "out.nc generated");
unlink "out.nc";
