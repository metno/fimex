#! /usr/bin/perl -w
use strict;
use warnings;

use FindBin;

unless (@ARGV == 2) {
    print STDERR "usage: newProjection.pl FGDCNAME PROJ4_NAME\n";
    print STDERR "example: newProjection.pl albers_conical_equal_area aea\n";
    print STDERR "   this will create the skeletons AlbersConicalEqualAreaProjection.cc and AlbersConicalEqualAreaProjection.h\n";
    exit(0);
}

my @fileParts = split "_", $ARGV[0];
my $baseFileName = join "", map {ucfirst($_)} (@fileParts, "projection");

my $rootDir = "$FindBin::Bin/../..";

{
    # includeFile
    my $incDir = "$rootDir/include/fimex/coordSys/";
    my $incFile = $incDir . $baseFileName . '.h';
    if (-f $incFile) {
        print STDERR "$incFile exists, skipping\n";
    } else {
        my $infile = "$incDir/LambertAzimuthalEqualAreaProjection.h";
        open my $ifh, $infile or die "cannot read $infile\n";
        open my $ofh, ">$incFile" or die "cannot write $incFile\n";
        local $/ = undef;
        my $data = <$ifh>;
        print $ofh replace($data);
    }
}
{
    # cc File
    my $srcDir = "$rootDir/src/coordSys/";
    my $srcFile = $srcDir . $baseFileName . '.cc';
    if (-f $srcFile) {
        print STDERR "$srcFile exists, skipping\n";
    } else {
        my $infile = "$srcDir/LambertAzimuthalEqualAreaProjection.cc";
        open my $ifh, $infile or die "cannot read $infile\n";
        open my $ofh, ">$srcFile" or die "cannot write $srcFile\n";
        local $/ = undef;
        my $data = <$ifh>;
        print $ofh replace($data);
    }
}

sub replace {
    my ($data) = @_;
    $data =~ s/LambertAzimuthalEqualAreaProjection/$baseFileName/g;
    $data =~ s/LAMBERTAZIMUTHALEQUALAREAPROJECTION/uc($baseFileName)/ge;
    $data =~ s/lambert_azimuthal_equal_area/$ARGV[0]/g;
    $data =~ s/laea/$ARGV[1]/g;
    return $data;
}

