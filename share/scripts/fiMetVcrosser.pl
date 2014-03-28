#! /usr/bin/perl -w

use strict;
use warnings;

=begin LICENSE

Copyright (C) 2013 met.no

This file is part of METAMOD

METAMOD is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

METAMOD is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with METAMOD; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

=end LICENSE

=head1 NAME

fiMetVcrosser - read a metno-vcross definition table and use it to run fimex

=head1 SYNOPSIS

fiMetVcrosser VCROSS_DEF.txt FIMEX_OPTIONS

=head1 DESCRIPTION

Read met.no vertical-cross-section definition files and use fimex to create
the vertical cross-sections from a model-input files.

=head1 OPTIONS

=head2 VCROSS_DEF.txt

The met.no vertical-cross section definition file in the form

NT, NLL, LAT1, LON1, LAT2, LON2, ..., 'NAME'

with

=over 8

=item NT number type, with 1 = i,j coordinates (not supported); 2 = x,y coordinates (not supported);
         3 = lat,lon in degree with decimals; 4 = lat, lon in degree *100 + minutes; 0 means end of input

=item NLL number of coordinates, should be > 1

=item LAT1, LON1, ... coordinates

=item NAME a name in ' ' to give the cross-section

=back

=head2 FIMEX_OPTIONS

All further options will be send to fimex without any modifications.

=cut

our $DEBUG = 0;
use constant LATIN1 => 1;

use strict;
use warnings;
use Pod::Usage qw(pod2usage);

if (@ARGV < 2) {
    pod2usage(-exitval => 2,-verbose => 2);
}

my $fimex = $ENV{FIMEX} || "fimex";

my (@points, @lats, @lons, @names);

my $vcrossFile = shift @ARGV;
open (my $fh, $vcrossFile)
    or die "cannot read input $vcrossFile: $!\n";
LINES: while (defined (my $line = <$fh>)) {
    chomp $line;
    my @vals = split ',', $line;
    my $type = int(shift @vals);
    if ($type == 0) {
        last LINES;
    }
    if (($type != 3) and ($type != 4)) {
        print STDERR "undefined type in: $line\n";
        print STDERR "skipping ...";
        next LINES;
    }
    my $points = int(shift @vals);
    if ((2*$points) > (@vals + 1)) {
        print STDERR "not enough coordinates in line: $line\n";
        print STDERR "skipping ...\n";
        next LINES;
    }

    push @points, $points;
    for (my $i = 0; $i < $points; ++$i) {
        my $lat = type2val($type, shift @vals);
        my $lon = type2val($type, shift @vals);
        push @lats, $lat;
        push @lons, $lon;
    }
    my $name = join ',', @vals;
    $name =~ s/.*'(.*)'.*/$1/; # something between ticks
    $name =~ s/,/;/g; # forbidd , in names (split in fimex)
    # special met.no decoding scheme, pre latin1/utf8
    $name =~ s/#/Æ/g;
    $name =~ s/\@/Ø/g;
    $name =~ s/\$/Å/g;
    if (LATIN1) {
        require Encode;
        # make sure characters are recognized as utf8
        $name = Encode::decode_utf8($name);
        $name = Encode::encode("ISO-8859-1", $name, Encode::FB_CROAK);
    }
    push @names, $name;
}
close $fh;

# create fimex commandLine
my $names = join ",", @names ;
my $lonVals = join ",", @lons;
my $latVals = join ",", @lats;
my $points = join ",", @points;

my $method = "--interpolate.method=nearestneighbor";
foreach my $arg (@ARGV) {
    if ($arg =~ /-interpolate.method/) {
        $method = "";
    }
}

my @command = ($fimex);
push @command, $method if $method;
push @command, ("--interpolate.vcrossNames=$names",
                "--interpolate.vcrossNoPoints=$points",
                "--interpolate.longitudeValues=$lonVals",
                "--interpolate.latitudeValues=$latVals",
                @ARGV);
print STDERR "@command\n";
system(@command) == 0
    or die "system '@command' failed: $?";



sub type2val {
    my ($type, $val) = @_;
    if ($type == 3) {
        return 0+$val;
    } elsif ($type == 4) {
        my $v = int ($val / 100);
        my $min = $val % 100;
        return $v + $min/60;
    }
}


1;
__END__


=head1 AUTHOR

Heiko Klein, E<lt>H.Klein@met.noE<gt>

=head1 SEE ALSO

=cut