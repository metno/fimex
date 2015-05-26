#! /usr/bin/perl

use strict;
use warnings;

unless (@ARGV) {
    print STDERR "usage: $0 file.nc\n";
    exit(1);
}

my $cmd = "fiXYcontents --input.file=$ARGV[0] --stats=mean,min,max,stddev";
open my $ph, "$cmd |"
    or die "cannot call '$cmd': $!\n";
my %stats;
my $var = "";
while (defined (my $line = <$ph>)) {
    if ($line =~ /^Var\:/) {
        printStats($var, \%stats) if $var;
        my @newVar = split ' ', $line;
        $var = "$newVar[1]($newVar[3])";
        $stats{min} = [];
        $stats{max} = [];
        $stats{mean} = [];
        $stats{stddev} = [];
    } elsif ($line =~ /mean=(\S+)\s+min=(\S+)\s+max=(\S+)\s+stddev=(\S+)/) {
        push @{ $stats{mean} }, $1;
        push @{ $stats{min} }, $2;
        push @{ $stats{max} }, $3;
        push @{ $stats{stddev} }, $4;
    } 
}

sub printStats {
    my ($var, $stats) = @_;
    print $var, " ";
    foreach my $stat (qw(min max mean)) {
        my ($min, $max) = minMax(@{ $stats->{$stat} });
        print "$stat=$min-$max ";
    }
    print "\n";
}

sub minMax {
    my $min = $_[0];
    my $max = $_[0];
    foreach my $el (@_) {
        if ($min > $el) {
            $min = $el;
        }
        if ($max < $el) {
            $max = $el;
        }
    }
    return ($min, $max);
}

sub max {
}
