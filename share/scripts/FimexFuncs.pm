package FimexFuncs;

use 5.00400;
use strict;
use warnings;
use POSIX qw(strftime);
use File::Temp qw();
use File::Spec qw();
use File::Glob qw();

use constant SUCCESS => 1;
use constant FAIL => 0;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

%EXPORT_TAGS = ( 'all' => [ qw(
    grib2netcdf
    SUCCESS
    FAIL
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw();

sub grib2netcdf {
    my ($ref) = @_;
    my $tmpdir = $ref->{tmpdir} || $ENV{TMPDIR} || '/tmp';
    my $debug = $ref->{debug};
    my $refTime = $ref->{refTime};
    unless ($refTime) {
        debugPrint_("grib2netcdf: missing refTime");
        return FAIL;
    }
    my $gribFile = $ref->{gribFile};
    unless ($gribFile && -f $gribFile) {
        debugPrint_("grib2netcdf: missing gribFile: $gribFile");
        return FAIL;
    }
    my $gribConfTemplate = $ref->{gribConfTemplate};
    unless ($gribConfTemplate && -f $gribConfTemplate) {
        debugPrint_("grib2netcdf: missing gribConfTemplate: $gribConfTemplate") if $debug;
        return FAIL;
    }
    my $ncTemplate = $ref->{ncTemplate};
    unless ($ncTemplate && -f $ncTemplate) {
        debugPrint_("grib2netcdf: missing ncTemplate: $ncTemplate") if $debug;
        return FAIL;
    }
    my $ncmlTemplate = $ref->{ncmlTemplate};
    unless ($ncmlTemplate && -f $ncmlTemplate) {
        debugPrint_("grib2netcdf: missing ncmlTemplate: $ncmlTemplate") if $debug;
        return FAIL;
    }
    my $ncName = $ref->{ncName};
    unless ($ncName) {
        debugPrint_("grib2netcdf: missing ncName") if $debug;
        return FAIL;
    }
    my $fmx = 'fimex';
    debugPrint_('grib2netcdf: '.`$fmx --version`) if $debug;
    my ($sec,$min,$hour,$mday,$mon,$year) = gmtime($refTime);
    my $outnc = strftime($ncName, ($sec,$min,$hour,$mday,$mon,$year));
    my (undef, undef, $outfilename) =  File::Spec->splitpath( $outnc );
    my $fileTimeStamp = strftime("%Y-%m-%dT%H%M%S", ($sec,$min,$hour,$mday,$mon,$year));
    my $timeStamp = strftime("%Y-%m-%d %H:%M:%S", ($sec,$min,$hour,$mday,$mon,$year));
    my $ncml = File::Spec->catfile($tmpdir, $outfilename . ".ncml");
    my $cdmConfig = File::Spec->catfile($tmpdir, $outfilename . ".xml");

    if (! -f $outnc) {
        # cleanup eventually existing configuration files (in case of restart)
        unlink $ncml;
        unlink $cdmConfig;
    }

    if (! -f $ncml) {
        debugPrint_("grib2netcdf: generating $ncml") if $debug;
        open my $fhi, $ncmlTemplate or die "cannot read $ncmlTemplate: $!";
        open my $fho, "> $ncml" or die "cannot write $ncml: $!";
        while (defined (my $line = <$fhi>)) {
            $line =~ s/YYYY-MM-DD HH:MM:SS/$timeStamp/g;
            print $fho $line;
        }
        close $fho;
        close $fhi;
    }
    if (! -f $cdmConfig ) {
        debugPrint_("grib2netcdf: generating $cdmConfig") if $debug;
        open my $fhi, $gribConfTemplate or die "cannot read $gribConfTemplate: $!";
        open my $fho, "> $cdmConfig" or die "cannot write $cdmConfig: $!";
        while (defined (my $line = <$fhi>)) {
            $line =~ s/YYYY-MM-DD HH:MM:SS/$timeStamp/g;
            print $fho $line;
        }
        close $fho;
        close $fhi;
    }
    if (! -f $outnc) {
        my @command = ($fmx, "--input.file=$ncTemplate", "--input.config=$ncml",
                       "--output.file=$outnc", "--output.type=nc4");
        debugPrint_("grib2netcdf: generating $outnc: @command") if $debug;
        if (systemCall_(@command)) {
            errorPrint_("grib2netcdf: generating $outnc: @command");
            return FAIL;
        }
    }

    # now filling the data
    my @command = ($fmx, "--input.file=$gribFile", "--input.type=grib", "--input.config=$cdmConfig",
                    "--output.fillFile=$outnc");
    debugPrint_("grib2netcdf: filling $outnc: @command\n") if $debug;
    if (systemCall_(@command)) {
        errorPrint_("grib2netcdf: filling $outnc failed: @command");
        return FAIL;
    }

    if (defined($ref->{cleanup}) && (ref($ref->{cleanup}) eq 'ARRAY')) {
        my $glob = $ref->{cleanup}[0];
        my $days = $ref->{cleanup}[1];
        unless (defined $days) {
            errorPrint_("grib2netcdf: cleaning $glob: no. of days not given");
            return FAIL;

        }
        my @files = File::Glob::bsd_glob($glob);
        my $minage = time - 24*60*60*($days+1);
        foreach my $file (@files) {
            my $mtime = (stat($file))[9];
            if ($mtime < $minage) {
                debugPrint_("grib2netcdf: cleanup file: $file") if $debug;
                unlink $file;
            }
        }

    }


    return SUCCESS;
}

sub systemCall_ {
    my (@cmd) = @_;
    system(@cmd);
    if ($? == 0) { return 0;}
    if ($? == -1) {
        debugPrint_("failed to execute '@cmd': $!\n");
    } elsif ($? & 127) {
        debugPrint_(sprintf("child died with signal %d, %s coredump on @cmd\n",
                 ($? & 127),  ($? & 128) ? 'with' : 'without'));
    } else {
        debugPrint_(sprintf("child exited with value %d\n", $? >> 8));
    }
    return 1;
}

sub debugPrint_ {
    if (defined &main::TIME_PRINT) {
        main::TIME_PRINT(@_);
    } else {
        print STDERR "@_\n";
    }
}

sub errorPrint_ {
    if (defined &main::ERROR) {
        main::ERROR(@_);
    } else {
        print STDERR "@_\n";
        die "@_\n";
    }
}

1;
__END__
=head1 NAME

FimexFuncs - Helper Functions for use with fimex

=head1 SYNOPSIS

 use FimexFuncs qw(grib2netcdf);

 my $time = Time::Local::timegm(0,0,12, 30, 9, 2014); # 30.october 2014 12UTC
 grib2netcdf({refTime => $time,
             debug => 1,
             tmpdir => '/tmp',
             gribConfTemplate => "etc/cdmGribReaderConfig_template.xml",
             ncmlTemplate => "etc/ncmlTemplate.ncml",
             ncTemplate => "etc/ncTemplate.nc",
             ncName => "output_nc%Y%m%d%h_%H.nc",
             cleanup => ["output_nc??????????_??.nc", 3],
             gribFile => "in.grib"}) || die "Cannot convert grib via fimex";


=head1 DESCRIPTION


=head1 PUBLIC METHODS


=over 4

=item grib2netcdf fills a netcdf file with a grib-file. Creates the netcdf-file if it doesn't exist.

=over 8

=item refTime file-reference time given as epoch-seconds

=item debug enable/disable debug-messages

=item tmpdir directory where to write temporary file, e.g. convertet templates

=item gribConfTemplate  cdmGribReaderConfig, with variable ref-dates written as 'YYYY-MM-DD HH:MM:SS'.
This updated file will be used on every incoming grib-file.

=item ncmlTemplate ncml-file with variable ref-dates written as 'YYYY-MM-DD HH:MM:SS'.
This file will be used on creation of a new nc-file.

=item ncTemplate netcdf-file with no (0) time-steps.
Will be used for creation of new nc-file together with ncml-file.

=item ncName output-file, which will be expanded using strftime (%m -> month). Will also be used as
ID. All grib-files outputing into the same ID will be put into it.

=item cleanup glob of files which will be cleaned up after number of days

=item gribFile input grib-file

=back

=back

=head1 AUTHOR

Heiko Klein, E<lt>H.Klein@met.no<gt>

=cut
