#! /usr/bin/perl -w

use 5.8.1;
use strict;
use warnings;

use CGI qw();
use CGI::Carp 'fatalsToBrowser';
$CGI::DISABLE_UPLOADS = 1;  # no uploads
use constant DEBUG => 0;

# SETUP, please modify
our $DataDir = '/opdata';
our $FiGribCut = '/home/heikok/Programme/MetSis/Fimex/src/binSrc/fiGribCut';
# use, i.e.: grib_ls -p paramId,parameterName /opdata/wave/WAM10geo.grb | sort | uniq
# to detect parameterIds and names
our @Files = (
    {name => 'WAM10',
     file => "$DataDir/wave/WAM10geo.grb",
     parameters => {
       3100 => 'SWH Signific.height,combined wind waves+swell m',
       3102 => 'SHWW Significant height of wind waves m',
       3105 => 'Significant height of swell waves m',
     }
    },
    {name => 'EC',
     file => "$DataDir/maritim/DNMI-ec.grb",
     parameters => {
        151 => 'MSL Mean sea level pressure Pa',
        165 => 'U U-component of wind m s**-1',
        166 => 'V V-component of wind m s**-1', 
        167 => 'T Temperature K',
     228164 => 'TCC Total cloud cover %', 
     228228 => 'TP Total precipitation kg m**-2',
       3017 => 'None Dew-point temperature K',
       3100 => 'SWH Signific.height,combined wind waves+swell m',
       3104 => 'None Direction of swell waves Degree true',
       3105 => 'None Significant height of swell waves m',
       3106 => 'None Mean period of swell waves s',
       3107 => 'MDPS Mean direction of primary swell Degree true',
       3108 => 'MPPS Mean period of primary swell s',
     }
    } 
);


# The program allows the states:
my %states = (
	'gribCut' => \&gribCut,
	'Fetch Data' => \&gribCut,
	'display' => \&display
);
my $cgi = new CGI();
my $action = $cgi->param('action') || '';
my $runState = exists $states{$action} ? $states{$action} : \&display;
$runState->();

sub display {
    print STDERR "running action: ", (caller(0))[3], "\n" if DEBUG;
    print $cgi->header,
          $cgi->start_html("$FiGribCut selection"),
          $cgi->h1("$FiGribCut selection"),
          $cgi->start_form(-method => 'GET'),
          $cgi->h2("Bounding Box"),
          'leave &quot;east&quot; emtpy for original data-region', '<br>',
          'north', $cgi->textfield(-name => 'northernmost_latitude', size=> 5),
          'west', $cgi->textfield(-name => 'easternmost_longitude', size=> 5),
          'south', $cgi->textfield(-name => 'southernmost_latitude', size=> 5),
          'east', $cgi->textfield(-name => 'easternmost_longitude', size=> 5);
    foreach my $file (@Files) {
    	print $cgi->h2($file->{name}, $cgi->checkbox(-name => 'fileName', -value => $file->{name}, -label => '')),
    		  $cgi->checkbox_group(-name => 'parameter',
    		                       -values => [sort keys %{$file->{parameters}}],
    		                       -labels => $file->{parameters},
    		                       -linebreak => 1);
    }

          
    print $cgi->hr(),
          $cgi->checkbox(-name => 'debug'),$cgi->br,
          $cgi->submit(-name => 'action', -value => 'Fetch Data'),
          $cgi->end_form(),
          $cgi->end_html();
}

sub gribCut {
    print STDERR "running action: ", (caller(0))[3], "\n" if DEBUG;
    my @gribCutProg;
    my $error = params2gribCut(\@gribCutProg);
    if ($error) {
    	print $cgi->header(-status => 400),
              $cgi->start_html('problems with $FiGribCut parameters'),
              $cgi->h1('problems with $FiGribCut parameters:'),
              $cgi->pre($cgi->escapeHTML($error)),
              $cgi->end_html();
    } else {
    	if ($cgi->param('debug')) {
    		my $prg = join ' ', @gribCutProg, '2>&1';
    		my $errMsg = `$prg`;
    		$cgi->delete('debug');
    		print $cgi->header(),
    		      $cgi->start_html('debug output of $FiGribCut'),
    		      $cgi->h1('debug output of $FiGribCut'),
    		      $cgi->h2('URL'),
    		      $cgi->escapeHTML($cgi->url(-path => 1, -query => 1)),
    		      $cgi->h2('command: '.$prg),
    		      $cgi->pre($cgi->escapeHTML($errMsg)),
    		      $cgi->end_html();
    	} else {
            print $cgi->header(-type => 'application/octet-stream',
                               -expires => '+5m',
                               -attachment => join('_', $cgi->param('fileName'),"").$$.'.grb');
            binmode STDOUT;    
	        system(@gribCutProg);
    	}
    }
}

# convert the cgi-parameters to gribCut system array
# return '' on succes, error-message otherwise
# analyze parameters:
#     @fileName
#     @parameter
#     $northernmost_latitude
#     $southernmost_latitude
#     $easternmost_longitude
#     $westernmost_longitude
sub params2gribCut(\@) {
	my ($gribCutProg) = @_;
	my $errors = '';
	
	# map fileNames to files
	my @fileNames = $cgi->param('fileName');
	if (!@fileNames) {
		return "no fileName given";
	}
    push @$gribCutProg, $FiGribCut;
	foreach my $fileName (@fileNames) {
		my $file = findFile($fileName);
		if (! ($file && -f $file)) {
			my $err = "no file given for $fileName";
			if ($file) {
                $err .= " at $file";
			}
			return $err;
		} else {
			push @$gribCutProg, '-i', $file;
		}
	}
	
	# parameter
	my @parameters = $cgi->param('parameter');
	foreach my $param (@parameters) {
		if ($param =~ /^\s*(\d+)\s*$/) {
    		push @$gribCutProg, '-p', $1;
		}
	}
	
	# boundingbox
	if (defined $cgi->param('easternmost_latitude') and $cgi->param('easternmost_latitude') =~ /\d/) {
		my ($north, $west, $south, $east) = map {$cgi->param($_)} qw(northernmost_latitude
		                                                             westernmost_longitude
		                                                             southernmost_latitude
		                                                             easternmost_longitude);
		if ((!defined $north) || ($north eq '')) {
			return "northernmost_latitude not defined";
		}
        if ((!defined $south) || ($south eq '')) {
            return "southernmost_latitude not defined";
        }
        if ((!defined $east) || ($east eq '')) {
            return "easternmost_longitude not defined";
        }
        if ((!defined $west) || ($west eq '')) {
            return "westernmost_longitude not defined";
        }
        # map to nummeric representation
        ($north, $west, $south, $east) = map {$_ + 0} ($north, $west, $south, $east);
        
        # check bounds
        if ($north < -90 or $north > 90) {
        	return "requirement -90 <= north ($north) <= 90 not fullfilled";
        } 
        if ($south < -90 or $south > 90) {
            return "requirement -90 <= south ($south) <= 90 not fullfilled";
        }
        if ($south > $north) {
        	return "south ($south) must be < than north($north)";
        } 
        if ($east < -180 or $east > 180) {
            return "requirement -180 <= east ($east) <= 180 not fullfilled";
        } 
        if ($west < -180 or $west > 180) {
            return "requirement -180 <= west ($west) <= 180 not fullfilled";
        } 
        if ($west > $east) {
            return "west ($west) must be < than east ($east)";
        }
        push @$gribCutProg, '-b', "$north,$east,$south,$west";
	}
	
	# the output file
	if ($cgi->param('debug')) {
		push @$gribCutProg, '-d';
		push @$gribCutProg, '-o', '/dev/null';
	} else {
		push @$gribCutProg, '-o', '-';
	}
	
	return undef;
}

sub findFile($) {
	my ($fileName) = @_;
	foreach my $fileHash (@Files) {
        if ($fileHash->{name} eq $fileName) {
        	return $fileHash->{file};
        }		
	}
	return undef;
}