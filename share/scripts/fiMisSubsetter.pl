#! /usr/bin/perl -w

# subsetter for MyOcean Information System
# requires fimex >0.25 compiled with netcdf4/dap support 

use 5.8.1;
use strict;
use warnings;

use CGI qw();
use CGI::Carp 'fatalsToBrowser';
use File::Temp qw(tempfile);
use File::Copy qw();
use Fcntl ':flock';
use Digest::MD5 qw(md5_hex);
$CGI::POST_MAX=1024 * 100;  # 100k 
$CGI::DISABLE_UPLOADS = 1;  # no uploads
use constant DEBUG => 1;


# SETUP, please modify
use constant FIMEX_BIN => '/home/heikok/bin/fimex';
# directory where the data will be stored and how to access that dir by url
use constant DELIVERY_DIR => ['/disk1/tmp/fimexData/' => 'http://myocean.met.no/fimexData'];
# connect service and product id to opendap-baseurl
use constant SERVICE_PRODUCT =>
{
    'METNO_OPENDAP' => {
        'myocean-class1-arctic' => 'http://thredds.met.no/thredds/dodsC/topaz/myocean/arctic/tmipv2a-class1-be'
    }
};
# reprocess files after 6 hours
use constant MAX_CACHE_AGE => 60*60*6;

# PROGRAM, do not modify


# example url
# ?action=productdownload&service=AvisoNRT&x_lo=0&x_hi=-0.33333333336929627&y_lo=-82&y_hi=81.9746&t_lo=2010-02-08&t_hi=2010-02-08&variable=Grid_0001&mode=console
our $cgi = new CGI();
our $action = $cgi->param('action') || '';
if ($action eq 'productdownload' or $action eq 'getsize') {
    processDownload($cgi);
} else {
    notAuthorized("action '$action' not known");    
}

sub processDownload {
    my ($cgi) = @_;
    my $service = $cgi->param('service') || '';
    my $product = $cgi->param('product') || '';
    my $opendapUrl = SERVICE_PRODUCT->{$service}{$product} || '';
    unless ($opendapUrl) {
        notAuthorized("unknown service '$service' or product '$product'");
        return;
    }
    my $mode = $cgi->param('mode') || '';
    
    
    # read the subsetting parameters
    my @variables = $cgi->param('variable');
    my %borders;
    # x = longitude, y = latitude
    foreach my $axis (qw(x y z t)) {
        my $low = $cgi->param($axis.'_lo'); 
        my $high = $cgi->param($axis.'_hi');
        if ((defined $low && defined $high) &&
            ($low =~ /\d/) && ($high =~ /\d/)) {
            $borders{$axis} = [$low, $high];
        }
    }
    
    # create the fimex command-line parameters
    my @fiParams;
    push @fiParams, "--input.file=$opendapUrl";
    push @fiParams, '--input.type=netcdf';
    push @fiParams, '--output.type=netcdf';
    push @fiParams, map {'--extract.selectVariables='.$_} @variables;
    # ignore x and y bounding box currently
    if (exists $borders{z}) {
        push @fiParams, '--extract.reduceVerticalAxis.start='.$borders{z}->[0];
        push @fiParams, '--extract.reduceVerticalAxis.end='.$borders{z}->[1];
        push @fiParams, '--extract.reduceVerticalAxis.unit=m';
    }
    if (exists $borders{t}) {
        push @fiParams, '--extract.reduceTime.start='.$borders{t}->[0];
        push @fiParams, '--extract.reduceTime.end='.$borders{t}->[1];
    }
    
    # fileId consists of service_product_ + md5sum of parameters
    my $fileId = $service . '_' . $product . '_' . md5_hex(join ':', @fiParams) . '.nc';
    my $filePath = DELIVERY_DIR()->[0] . $fileId;
    push @fiParams, '--output.file='.$filePath;
    
    # use external file to lock file-creation
    open my $fileLck, ">$filePath.lck"
        or die "cannot open $filePath.lck: $!";
    flock($fileLck, LOCK_SH);
    unless (-f $filePath and ((time - (stat(_))[9]) < MAX_CACHE_AGE()) ) {
        # file needs to be created
        flock($fileLck, LOCK_EX);
        print STDERR "executing: ". join(' ', (FIMEX_BIN, @fiParams)) . "\n" if DEBUG;
        system(FIMEX_BIN, @fiParams) == 0
            or die "system(".join(' ', (FIMEX_BIN, @fiParams)).") failed: $?";
        -f $filePath or die "'$filePath' has not been created by system(".join(' ', (FIMEX_BIN, @fiParams)).")\n";
        flock($fileLck, LOCK_SH);
    }
    
    if ($action eq 'productdownload') {
        deliverFile($cgi, $mode, $fileId);
    } elsif ($action eq 'getsize') {
        getSize($cgi, $fileId);
    } else {
        die "unknown action '$action'";
    }
    close($fileLck);
    unlink($fileLck);
}

sub notAuthorized {
    my ($msg) = @_;
    print $cgi->header(-status => '401 Unauthorized',
                       -type=>'text/plain');
    print $msg if $msg;
}

sub deliverFile {
    my ($cgi, $mode, $fileId) = @_;
    if ($mode eq 'url') {
        print $cgi->header(-type => 'text/plain',
                           -expires => '+'.MAX_CACHE_AGE().'s');
        print DELIVERY_DIR()->[1] . $fileId;
    } elsif ($mode eq 'console') {
        print $cgi->header(-type => 'application/x-netcdf',
                           -expires => '+'.MAX_CACHE_AGE().'s');
        File::Copy::copy(DELIVERY_DIR()->[0] . $fileId, \*STDOUT)
            or die "cannot copy ".DELIVERY_DIR()->[0] . $fileId . " to stdout: $!\n";
    } else {
        die "unknown mode '$mode'";
    }
}

sub getSize {
    my ($cgi, $fileId) = @_;
    my $size = (stat(DELIVERY_DIR()->[0] . $fileId))[7];
    print $cgi->header(-type => 'application/xml');
    print <<EOT;
<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>
<requestSize code="0" msg="OK" size="$size.0" maxAllowedSize="1.073741824E9" />
EOT
}