#! /usr/bin/perl -w

=head1 NAME

fiConfigOverview - create overview tables of fimex config

=head1 SYNOPSIS

fiConfigOverview -i DIR/FILES -o OUT.html

=head1 DESCRIPTION

fiConfigOverview read a single or a directory of fimex input/output
xml-configuration files and writes an overview-table in html format.

=head2 OPTIONS

=over8

=item -h

help

=item -i

comma-separated list of files or directories

=item -o

output-file

=back

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

https://github.com/metno/fimex/wiki

=cut

use 5.6.1;
use strict;
use warnings;
use Pod::Usage qw(pod2usage);
use Getopt::Std qw(getopts);
use File::Find  qw();

our %Args;
our @Args = @ARGV;
$Args{o} = '-';
getopts('hi:o:', \%Args)
    or pod2usage(2);
pod2usage(-exitval => 0,
          -verbose => 2) if $Args{h};
pod2usage(-exitval => 2,
          -msg => "Unkown emission input -i") unless ($Args{i});

File::Find::find({
    wanted => sub {-f $_ && Fimex::Config::parseFile($File::Find::name);},
    follow => 1,
    no_chdir => 1,
}, split (',', $Args{i}));

Fimex::Config::writeHtml($Args{o});


#####################################################################

package Fimex::Config;
use Carp qw(carp);
our %content; # type => id => Fimex::Config::Content object

use constant DEBUG => 0;

use constant NAMESPACES => {
    gr => 'http://www.met.no/schema/fimex/cdmGribReaderConfig',
    ncml => 'http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2',
};

use constant IGNORE_APX => [qw(dtd xsd cfg ~)];

use constant FILETYPES => {
    "gr:cdmGribReaderConfig" => \&parseGribReader,
    'ncml:netcdf' => \&ignore,
    "cdm_felt_config" => \&parseFeltConfig,
        # XINCLUDEs for cdm_felt_config
        "variables" => \&parseFeltConfig,
        'global_attributes' => \&ignore,
        'axes' => \&ignore,
    'cdm_gribwriter_config' => \&parseGribWriter,
    'cdm_ncwriter_config' => \&ignore,
    'cdmQualityConfig' => \&ignore,
    'gribFileIndex' => \&ignore,
};


sub parseFile {
    my ($file) = @_;

    # ignore all .-files or files in .-directories
    return if $file =~ /\/\./;
    foreach my $apx (@{IGNORE_APX()}) {
        if ($file =~ /\Q$apx\E$/) {
            print STDERR "ignore $file $apx\n" if DEBUG;
            return;
        }
    }

    # late loading of external modules
    require XML::LibXML;
    require XML::LibXML::XPathContext;

    eval {
        my $doc = XML::LibXML->load_xml(location => $file,
                                        no_blanks => 1);
        my $xpc = XML::LibXML::XPathContext->new($doc);
        while (my ($prefix, $uri) = each %{NAMESPACES()}) {
            $xpc->registerNs($prefix, $uri);
        }
        my $found = 0;
        my @content;
        foreach my $rootPath (keys %{FILETYPES()}) {
            if ($xpc->exists($rootPath)) {
                @content = FILETYPES->{$rootPath}->($file, $xpc, $doc);
                $found++;
                print STDERR "found $rootPath in $file\n" if DEBUG;
                last;
            }
        }
        if ($found) {
            addContent(@content);
        } else {
            print STDERR "unknown xml-file: $file\n";
        }
    }; if ($@) {
        if ($@ =~ /parser error/) {
            print STDERR "invalid xml-file: $file\n";
        } else {
            die $@;
        }
        return;
    }
}

sub addContent {
    my @content = @_;
    foreach my $c (@content) {
        if (exists $content{$c->type}{$c->id}) {
            $content{$c->type}{$c->id}->merge($c);
        } else {
            $content{$c->type}{$c->id} = $c;
        }
    }
}

sub parseGribReader {
    my ($filename, $xpc, $doc) = @_;
    my @content;
    foreach my $node ($xpc->findnodes("/gr:cdmGribReaderConfig/gr:variables/gr:parameter")) {
        my $xml = $node->toString(1);
        my $varName;
        my %attr = map {$_->getName => $_->getValue} $node->attributes;
        $varName = $attr{name};
        my ($longName, $metnoName, $unit, $standardName);
        foreach my $attNode ($xpc->findnodes("gr:attribute", $node)) {
            my %attr = map {$_->getName => $_->getValue} $attNode->attributes;
            if ($attr{name} eq "units") {
                $unit = $attr{value};
            } elsif ($attr{name} eq "long_name") {
                $longName = $attr{value};
            } elsif ($attr{name} eq "standard_name") {
                $standardName = $attr{value};
            } elsif ($attr{name} eq "metno_name") {
                $metnoName = $attr{value};
            }
        }
        foreach my $param ($xpc->findnodes("gr:grib1", $node)) {
            my %attr = map {$_->getName => $_->getValue} $param->attributes;
            my @idTerms = map {exists $attr{$_} ? ($attr{$_}) : ("")} qw(indicatorOfParameter gribTablesVersionNo identificationOfOriginatingGeneratingCentre typeOfLevel);
            my $id = join ",", @idTerms;
            push @content, Fimex::Config::Content::Grib1->new($id, $xml, $filename, $unit, $standardName, $varName, $longName, $metnoName);
        }
        foreach my $param ($xpc->findnodes("gr:grib2", $node)) {
            my %attr = map {$_->getName => $_->getValue} $param->attributes;
            my @idTerms = map {exists $attr{$_} ? ($attr{$_}) : ("")} qw(discipline parameterCategory parameterNumber typeOfLevel);
            my $id = join ",", @idTerms;
            push @content, Fimex::Config::Content::Grib2->new($id, $xml, $filename, $unit, $standardName, $varName, $longName, $metnoName);
        }
    }
    return @content;
}

sub parseGribWriter {
    my ($fileName, $xpc, $doc) = @_;
    my @content;
    foreach my $node ($xpc->findnodes("//variables/parameter")) {
        my $xml = $node->toString(1);
        my ($varName, $level, $standardName);
        my ($longName, $metnoName) = ("", "");
        my %attr = map {$_->getName => $_->getValue} $node->attributes;
        $varName = $attr{name} || "";
        $level = $attr{level} || "";
        $standardName = $attr{standard_name} || "";
        foreach my $param ($xpc->findnodes("grib1", $node)) {
            my %attr = map {$_->getName => $_->getValue} $param->attributes;
            my @idTerms = map {exists $attr{$_} ? ($attr{$_}) : ("")} qw(parameterNumber codeTable identificationOfOriginatingGeneratingCentre typeOfLevel);
            my $id = join ",", @idTerms;
            $id .= ",level=$level" if $level;
            push @content, Fimex::Config::Content::Grib1->new($id, $xml, $fileName, $attr{units}, $standardName, $varName, $longName, $metnoName);
        }
        foreach my $param ($xpc->findnodes("grib2", $node)) {
            my %attr = map {$_->getName => $_->getValue} $param->attributes;
            my @idTerms = map {exists $attr{$_} ? ($attr{$_}) : ("")} qw(discipline parameterCategory parameterNumber typeOfLevel);
            my $id = join ",", @idTerms;
            $id .= ",level=$level" if $level;
            push @content, Fimex::Config::Content::Grib2->new($id, $xml, $fileName, $attr{units}, $standardName, $varName, $longName, $metnoName);
        }
    }
    return @content;
}

sub parseFeltConfig {
    my ($fileName, $xpc, $doc) = @_;
    my @content;
    foreach my $node ($xpc->findnodes("//variables/parameter")) {
        my $xml = $node->toString(1);
        my ($varName, $id);
        my %attr = map {$_->getName => $_->getValue} $node->attributes;
        $varName = $attr{name};
        $id = $attr{id};
        my ($longName, $metnoName, $unit, $standardName);
        foreach my $attNode ($xpc->findnodes("attribute", $node)) {
            my %attr = map {$_->getName => $_->getValue} $attNode->attributes;
            if ($attr{name} eq "units") {
                $unit = $attr{value};
            } elsif ($attr{name} eq "long_name") {
                $longName = $attr{value};
            } elsif ($attr{name} eq "standard_name") {
                $standardName = $attr{value};
            } elsif ($attr{name} eq "metno_name") {
                $metnoName = $attr{value};
            }
        }
        push @content, Fimex::Config::Content::Felt->new($id, $xml, $fileName, $unit, $standardName, $varName, $longName, $metnoName);
    }
    return @content;
}

sub ignore {
    my ($fileName, $xpc, $doc) = @_;
    print STDERR "ignore $fileName\n" if DEBUG;
    return ();
}


sub writeHtml {
    my ($outFile) = @_;

    open HTML, ">$outFile"
        or die "Cannot write $outFile: $!\n";
    print HTML <<'EOT';
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<meta name="author" content="Heiko Klein, heiko.klein@met.no">
<style type="text/css">
<!--
#main
{
  padding: 0;
  margin: 0;
  border-collapse: collapse;
  border: 1px solid #333;
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 0.9em;
  color: #000;
}

#main caption
{
  caption-side: bottom;
  font-size: 0.9em;
  font-style: italic;
  text-align: right;
  padding: 0.5em 0;
}

#main td
{
  border: 1px dotted #666;
  padding: 0.5em;
  text-align: left;
}

.hidden
{
    display: none;
}

.explanation {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 0.9em;
}

-->
</style>
<link rel="stylesheet" href="http://code.jquery.com/ui/1.8.21/themes/redmond/jquery-ui.css" type="text/css" media="all" />
<script src="http://code.jquery.com/jquery-1.7.2.min.js" type="text/javascript"></script>
<script src="http://fimex.met.no/js/colResizable-1.3.min.js" type="text/javascript"></script>
<script src="http://code.jquery.com/ui/1.8.21/jquery-ui.min.js" type="text/javascript"></script>
<script src="http://fimex.met.no/js/stupidtable.min.js" type="text/javascript"></script>
<script type="text/javascript">
<!--
"use strict";
EOT
    print HTML 'var fiConf = ', Fimex::JSON::encode(\%content),";\n";
    print HTML <<'EOT';
-->
</script>
<script type="text/javascript">
<!--
"use strict";
// indexed-array to all rows from fiConf
var fiConfRows = [];

function tableRowDialog(elem, rowNo) {
    $(document.createElement('div')).attr({title: fiConfRows[rowNo]["varName"] + " " + $(elem).text()})
    .append(fiConfRows[rowNo]["fileName"])
    .append($(document.createElement('pre')).text(fiConfRows[rowNo]["xml"]))
    .dialog({ width: 640 });
}

$(document).ready(function() {
var cols = ["varName", "unit", "standardName", "grib1Id", "grib2Id", "feltId", "longName", "metnoName"];
var type, id, i, j, k;
var $col, $row;
var rowNo, rowObj;
var uniqXml = {}; /* xml -> rowId */
for (type in fiConf) {
    for (id in fiConf[type]) {
        ROW: for (i = 0; i < fiConf[type][id]["varName"].length; ++i) {
            if (fiConf[type][id]["xml"][i] in uniqXml) {
                /* row exists already, add fileName and continue */
                rowNo = uniqXml[fiConf[type][id]["xml"][i]];
                /* fiConfRows[rowNo]["fileName"] += ", " + fiConf[type][id]["fileName"][i]; */
                for ( k in fiConf[type][id] ) {
                    if ((i < fiConf[type][id][k].length) && (fiConf[type][id][k][i].length > 0)) {
                        if (fiConfRows[rowNo][k] != fiConf[type][id][k][i]) {
                            if (k in fiConfRows[rowNo] && fiConfRows[rowNo][k]) {
                                fiConfRows[rowNo][k] += "; " + fiConf[type][id][k][i];
                            } else {
                                fiConfRows[rowNo][k] = fiConf[type][id][k][i];
                            }
                        }
                    }
                }
                continue ROW;
            }
            rowNo = fiConfRows.length;
            uniqXml[fiConf[type][id]["xml"][i]] = rowNo;
            rowObj = {};
            for (k in fiConf[type][id]) {
                rowObj[k] = fiConf[type][id][k][i];
            }
            fiConfRows.push(rowObj);
        }
    }
}
for (i = 0; i < fiConfRows.length; ++i) {
    $row = $(document.createElement('tr'));
    $("#main").append($row);
    for (j = 0; j < cols.length; ++j) {
        $(document.createElement('td'))
        .click({"no": i}, function(evt) {tableRowDialog(this, evt.data.no);})
        .append(fiConfRows[i][cols[j]])
        .appendTo($row);
    }
}
$("#main").colResizable();
$("#main").stupidtable();

});
-->
</script>
<title>Fimex Config Overview</title>
</head>
<body>
<h1 class="ui-widget">Fimex Config Overview</h1>
<div class="explanation">Click table-header to sort. Click table-content to get full information.</div>
EOT
    my $datestamp = gmtime(time);
    print HTML <<"EOT";
<table id='main' width="100%">
<caption>generated on $datestamp UTC with: $0 @main::Args</caption>
<thead class="ui-widget-header">
<tr><th class="type-string">variable</th><th class="type-string">units</th><th class="type-string">standard_name</th><th class="type-int">grib1</th><th class="type-int">grib2</th><th class="type-int">felt-id</th><th class="type-string">long_name</th><th class="type-string">metno_name</th></tr>
</thead>
<tbody></tbody>
</table>
</html>
EOT
    close HTML;
}

########################################################################
# minimum requirements to fill data with
package Fimex::Config::Content;


sub mk_listaccessors_ {
    my ($class, @fields) = @_;
    foreach my $f (@fields) {
        my $func = sub {
            my ($self, $fData) = @_;
            push @{$self->{$f}}, $fData if defined $fData;
            return @{ $self->{$f} };
        };
        my $funcName = $class . '::' . $f;
        no strict 'refs';
        *{$funcName} = $func;
    }
}

BEGIN {
    __PACKAGE__->mk_listaccessors_(qw(unit varName xml fileName longName standardName metnoName grib1Id grib2Id feltId));
    die unless UNIVERSAL::can(__PACKAGE__, 'xml');
}

sub new {
    my ($package, $id, $xml, $filename, $unit, $standardName, $varName, $longName, $metnoName) = @_;
    die "don't use interface, use implementation" if ($package eq __PACKAGE__);
    my $self = {
        id => $id,
        unit => [],
        varName => [],
        longName => [],
        standardName => [],
        metnoName => [],
        grib1Id => [],
        grib2Id => [],
        feltId => [],
        xml => [],
        fileName => [],
    };
    bless $self, $package;
    $self->xml($xml || "");
    $self->unit($unit || "");
    $self->fileName($filename || "");
    $self->standardName($standardName || "");
    $self->varName($varName || "");
    $self->longName($longName || "");
    $self->metnoName($metnoName || "");
    return $self;
}

sub type {
    die "not implemented in base";
}

sub id {
    return $_[0]->{id};
}

=head2 merge(other)

merge information from $other to this object

=cut

sub merge {
    my ($self, $other) = @_;
    if ($self->isa($other)) {
        die "cannot merge different types";
    }
    foreach my $key (%{ $other }) {
        if (ref($self->{$key}) eq 'ARRAY') {
            push @{ $self->{$key} }, @{$other->{$key}};
        }
    }

}

package Fimex::Config::Content::Grib1;
use base qw(Fimex::Config::Content);

sub new {
    my $self = Fimex::Config::Content::new(@_);
    $self->grib1Id($self->id);
    return $self;
}

sub type {
    return "grib1";
}

package Fimex::Config::Content::Grib2;
use base qw(Fimex::Config::Content);

sub new {
    my $self = Fimex::Config::Content::new(@_);
    $self->grib2Id($self->id);
    return $self;
}

sub type {
    return "grib2";
}

package Fimex::Config::Content::Felt;
use base qw(Fimex::Config::Content);

sub new {
    my $self = Fimex::Config::Content::new(@_);
    $self->feltId($self->id);
    return $self;
}

sub type {
    return "felt";
}


#####################################################################
package Fimex::JSON;
use Scalar::Util qw(reftype);
sub encode {
    my ($var) = @_;
    my $ref = reftype $var;
    if (!defined $ref) {
        return encodeScalar($var);
    } elsif ($ref eq 'SCALAR') {
        return encodeScalar($$var);
    } elsif ($ref eq 'HASH') {
        return encodeHashRef($var);
    } elsif ($ref eq 'ARRAY') {
        return encodeArrayRef($var);
    }
}

sub encodeHashRef {
    my ($h) = @_;
    my %out;
    while (my ($key, $val) = each %$h) {
        $out{$key} = encode($val);
    }
    return "{". join(',', map {encodeScalar($_).': '. $out{$_} } keys %out) ."}";
}

sub encodeArrayRef {
    my ($a) = @_;
    my @out;
    foreach my $e (@$a) {
        push @out, encode($e);
    }
    return "[". join(',', @out) . "]";
}

sub encodeScalar {
    my ($s) = @_;

    # escape
    $s =~ s:\\:\\\\:g;
    $s =~ s:/:\\/:g;
    $s =~ s:":\\":g;
    $s =~ s:\r:\\r:g;
    $s =~ s:\n:\\n:g;
    $s =~ s:\t:\\t:g;
    return '"'. $s . '"';
}
