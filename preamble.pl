#!/usr/bin/perl -w
use strict;
our $preamble = <<'EOF';
/*
 * Fimex
 * 
 * (C) Copyright 2008-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

EOF

our $extension = '.orig';
our $oldargv = '';
LINE: while (<>) {
    my $backup;
    if ($ARGV ne $oldargv) {
	if ($extension !~ /\*/) {
	    $backup = $ARGV . $extension;
	} else {
	    ($backup = $extension) =~ s/\*/$ARGV/g;
	}
	rename($ARGV, $backup);
	open(ARGVOUT, ">$ARGV");
	select(ARGVOUT);
	$oldargv = $ARGV;
	print $preamble;
    }
#    s/foo/bar/;
} continue {
    print;				# this prints to original filename
}
select(STDOUT);
