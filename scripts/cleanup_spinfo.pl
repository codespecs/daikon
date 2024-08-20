#!/usr/bin/env perl

# remove uninteresting splitting conditions from a file of splitting conditions
# Reads from standard input and prints to standard out.
# Removes all conditions that compare variables against numbers, unless the numbers
# are -2, -1, 0, 1, 2

use English;
use strict;
$WARNING = 1;          # -w flag

my %all_conditions = ();

nextline:
    while (<>) {
	if ( /(\bplume\b|warning|\.class)/ ) {
	    next;
	} elsif ( /for\s*\(int/ ) {
	    next;
	} elsif (/\[.*?\.\.\.*?\]/) {
	    next;
	} else {
	    # we don't want splitting conditions comparing a variable against a
	    # number, except if the number is -2, -1 , 0, 1, 2,
	    my $afterString = $_;
	    while ($afterString =~ /^.*?(\d+)/) {
		$afterString = $';
		my $number = $1;
		if ($number !~ /^\s*-?(0|1|2)\s*$/) {
		    next nextline;
		}
	    }
	    my $hashkey = $_;
	    $hashkey =~ s/\s*//;

	    if ( ! exists $all_conditions{$hashkey} ) {
		$all_conditions{$hashkey} = "1";
		if (/PPT_NAME/) {
		    print "\n";
		}
		print $_;
	    }
	}
    }
