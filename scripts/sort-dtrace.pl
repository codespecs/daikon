#!/usr/bin/env perl

# Canonicalize a .decls, .dtrace, or combined .dtrace file by sorting
# PPT declarations, and the variables within each program point, into
# alphabetical order. Each continguous series of DECLARE paragraphs is
# reordered, while trace paragraphs remain in the same order as in the
# original file. Should be semantics-preserving.

use strict;
use 5.006;
use warnings;

my @decls;

sub flush_decls {
    if (@decls) {
	@decls = sort @decls;
	print join("\n\n", @decls), "\n\n";
	@decls = ();
    }
}

$/ = ""; # Read by paragraph

while (<>) {
    if ((/^VarComparability/) || (/^decl/) || (/^input/)) {
	print;
	next;
    } elsif (/^DECLARE/) {
	chomp;
	my @lines = split(/\n/, $_);
	die "Bad .decls paragraph format" if shift @lines ne "DECLARE";
	my $ppt_name = shift @lines;
	die "Bad number of lines" unless @lines % 4 == 0;
	my @vars;
	while (@lines) {
	    push @vars, join("\n", splice(@lines, 0, 4));
	}
	@vars = sort @vars;
	push @decls, join("\n", "DECLARE", $ppt_name, @vars);
    } else {
	flush_decls();
	chomp;
	my @lines = split(/\n/, $_);
	my @header;
	push @header, shift @lines;
	if (@lines and $lines[0] eq "this_invocation_nonce") {
	    push @header, shift @lines;
	    push @header, shift @lines;
	}
	die "Bad number of lines" unless @lines % 3 == 0;
	my @vars;
	while (@lines) {
	    push @vars, join("\n", splice(@lines, 0, 3));
	}
	@vars = sort @vars;
	print join("\n", @header, @vars), "\n\n";
    }
    flush_decls() if eof;
}
