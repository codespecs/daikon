#!/usr/bin/env perl

use English;
use strict;
$WARNING = 1;			# "-w" flag

my $decls_file = $ARGV[0];
my $decls_new;
if (scalar(@ARGV) == 1) {
  $decls_file =~ /.*\/(\S*)\.decls/;
  $decls_new = "$1_new.decls";
} elsif (scalar(@ARGV) == 2) {
  $decls_new = $ARGV[1];
} else {
  die "Wrong number of arguments (" . scalar(@ARGV) . ") to decls-add-cluster.pl";
}

open (IN, $decls_file) || die "couldn't open $decls_file\n";
open (OUT, ">$decls_new") || die "couldn't open $decls_new for output\n";

while (<IN>) {
    my $line = $_;
    print OUT $line;
    if ($line =~ /:::/) {
	print OUT "cluster\n";
	print OUT "int\n";
	print OUT "int\n";
	print OUT "22\n";
    }
}

close IN;
close OUT;
