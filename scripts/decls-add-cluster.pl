#!/usr/bin/env perl

## Usage: decls-add-cluster.pl decls_file ...

## Adds the variable "cluster" to one or more decls files as the first
## variable at each program point.  The original decls files are left
## unchanged, and the result is ouput to file *_runcluster_temp.decls in the
## current directory.  Furthermore, the list of output files is written
## to standard out.

use English;
use strict;
$WARNING = 1;			# "-w" flag

foreach my $decls_file (@ARGV) {
    $decls_file =~ /.*\/(\S*)\.decls/;
    my $decls_cluster = "$1_runcluster_temp.decls";
    print " $decls_cluster ";
    open (IN, $decls_file) || die "couldn't open $decls_file for input\n";
    open (OUT, ">$decls_cluster") || die "couldn't open $decls_cluster for output\n";

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
}
