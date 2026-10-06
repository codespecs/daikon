#!/usr/bin/env perl

## Usage: decls-add-cluster.pl decls_file ...

## Adds the variable "cluster" to one or more decls files as the first
## variable at each program point.  The original decls files are left
## unchanged, and the result is output to file *_runcluster_temp.decls in the
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

    # True if the current program point declaration still needs the
    # cluster variable.  The cluster variable must precede the other
    # variables, but must follow the ppt-level records such as ppt-type.
    my $pending = 0;
    while (<IN>) {
	my $line = $_;
	if ($pending && ($line =~ /^\s*variable\s/ || $line =~ /^\s*$/)) {
	    print_cluster_var();
	    $pending = 0;
	}
	print OUT $line;
	if ($line =~ /^ppt /) {
	    $pending = 1;
	}
    }
    if ($pending) {
	print_cluster_var();
    }
    close IN;
    close OUT;
}

# Prints the declaration of the cluster variable.
sub print_cluster_var {
    print OUT "  variable cluster\n";
    print OUT "    var-kind variable\n";
    print OUT "    dec-type int\n";
    print OUT "    rep-type int\n";
    print OUT "    comparability 22\n";
}
