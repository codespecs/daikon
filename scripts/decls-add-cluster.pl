#!/usr/bin/env perl
## Adds the variable "cluster" to a decls file.
## usage: decls-add-cluster.pl @decls_files

use English;
use strict;
$WARNING = 1;			# "-w" flag

foreach my $decls_file (@ARGV) {
    $decls_file =~ /.*\/(\S*)\.decls/;
    my $decls_cluster = "$1_daikon_temp.decls";
    print " $decls_cluster ";
    open (IN, $decls_file) || die "couldn't open $decls_file\n";
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
