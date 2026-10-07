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
    # The parent records of the current program point, as
    # "<parent-ppt-name> <relation-id>" strings.  The cluster variable is
    # linked to the cluster variable of each such parent, so that, for
    # example, an OBJECT program point gets cluster values from its methods.
    my @parents = ();
    while (<IN>) {
	my $line = $_;
	if ($pending && ($line =~ /^\s*variable\s/ || $line =~ /^\s*$/)) {
	    print_cluster_var(@parents);
	    $pending = 0;
	}
	print OUT $line;
	if ($line =~ /^ppt /) {
	    $pending = 1;
	    @parents = ();
	} elsif ($pending && $line =~ /^\s*parent\s+\S+\s+(\S+)\s+(\S+)\s*$/) {
	    push @parents, "$1 $2";
	}
    }
    if ($pending) {
	print_cluster_var(@parents);
    }
    close IN;
    close OUT;
}

# Prints the declaration of the cluster variable.  The arguments are the
# "<parent-ppt-name> <relation-id>" strings for the program point's parents.
sub print_cluster_var {
    my (@parents) = @_;
    print OUT "  variable cluster\n";
    print OUT "    var-kind variable\n";
    print OUT "    dec-type int\n";
    print OUT "    rep-type int\n";
    print OUT "    comparability 22\n";
    foreach my $parent (@parents) {
	print OUT "    parent $parent\n";
    }
}
