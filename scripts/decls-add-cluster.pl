#!/usr/bin/env perl

## Usage: decls-add-cluster.pl decls_file ...

## Adds the variable "cluster" to one or more decls files as the first
## variable at each program point.  The original decls files are left
## unchanged, and the result is output to file *_runcluster_temp.decls in the
## current directory.  Furthermore, the list of output files is written
## to standard out.
##
## Each input file may be in the old or the new (version 2.0) declaration
## format, and may be compressed with gzip.  An input file may also be a
## dtrace file that contains declarations; its samples are not copied to the
## output.  In the new format, the variable is not added to :::OBJECT and
## :::CLASS program points, which have no samples.

use English;
use strict;
$WARNING = 1;			# "-w" flag

use File::Basename;

foreach my $decls_file (@ARGV) {
    my $decls_cluster = basename($decls_file);
    $decls_cluster =~ s/\.(decls|dtrace)(\.gz)?$//;
    $decls_cluster .= "_runcluster_temp.decls";
    print " $decls_cluster ";
    if ($decls_file =~ /\.gz$/) {
	open (IN, "zcat $decls_file |") || die "couldn't open $decls_file for input with zcat\n";
    } else {
	open (IN, $decls_file) || die "couldn't open $decls_file for input\n";
    }
    open (OUT, ">$decls_cluster") || die "couldn't open $decls_cluster for output\n";

    # Process the file one paragraph (a group of lines ended by a blank line) at a time.
    my @paragraph = ();
    while (my $line = <IN>) {
	if ($line =~ /^\s*$/) {
	    &output_paragraph(@paragraph);
	    @paragraph = ();
	} else {
	    push @paragraph, $line;
	}
    }
    &output_paragraph(@paragraph);
    close IN;
    close OUT;
}

# Print the given paragraph, which is a list of lines, to OUT, adding the
# "cluster" variable if the paragraph is a program point declaration.
sub output_paragraph ( @ ) {
    my @lines = @_;
    if (scalar(@lines) == 0) {
	return;
    }
    if ($lines[0] =~ /^DECLARE$/) {
	# Old format: the program point name, then 4 lines per variable.
	print OUT $lines[0], $lines[1];
	print OUT "cluster\n";
	print OUT "int\n";
	print OUT "int\n";
	print OUT "22\n";
	print OUT @lines[2..$#lines];
    } elsif ($lines[0] =~ /^ppt\s/) {
	# New format: insert the variable before the first variable declaration.
	my $add = ($lines[0] !~ /:::(OBJECT|CLASS)\s*$/);
	foreach my $line (@lines) {
	    if ($add && $line =~ /^\s*variable\s/) {
		print OUT "variable cluster\n";
		print OUT "  var-kind variable\n";
		print OUT "  dec-type int\n";
		print OUT "  rep-type int\n";
		$add = 0;
	    }
	    print OUT $line;
	}
	if ($add) {
	    print OUT "variable cluster\n";
	    print OUT "  var-kind variable\n";
	    print OUT "  dec-type int\n";
	    print OUT "  rep-type int\n";
	}
    } elsif ($lines[0] =~ /:::/) {
	# A sample in a dtrace file.
	return;
    } else {
	# Other information, such as comments or the declaration format.
	print OUT @lines;
    }
    print OUT "\n";
}
