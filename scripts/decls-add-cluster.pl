#!/usr/bin/env perl

## Usage: decls-add-cluster.pl decls_file ...

## Adds the variable "cluster" to one or more decls files as the first
## variable at each program point.  The original decls files are left
## unchanged, and the result is output to file *_runcluster_temp.decls in the
## current directory.  If several input files have the same basename, a
## number is added to all but the first one's output file name.  Furthermore, the list of output files is written
## to standard out.
##
## Each input file must be in the version 2 declaration format, and may be
## compressed with gzip.  An input file may also be a dtrace file that
## contains declarations; its samples are not copied to the output.  The
## variable is not added to :::OBJECT and :::CLASS program points, which have
## no samples.

use English;
use strict;
$WARNING = 1;			# "-w" flag

use File::Basename;

# The output file names used so far, to keep inputs with the same basename
# from overwriting one another's output.
my %used_names = ();

foreach my $decls_file (@ARGV) {
    my $base = basename($decls_file);
    $base =~ s/\.(decls|dtrace)(\.gz)?$//;
    my $decls_cluster = "${base}_runcluster_temp.decls";
    for (my $i = 2; exists($used_names{$decls_cluster}); $i++) {
	$decls_cluster = "${base}-${i}_runcluster_temp.decls";
    }
    $used_names{$decls_cluster} = 1;
    print " $decls_cluster ";
    if ($decls_file =~ /\.gz$/) {
	open (IN, "-|", "zcat", $decls_file) || die "couldn't open $decls_file for input with zcat\n";
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
    # The first line that is not a comment (as recognized by
    # daikon.FileIO.isComment) determines the kind of the paragraph.
    my $first = undef;
    foreach my $line (@lines) {
	if ($line !~ /^(\/\/|#)/) {
	    $first = $line;
	    last;
	}
    }
    if (!defined($first)) {
	# The paragraph consists only of comments.
	print OUT @lines;
    } elsif ($first =~ /^ppt\s/) {
	# Insert the variable before the first variable declaration.
	my $add = ($first !~ /:::(OBJECT|CLASS)\s*$/);
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
    } elsif ($first =~ /:::/) {
	# A sample in a dtrace file.
	return;
    } else {
	# Other information, such as comments or the declaration format.
	print OUT @lines;
    }
    print OUT "\n";
}
