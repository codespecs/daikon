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

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);

# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

# The declaration of the "cluster" variable.
my $cluster_decl = "variable cluster\n"
    . "  var-kind variable\n"
    . "  dec-type int\n"
    . "  rep-type int\n";

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

    # Process the file one record at a time.  The record reader separates
    # comments and one-line headers (such as "var-comparability none") from
    # the record that follows them, even if no blank line intervenes.
    my $next_record = record_reader(\*IN, $decls_file);
    while (defined(my $record = $next_record->())) {
	&output_record($record);
    }
    close IN;
    close OUT;
}

# Print the given record, as returned by a record reader, to OUT, adding the
# "cluster" variable if the record is a program point declaration.
sub output_record ( $ ) {
    my ($record) = @_;
    print OUT $record->{comments};
    my $kind = $record->{kind};
    if ($kind eq "") {
	# Comments at the end of the file.
	return;
    }
    my $text = $record->{text};
    if ($text !~ /\n\z/) {
	$text .= "\n";
    }
    if ($kind eq "header") {
	# A one-line header, such as "decl-version 2.0", which needs no blank line.
	print OUT $text;
	return;
    } elsif ($kind eq "data") {
	# A sample in a dtrace file.
	return;
    } elsif ($kind eq "ppt") {
	# Insert the variable before the first variable declaration.
	my @lines = split(/^/, $text);
	my $add = ($lines[0] !~ /:::(OBJECT|CLASS)\s*$/);
	foreach my $line (@lines) {
	    if ($add && $line =~ /^\s*variable\s/) {
		print OUT $cluster_decl;
		$add = 0;
	    }
	    print OUT $line;
	}
	if ($add) {
	    print OUT $cluster_decl;
	}
    } else {
	# Other information, such as a ListImplementors record.
	print OUT $text;
    }
    print OUT "\n";
}
