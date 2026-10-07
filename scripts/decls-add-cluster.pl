#!/usr/bin/env perl

## Usage: decls-add-cluster.pl decls_file ...

## Adds the variable "cluster" to one or more decls files as the first
## variable at each program point.  The original decls files are left
## unchanged, and the result for BASENAME.decls is output to file
## BASENAME_runcluster_temp.decls in the current directory.  Furthermore, the list of output files is written
## to standard out.

use English;
use strict;
$WARNING = 1;			# "-w" flag

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);

# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

# Maps each output file name to the input file that it was created from.
my %output_to_input = ();

foreach my $decls_file (@ARGV) {
    my $decls_cluster = basename($decls_file);
    $decls_cluster =~ s/\.decls$//;
    $decls_cluster .= "_runcluster_temp.decls";
    if (exists($output_to_input{$decls_cluster})) {
	die "$decls_file and $output_to_input{$decls_cluster} would both be output to $decls_cluster\n";
    }
    $output_to_input{$decls_cluster} = $decls_file;
    open (IN, $decls_file) || die "couldn't open $decls_file for input\n";
    open (OUT, ">$decls_cluster") || die "couldn't open $decls_cluster for output\n";

    my $ppt_seen = 0;
    while (defined(my $record = read_record(\*IN, $decls_file))) {
	print OUT $record->{comments};
	my $text = $record->{text};
	next if $text eq "";
	$text .= "\n" if $text !~ /\n\z/;
	if ($record->{kind} eq "ppt") {
	    $ppt_seen = 1;
	    check_no_cluster_var(parse_ppt_decl($text), $decls_file);
	    $text = add_cluster_var($text);
	}
	print OUT $text, "\n";
    }
    if (!$ppt_seen) {
	die "No program point declarations in $decls_file\n";
    }
    close IN;
    close OUT;
    print " $decls_cluster ";
}

# Returns the program point declaration paragraph that is the first
# argument, with the cluster variable added.  The cluster variable must
# precede the other variables, but must follow the ppt-level records such
# as ppt-type.
sub add_cluster_var {
    my ($record) = @_;
    my @lines = split(/^/m, $record);
    my $result = "";
    my $pending = 1;
    # The "parent"-type parent records of the program point, as
    # "<parent-ppt-name> <relation-id>" strings.  The cluster variable is
    # linked to the cluster variable of each such parent, so that, for
    # example, an OBJECT program point gets cluster values from its methods.
    # "user"-type relations are not followed:  they link a program point
    # to an unrelated program point, such as the OBJECT program point of a
    # parameter's class.
    my @parents = ();
    foreach my $line (@lines) {
	if ($pending && $line =~ /^\s*variable\s/) {
	    $result .= cluster_var_decl(@parents);
	    $pending = 0;
	}
	$result .= $line;
	if ($pending && $line =~ /^\s*parent\s+parent\s+(\S+)\s+(\S+)\s*$/) {
	    push @parents, "$1 $2";
	}
    }
    if ($pending) {
	$result .= cluster_var_decl(@parents);
    }
    return $result;
}

# Returns the declaration of the cluster variable.  The arguments are the
# "<parent-ppt-name> <relation-id>" strings for the program point's parents.
sub cluster_var_decl {
    my (@parents) = @_;
    my $result = "  variable cluster\n"
      . "    var-kind variable\n"
      . "    dec-type int\n"
      . "    rep-type int\n"
      . "    comparability 22\n";
    foreach my $parent (@parents) {
	$result .= "    parent $parent\n";
    }
    return $result;
}
