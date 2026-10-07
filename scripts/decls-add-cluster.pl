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

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);

# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

foreach my $decls_file (@ARGV) {
    $decls_file =~ /.*\/(\S*)\.decls/;
    my $decls_cluster = "$1_runcluster_temp.decls";
    open (IN, $decls_file) || die "couldn't open $decls_file for input\n";
    open (OUT, ">$decls_cluster") || die "couldn't open $decls_cluster for output\n";

    my $ppt_seen = 0;
    local $INPUT_RECORD_SEPARATOR = ""; # Read by paragraph
    while (my $para = <IN>) {
	# Daikon does not require a blank line after a comment.
	my ($comments, $record) = split_leading_comments($para);
	print OUT $comments;
	if ($record eq "" || record_kind($record, $decls_file) ne "ppt") {
	    print OUT $record;
	    next;
	}
	$ppt_seen = 1;
	print OUT add_cluster_var($record, $decls_file);
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
# as ppt-type.  The second argument is the file name, for error messages.
sub add_cluster_var {
    my ($record, $decls_file) = @_;
    my @lines = split(/^/m, $record);
    my $pptname = $lines[0];
    $pptname =~ s/^ppt\s+//;
    $pptname =~ s/\s+$//;
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
	if ($line =~ /^\s*variable\s+(.*?)\s*$/ && unescape_decl($1) eq "cluster") {
	    die "Program point " . unescape_decl($pptname) . " in $decls_file already has a variable named \"cluster\"\n";
	}
	if ($pending && ($line =~ /^\s*variable\s/ || $line =~ /^\s*$/)) {
	    $result .= cluster_var_decl(@parents);
	    $pending = 0;
	}
	$result .= $line;
	if ($pending && $line =~ /^\s*parent\s+parent\s+(\S+)\s+(\S+)\s*$/) {
	    push @parents, "$1 $2";
	}
    }
    if ($pending) {
	if ($result !~ /\n\z/) {
	    $result .= "\n";
	}
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
