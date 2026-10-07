#!/usr/bin/env perl

# Takes as input a trace file.
# Produces as output a file with each entry indented according to its call depth.
# Strips all but the first line of each entry.
# (In the future, add an option to include the whole entry and/or to
# indicate line numbers in the original file.)

use strict;
use 5.006;
use warnings;

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);
# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

my $indentation = 0;

$/ = ""; # Read by paragraph
while (<>) {
    # Daikon does not require a blank line after a comment.
    (undef, $_) = split_leading_comments($_);
    # Skip .decls-like paras
    next if $_ eq "" or record_kind($_) ne "data";
    /^(.*):::([A-Z\d]+)$/m or die "Can't parse PPT name from <$_>";
    my $base = $1;
    my $suffix = $2;
    my $name = $base . ":::" . $suffix;
    if ($suffix !~ /^EXIT|^ENTER$/) {
	die "What is this line? <suffix> <$_>";
    }
    if ($suffix =~ /^EXIT/) {
	$indentation--;
    }
    my $line = (' ' x $indentation) . $name . "\n";
    print $line;
    if ($suffix eq "ENTER") {
	$indentation++;
    }
}
