#!/usr/bin/env perl

# colony-runmatches.pl:  runs a set of matches.
# See "usage()" routine for usage information.

sub usage () {
  return
    "$0 input-filename output-filename [parameters]\n"
    . "input filename contains lines consisting of \"team1 team2\"\n"
    . "output format is same as colony-runmatch.pl\n";
}

use strict;
use English;
$WARNING = 1;
use checkargs;
use util_daikon;
use colony_simconf;

if (scalar(@ARGV) < 2) {
  die "$0: needs at least 2 arguments\n" . usage();
}
my $input_filename = shift(@ARGV);
my $output_filename = shift(@ARGV);
my $parameters = join(' ', @ARGV);

open (IN, $input_filename) || die "Cannot read $input_filename";
my $line;
while (defined($line = <IN>)) {
  chomp($line);
  my ($team1, $team2) = split(' ', $line, 2);
  system_or_die("colony-runmatch.pl $team1 $team2 $output_filename $parameters");
}
