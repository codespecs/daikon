#!/usr/bin/env perl
use strict;
use English;
$WARNING = 1;

# Verify that the txt-daikon files have the same number of lines as the
# txt-daikon.goal files.  This is used for testing guarding.

my $status = 0;

# Check only txt-daikon, not txt-esc or txt-jml, as the latter default to
# some guarding and existence of any previous guarding can throw off the
# check.

my $glob = $ENV{"inv"} . "/tests/daikon-tests/*/*-daikon.goal";
# print "glob: $glob\n";
my @goals = glob($glob);
# print "numgoals = " . scalar(@goals) . "\n";
for my $goal (@goals) {
  # print "goal: $goal\n";
  my $goallines = `wc -l < $goal`;
  chomp($goallines);
  my $actual = $goal;
  $actual =~ s/\.goal$//;
  if (! -e $actual) {
    print "file does not exist: $actual\n";
    $status = 1;
    next;
  }
  my $actuallines = `wc -l < $actual`;
  chomp($actuallines);
  if ($goallines ne $actuallines) {
    print "$goallines lines in $goal\n$actuallines lines in $actual\n";
    print "  (ediff-files \"$goal\" \"$actual\")\n";
    $status = 1;
  }
}

exit $status;
