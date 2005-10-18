#!/usr/bin/env perl
use strict;
use English;
$WARNING = 1;

my $status = 0;

my $glob = $ENV{"inv"} . "/tests/daikon-tests/*/*-{daikon,esc,jml}.goal";
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
    $status = 1;
  }
}

exit $status;
