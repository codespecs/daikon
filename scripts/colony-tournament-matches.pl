#!/usr/bin/env perl

# colony-tournament-matches.pl:  runs a tournament among multiple teams.
# See "usage()" routine for usage information.

sub usage () {
  return
    "$0 [-r numrepetitions] [previous-match-files ...]\n"
    . "output format (to stdout) is same as input format for colony-runmatches.pl\n"
    . "previous-match-files are in output format of colony-runmatch.pl\n";
}

my $lees_2003_dir = "/g6/users/leelin/research/6.370/colony-2003";
# my $lees_2003_dir = "/g2/users/mernst/tmp/steering-experiments/lees-2003";



use strict;
use English;
$WARNING = 1;
use checkargs;
use util_daikon;
use colony_simconf;

my $numrepetitions;
if ((scalar(@ARGV) > 0) && ($ARGV[0] eq "-r"))  {
  shift @ARGV;
  $numrepetitions = shift @ARGV;
} elsif (scalar(@ARGV) == 0) {
  $numrepetitions = 1;
}


read_sim_conf("$lees_2003_dir/conf/sim.conf");
my @teams = packages();
my $num_teams = scalar(@teams);
my %teams = ();
for (my $i=0; $i<$num_teams; $i++) { $teams{$teams[$i]} = $i; }

# Number of previous matches between two teams.
my @prev_matches = ();
for (my $i1=0; $i1<$num_teams; $i1++) {
  for (my $i2=$i1+1; $i2<$num_teams; $i2++) {
    $prev_matches[$i1][$i2] = 0;
  }
}

while (@ARGV) {
  my $file = shift @ARGV;
  open(IN, $file) or die "cannot read file $file";
  my $line;
  while (defined($line = <IN>)) {
    my ($team1, $team2, $rest) = split("\t", $line, 3);
    if (0) { print $rest; } # avoid warning about unused variable
    my $i1 = $teams{$team1};
    my $i2 = $teams{$team2};
    if (! defined($i1)) { die "$team1 does not appear in sim.conf"; }
    if (! defined($i2)) { die "$team2 does not appear in sim.conf"; }
    if (($team1 cmp $team2) > 0) { die "Teams should be alphabetized: $line"; }
    if (($i1 > $i2) > 0) { die "Teams should be alphabetized: $line"; }
    $prev_matches[$i1][$i2]++;
  }
}

# If the number of repetitions was not provided, make it equal to the
# maximum number of matches that were played between any two teams.
if (! defined($numrepetitions)) {
  $numrepetitions = 1;
  for (my $i1=0; $i1<$num_teams; $i1++) {
    for (my $i2=$i1+1; $i2<$num_teams; $i2++) {
      my $nummatches = $prev_matches[$i1][$i2];
      if ($nummatches > $numrepetitions) {
        $numrepetitions = $nummatches;
      }
    }
  }
}

for (my $i1=0; $i1<$num_teams; $i1++) {
  my $team1 = $teams[$i1];
  for (my $i2=$i1+1; $i2<$num_teams; $i2++) {
    my $team2 = $teams[$i2];
    for (my $count = $prev_matches[$i1][$i2]; $count<$numrepetitions; $count++) {
      print "$team1 $team2\n";
    }
  }
}
