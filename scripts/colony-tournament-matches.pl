#!/usr/bin/env perl

# colony-tournament-matches.pl:  outputs a match file (a list of matches)
# needed to run a tournament among multiple teams.
# See "usage()" routine for usage information.

my $usage = <<END_USAGE;
colony-tournament-matches.pl [--colonydir dir]
  [-r numrepetitions] [previous-result-files ...]
Outputs (to stdout) a "match file" (the input format for colony-runmatches.pl).
Arguments:
  --help
        Print this message.
  --colonydir colonydir
        Directory in which "conf/sim.conf" file exists.
  -r numrepetitions
        How many rounds the tournament should have (how many times each
        team plays each other team).  If omitted and previous-result-files
        are specified, then numrepetitions is taken to be the maximum
        number of matches that were already played between any two teams;
        this is useful when completing a partial tournament.  If omitted
        and no previous-result-files are provided, defaults to 1.
  previous-result-files
        Result files from previous (possibly partial) tournaments.
        These are in the output format of colony-runmatch[es].pl.

Example use:

  # Create a list of all matches in a complete round-robin tournament
  colony-tournament-matches.pl > matches-one-tournament

  # Create a list of all matches required to balance any matches in the
  # tournament-results-* files into a round-robin tournament (possibly with
  # multiple matches per pair of teams).
  colony-tournament-matches.pl tournament-results-* > matches-remaining-noargs

END_USAGE

sub usage () {
  return $usage;
}

use strict;
use English;
$WARNING = 1;
use checkargs;
use util_daikon;
use colony_simconf;

# If $numrepetitions is not defined on the command line, it will be
# defined later.
my $numrepetitions;

my $colony_dir = "/g6/users/leelin/research/6.370/colony-2003";
# my $colony_dir = "/g2/users/mernst/tmp/steering-experiments/lees-2003/colony";

# Parse command line arguments.
while ((scalar(@ARGV) > 0) && ($ARGV[0] =~ /^-/))  {
  if ($ARGV[0] eq "--help") {
    print usage();
    exit;
  } elsif ($ARGV[0] eq "--colonydir") {
    shift @ARGV;
    $colony_dir = shift @ARGV;
  } elsif ($ARGV[0] eq "-r") {
    shift @ARGV;
    $numrepetitions = shift @ARGV;
  } else {
    print usage();
    die "unrecognized argument $ARGV[0]";
  }
}


read_sim_conf("$colony_dir/conf/sim.conf");
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
