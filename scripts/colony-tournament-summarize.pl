#!/usr/bin/env perl

# colony-tournament-summarize.pl:  Display statistics for completed tournament.
# See "usage()" routine for usage information.

my $usage = <<END_USAGE;
colony-tournament-summarize.pl [--colonydir dir] [match-result-files ...]
Display staticsts for a completed tournament.
Arguments:
  --help
        Print this message.
  --colonydir colonydir
        Directory in which "conf/sim.conf" file exists.
  match-result-files
        Result files from the tournament,
        in the output format of colony-runmatch.pl.
The output lists the teams twice, once by team number and once by rank.
The "dominates" number indicates how many other teams a given team dominates.
  Team A dominates team B if team A won more of the A-B matches; matches
  against other opponents are irrelevant to whether A dominates B.
  See the comments in the script for more detail, and to understand the "time"
  measure.
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
  } else {
    print usage();
    die "unrecognized argument $ARGV[0]";
  }
}


read_sim_conf("$colony_dir/conf/sim.conf");
my @teams = packages();         # package names
my $num_teams = scalar(@teams);
my %teams = ();                 # maps from team names (packages) to numbers
for (my $i=0; $i<$num_teams; $i++) { $teams{$teams[$i]} = $i; }

# the "array_*" variables are 2-dimensional arrays; each index is a team
# number.  Call the two indices $i and $j.
my @array_matches = ();     # number of matches between team $i and team $j
my @array_wins = ();        # number of wins for team $i over team $j
my @array_time = ();        # total time for the matches where team $i beat team $j
# parameters to the simulator; should be the same for all matches
my $global_parameters;

for (my $i1=0; $i1<$num_teams; $i1++) {
  for (my $i2=$i1+1; $i2<$num_teams; $i2++) {
    $array_matches[$i1][$i2] = 0;
    $array_wins[$i1][$i2] = 0;
    $array_wins[$i2][$i1] = 0;
    $array_time[$i1][$i2] = 0;
    $array_time[$i2][$i1] = 0;
  }
}

while (@ARGV) {
  my $file = shift @ARGV;
  open(IN, $file) or die "cannot read file $file";
  my $line;
  while (defined($line = <IN>)) {
    my ($team1, $team2, $winner, $time, $parameters) = split("\t", $line);
    my $i1 = $teams{$team1};
    my $i2 = $teams{$team2};
    if (! defined($i1)) { die "$team1 does not appear in sim.conf"; }
    if (! defined($i2)) { die "$team2 does not appear in sim.conf"; }
    if (($team1 cmp $team2) > 0) { die "Teams should be alphabetized: $line"; }
    if (($i1 > $i2) > 0) { die "Teams should be alphabetized: $line"; }

    $array_matches[$i1][$i2]++;
    if ($winner eq $team1) {
      $array_wins[$i1][$i2]++;
      $array_time[$i1][$i2] += $time;
    } elsif ($winner eq $team2) {
      $array_wins[$i2][$i1]++;
      $array_time[$i2][$i1] += $time;
    } elsif ($winner eq "NoWinner") {
      $array_wins[$i1][$i2] += .5;
      $array_wins[$i2][$i1] += .5;
      $array_time[$i1][$i2] += ($time/2.0);
      $array_time[$i2][$i1] += ($time/2.0);
    } else {
      die "Unrecognized winner $winner should be $team1 or $team2 or NoWinner";
    }
    # my $name1 = $teams[$i1];    # for debugging
    # my $name2 = $teams[$i2];    # for debugging

    if (! defined($global_parameters)) {
      $global_parameters = $parameters;
    }
    if ($parameters ne $global_parameters) {
      die "inconsistent parameters: \"$global_parameters\" \"$parameters\"";
    }
  }
}

# $team_wins[$i] is the total number of matches that team $i won.
my @team_wins = ();
# $team_losses[$i] is the total number of matches that team $i lost.
my @team_losses = ();
# $team_dominates[$i] is the total number of other teams that team $i
# dominates; larger numbers are better.  Team $i dominates team $j iff team
# $i won more of the ($i,$j) matches.  Thus, if the two teams won equal
# numbers of matches against one another, then neither team dominates the
# other.  Domination has only to do with matches between the two teams,
# and nothing to do with how the two teams did against other opponents.
my @team_dominates = ();
# $team_time[$i] is the times for the matches lost by team $i, minus the
# time for the matches won by team $i.  Given a particular number of wins
# and losses, larger numbers are better.
my @team_time = ();
# $team_rank[$i] is the rank of team $i.
my @team_rank = ();
# $team_rank[$rank] is the number of the team whose rank is $rank.
my @team_rank_inverted = ();

for (my $i1=0; $i1<$num_teams; $i1++) {
  $team_wins[$i1] = 0;
  $team_losses[$i1] = 0;
  $team_dominates[$i1] = 0;
  $team_time[$i1] = 0;
  $team_rank_inverted[$i1] = $i1;
}
for (my $i1=0; $i1<$num_teams; $i1++) {
  for (my $i2=$i1+1; $i2<$num_teams; $i2++) {
    my $wins1 = $array_wins[$i1][$i2];
    my $wins2 = $array_wins[$i2][$i1];
    $team_wins[$i1] += $wins1;
    $team_wins[$i2] += $wins2;
    $team_losses[$i1] += $wins2;
    $team_losses[$i2] += $wins1;
    # my $name1 = $teams[$i1];    # for debugging
    # my $name2 = $teams[$i2];    # for debugging
    if ($wins1 > $wins2) {
      $team_dominates[$i1]++;
    } elsif ($wins2 > $wins1) {
      $team_dominates[$i2]++;
    } else {
    }

    # A larger number is good, so add the times for the losses and subtract
    # the times for the wins.
    $team_time[$i1] += $array_time[$i2][$i1] - $array_time[$i1][$i2];
    $team_time[$i2] += $array_time[$i1][$i2] - $array_time[$i2][$i1];

  }
}

# Note that we never check whether team $a dominates team $b; that is
# irrelevant (and there can be cycles in that graph).  We only check the
# total number of other teams that a team dominates.
# The wins and time should really be divided by the total number of matches
# for the team.
sub team_rank_cmp {
  my $result = (
                ($team_dominates[$a] <=> $team_dominates[$b])
                || ($team_wins[$a] <=> $team_wins[$b])
                || ($team_time[$a] <=> $team_time[$b])
                );
  ## Debugging
  ## my $better_team;
  ## if ($result < 0) { $better_team = $teams[$b]; }
  ## if ($result > 0) { $better_team = $teams[$a]; }
  ## if ($result == 0) { $better_team = "tie"; }
  ## print "comparing $teams[$a] to $teams[$b], result = $better_team ($result)\n";
  return $result;
}
@team_rank_inverted = sort team_rank_cmp @team_rank_inverted;
for (my $i1=0; $i1<$num_teams; $i1++) {
  $team_rank[$team_rank_inverted[$i1]] = $num_teams - $i1;
}


## print "team rank inverted ", join(' ', @team_rank_inverted), "\n";
## print "team rank          ", join(' ', @team_rank), "\n";
## print "\n\n";

sub print_team ( $ ) {
  my ($i) = check_args(1, @_);
  print "$teams[$i]\n";
  print "  rank: ", $team_rank[$i], "/$num_teams\n";
  print "  dominates: $team_dominates[$i]\n";
  print "  wins: $team_wins[$i]\n";
  print "  losses: $team_losses[$i]\n";
  print "  time: $team_time[$i]\n";
}

print "BY TEAM NUMBER:\n\n";
for (my $i1=0; $i1<$num_teams; $i1++) {
  print_team($i1);
}

print "\n\nBY RANK:\n\n";
for (my $i1=$num_teams-1; $i1>=0; $i1--) {
  print_team($team_rank_inverted[$i1]);
}
