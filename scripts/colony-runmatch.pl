#!/usr/bin/env perl

# colony-runmatch.pl:  runs a match between two teams.
# See "usage()" routine for usage information.

sub usage () {
  return
    "$0 team1-package team2-package output-filename [parameters]\n"
    . "output: tab-delimited line containing: team1, team2, winner, time, parameters\n";
}

use strict;
use English;
$WARNING = 1;
use checkargs;
use util_daikon;
use colony_simconf;

 my $lees_2003_dir = "/g6/users/leelin/research/6.370/colony-2003";
#my $lees_2003_dir = "/g2/users/mernst/tmp/steering-experiments/lees-2003";

if (scalar(@ARGV) < 3) {
  die "Too few arguments (" . scalar(@ARGV) . "), need at least 3: " . join(' ', @ARGV) . "\n" . usage();
}

my $team1 = shift @ARGV;
my $team2 = shift @ARGV;
my $outputfile = shift @ARGV;
my $java_args = join(' ', @ARGV);

my $tmpdir = "/tmp/$ENV{'USER'}";
if (! -d $tmpdir) {
  mkdir $tmpdir or die "Cannot create directory $tmpdir";
}
# relative to $lees_2003_dir
my @javacp_list = ("sources", "teams", ".");
# absolute
my $javacp = join(':', map { "$lees_2003_dir/$_" } @javacp_list);


# Example command:
#   cd ~/tmp/steering-experiments/lees-2003/colony
#   java -cp sources:../common/classes:teams:. colony.simulator.SimClient GUI -1 testout.txt

my $java_command = "java -cp $javacp colony.simulator.SimClient $java_args";

my $this_dir = setup_directory();
my $command = "cd $this_dir; $java_command > $this_dir/java-output.txt 2> java_err.txt";
system_or_die($command);
# Sample last line of output:
# [Mon Sep 15 22:39:57 EDT 2003] Winning Team: truth and justice Elapsed Time: 1874 cycles
my $lastline = `tail -n 1 $this_dir/java-output.txt`;
if ($lastline !~ /^\[.*\] Winning Team: (.*) Elapsed Time: ([0-9]+) cycles\n\z/) {
  die "Couldn't parse output of command $command";
}
my ($winner_name, $time) = ($1, $2);

open(OUTFILE, ">>$outputfile") || die "Cannot append to $outputfile";
my $winner_package = name_to_package($winner_name);
# Alphabetize the two teams
if (($team1 cmp $team2) > 0) {
  ($team1, $team2) = ($team2, $team1);
}
print OUTFILE join("\t", $team1, $team2, $winner_package, $time, $java_args), "\n";
close(OUTFILE);

# Last in order to preserve the directory if there is any problem.
system_or_die("rm -rf $this_dir");

exit(0);


###########################################################################
### Subroutines
###

sub setup_directory {
  my $dir = "$tmpdir/runmatch-$PROCESS_ID";
  mkdir $dir;

  # Must copy whole conf directory because I need to modify sim.conf.
  system_or_die("cp -pR $lees_2003_dir/conf $dir");
  rename("$dir/conf/sim.conf", "$dir/conf/sim.conf-orig")
    || die "Cannot rename $dir/conf/sim.conf to $dir/conf/sim.conf-orig";
  read_sim_conf("$dir/conf/sim.conf-orig");
  write_sim_conf("$dir/conf/sim.conf");

  # Must copy whole maps directory (cannot just link) because the program
  # tries to write maps/randmap.map.
  system_or_die("cp -pR $lees_2003_dir/maps $dir");

  # This directory must exist so that file logs/game-log-null can be created.
  mkdir "$dir/logs";

  ## These don't seem necessary.
  # system_or_die("cp -pR $lees_2003_dir/colony/graphics $dir");
  # system_or_die("cp -pR $lees_2003_dir/colony/images $dir");
  # system_or_die("cp -pR $lees_2003_dir/colony/jars $dir");
  # system_or_die("cp -pR $lees_2003_dir/colony/models $dir");

  return $dir;
}

# Write a new simp.conf file containing only the two lines containing team1
# and team2 (which are package names).  Any comment characters are removed.
sub write_sim_conf ( $ ) {
  my ($newfile) = check_args(1, @_);

  my $line1 = package_to_line($team1);
  if (! defined($line1)) {
    die "Didn't find team with package \"$team1\"";
  }
  my $line2 = package_to_line($team2);
  if (! defined($line2)) {
    die "Didn't find team with package \"$team2\"";
  }

  open(OUT, ">$newfile") or die "Cannot write file $newfile";
  print OUT $line1;
  print OUT $line2;
  close(OUT);
}
