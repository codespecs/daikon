#!/usr/bin/env perl

# colony-runmatches.pl:  runs a set of matches.
# See "usage()" routine for usage information.

my $usage = <<END_USAGE;
colony-runmatches.pl [OPTIONS] match-file result-file [parameters]
Runs a set of matches specified by match-file, leaving the results
  in result-file.  See \"colony-README\" file for file formats.
The [parameters] are passed through to colony-runmatch.pl.
Options:
  --help
        Print this message.
  --debug
  	Produce debugging output.
  --distribute[=machine1,machine2,...]
        Distribute the matches among multiple computers; each computer
        runs a subset of the matches.
  --duplicate[=machine1,machine2,...]
        Duplicate matches among multiple computers; each computer runs
        all the matches, resulting in a tournament with multiple rounds.
  --loadlimit=N
        Don't run on any machine whose load is greater than the given value.
	If this would filter out all machines, run on all machines anyway.
  --firstline=N
  --lastline=N
	Only run a subset of the matches:  those between firstline and
        lastline, inclusive.
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
use POSIX;                      # for ceil
require Cwd;		# make Cwd:: accessible

my $debug = "";
my $distribute = 0;
my $duplicate = 0;
my $loadlimit = 0;
my $firstline = 0;
my $lastline = 0;
# Take care that no one else needs particular machines (say, for timing
# purposes) before starting compute-intensive jobs on them.
my @machines = ('parsnip', 'peanut',
                'beet', 'daikon', 'manioc', 'rutabaga', 'turnip',
                'yam', 'scallion',
                'shallot',
                # These are so slow it's hardly worthwhile to include them
                # 'potato',
                # 'meoptiplex'
                );
my $current_dir;


# using cleanup is bad because this script will not be able to
# run multiple times on the same machine. The user should manually
# delete the tmp files or the cleanup should be done intentially by a
# commandline arg.
#cleanup();

# Parse command line arguments.
while ((scalar(@ARGV) > 0) && ($ARGV[0] =~ /^-/)) {
  my $arg = shift @ARGV;
  if ($arg eq "--help") {
    print usage();
    exit;
  } elsif ($arg eq "--debug") {
    $debug = $arg;
  } elsif ($arg =~ /^--distribute(=(.*))?$/) {
    $distribute = 1;
    if (defined($2)) {
      @machines = split(',', $1);
    }
  } elsif ($arg =~ /^--duplicate(=(.*))?$/) {
    $duplicate = 1;
    if (defined($2)) {
      @machines = split(',', $1);
    }
  } elsif ($arg =~ "--loadlimit=(.*)") {
    $loadlimit = $1;
  } elsif ($arg =~ "--firstline=(.*)") {
    $firstline = $1;
  } elsif ($arg =~ "--lastline=(.*)") {
    $lastline = $1;
  } else {
    die "unrecognized argument $arg";
  }
}

if (0 && $debug) {
  print "args:\n";
  print " distribute  $distribute\n";
  print " duplicate  $duplicate\n";
  print " loadlimit  $loadlimit\n";
  print " firstline  $firstline\n";
  print " lastline  $lastline\n";
}

if (scalar(@ARGV) < 2) {
  die "$0: not enough arguments; needs match-file and result-file\n" . usage();
}
my $input_filename = shift(@ARGV);
my $output_filename = shift(@ARGV);
my $parameters = join(' ', @ARGV);


###########################################################################
### Subroutines
###

#Clean up old temp files
#No other instances of the script may be running for this to work properly
sub cleanup () {
    my $tmpdir = "/tmp/$ENV{'USER'}";
    opendir (DIR, $tmpdir);
    my @files = readdir DIR;
    close DIR;
    # look through the directory for files matching the pattern
    # runmatch-[digits]
    foreach my $file (@files) {
	if ($file =~ /runmatch.\d+/) {
	    system_or_die("rm -rf $tmpdir/$file");
	}
    }
}


# Filter out elements of @machines whose load is greater than $loadlimit.
# If this would filter out all machines, run on all machines anyway.
sub obey_loadlimit () {
  check_args(0);
  if ($loadlimit == 0) {
    return;
  }
  die "--loadlimit argument not yet implemented";
}

# From the Perl FAQ.
sub file_lines ( $ ) {
  my ($filename) = check_args(1, @_);
  my $lines = 0;
  my $buffer;
  open(FILE, $filename) or die "Can't open `$filename': $!";
  while (sysread FILE, $buffer, 4096) {
    $lines += ($buffer =~ tr/\n//);
  }
  close FILE;
  return $lines;
}

sub absolutify_filenames () {
  check_args(0);
  $current_dir = Cwd::getcwd();
  $input_filename = absolutify_filename($input_filename);
  $output_filename = absolutify_filename($output_filename);
}

sub absolutify_filename ( $ ) {
  my ($filename) = check_args(1, @_);
  if ($filename =~ /^\//) {
    return $filename;
  }
  return "$current_dir/$filename";
}


###########################################################################
### Main processing
###

### The --distribute and --duplicate options run multiple jobs on multiple
### processors, by recursively invoking this same script.

if ($duplicate) {
  obey_loadlimit();
  absolutify_filenames();
  if (scalar(@machines) == 0) {
    die "No machines specified";
  }
  for my $host (@machines) {
    my $command = "ssh -f $host.lcs.mit.edu nice $0 $debug $input_filename $output_filename-$host $parameters";
    # print "$command\n";
    system_or_die($command);
  }
  exit(0);
}

if ($distribute) {
  obey_loadlimit();
  absolutify_filenames();
  my $num_matches = file_lines($input_filename);
  my $num_machines = scalar(@machines);
  my $num_matches_per_machine = ceil($num_matches/$num_machines);
  if ($num_machines == 0) {
    die "No machines specified";
  }
  for (my $hostnum=0; $hostnum<$num_machines; $hostnum++) {
    my $this_firstline = $hostnum*$num_matches_per_machine;
    my $this_lastline = $this_firstline + $num_matches_per_machine - 1;
    my $host = $machines[$hostnum];
    my $command = "ssh -f $host.lcs.mit.edu nice $0 $debug --firstline=$this_firstline --lastline=$this_lastline $input_filename $output_filename-$host $parameters";
    # print "$command\n";
    system_or_die($command);
  }
  exit(0);
}


### BASE CASE
### Simply run the job locally.
open (IN, $input_filename) || die "Cannot read $input_filename";
my $line;
my $lineno = 0;
while (defined($line = <IN>)) {
  $lineno++;
  if (($firstline && ($lineno < $firstline))
      || ($lastline && ($lineno > $lastline))) {
    next;
  }
  chomp($line);
  my ($team1, $team2) = split(' ', $line, 2);
  my $command = "colony-runmatch.pl $team1 $team2 $output_filename $parameters";
  if ($debug) {
    # system_or_die will print the command if it fails.
    # print "\ncommand: $command\n";
    system_or_die($command);
  } else {
    # Not system_or_die; it's OK if it dies.  We'll notice the missing run
    # (by hand?) and come back to it later.

    #It is way too dangerous to let allow this script to continue if the 
    #call to colony-runmatch.pl fails.  A failing call will leave the files
    #on tmp and will do this for all matches in the tournament.  That amounts
    #to some 400 matches on the tmp directory which causes many problems.
    #Using the scratch or user drives may make the problem less severe but
    #really has the same problem.  The best solution for now is to die no
    #matter what.
    system_or_die("colony-runmatch.pl $team1 $team2 $output_filename $parameters");
  }
}
close(IN);

exit(0);
