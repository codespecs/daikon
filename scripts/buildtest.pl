#!/usr/bin/env perl

# Automatically builds and tests the software in the Daikon
# Distribution.  If the --quiet option is selected, only generates
# output if a task fails (useful for cron job).

use strict;
use English;
$WARNING = 1;
use Cwd;

# Process the command-line args
my $usage = "Usage: buildtest.pl [--quiet]\n";
my $quiet = 0;
if (@ARGV == 0) {
  $quiet = 0;
} elsif (@ARGV == 1) {
  if ($ARGV[0] eq "--quiet") {
    $quiet = 1;
  } else {
    die "$usage\n";
  }
} else {
  die "$usage\n";
}

# Set the DAIKONPARENT variable
my $date = `date +%Y%m%d-%H%M%S`;
#my $date = 'dummy-date';
chomp $date;
my $DAIKONPARENT = cwd() . "/$date";
$ENV{"DAIKONPARENT"} = $DAIKONPARENT;

# Set other initial variables
my $CVS_REP = "/g4/projects/invariants/.CVS/";
my $CVS_TAG = "ENGINE_V2_PATCHES";
$ENV{"JAVAC"} = "javac -g";

my %success = ();

mkdir($DAIKONPARENT) or die "can't make directory $DAIKONPARENT: $!\n";
chdir($DAIKONPARENT) or die "can't chdir to $DAIKONPARENT: $!\n";

my $LOG = "buildtest.out";
open LOG, ">>$LOG" or die "can't open buildtest.out: $!\n";

$success{"daikon_checkout"} = daikon_checkout();

# Inherit the environment of the group-wide init file
if ($success{"daikon_checkout"}) {
  %ENV = get_env("$DAIKONPARENT/invariants/scripts/pag-daikon.bashrc");
}

my $INV = $ENV{"INV"};

if ($success{"daikon_checkout"}) {
  $success{"daikon_update"} = daikon_update();
}
if ($success{"daikon_update"}) {
  $success{"daikon_compile"} = daikon_compile();
}

if ($success{"daikon_compile"}) {
  $success{"daikon_unit_test"} = daikon_unit_test();
  $success{"daikon_system_test"} = daikon_system_test();
  $success{"diff_system_test"} = diff_system_test();
}

if ($success{"daikon_checkout"}) {
  $success{"dfec_system_test"} = dfec_system_test();
}

$success{"dfej_checkout"} = dfej_checkout();
if ($success{"dfej_checkout"}) {
  $success{"dfej_configure"} = dfej_configure();
}
if ($success{"dfej_configure"}) {
  $success{"dfej_complie"} = dfej_compile();
}

# print the output files for any steps that failed
my @failed_steps = ();
foreach my $step (sort keys %success) {
  if (!$success{$step}) {
    push @failed_steps, $step;
  }
}
if (@failed_steps != 0) {
  print_log("\n\n");
  foreach my $step (@failed_steps) {
    if (-e "${step}_summary.out") {
      print_log("*** ${step}_summary.out ***\n");
      print_log(`cat ${step}_summary.out`);
      print_log("\n\n");
    } elsif (-e "$step.out") {
      print_log("*** $step.out ***\n");
      print_log(`cat $step.out`);
      print_log("\n\n");
    } elsif (-e "${step}_clean.out") {
      print_log("*** ${step}_clean.out ***\n");
      print_log(`cat ${step}_clean.out`);
      print_log("\n\n");
    } else {
      print_log("*** $step ***\n");
      print_log("<no output file>");
      print_log("\n\n");
    }
  }
  if ($quiet) {
    open READLOG, $LOG or die "can't open $LOG: $!\n";
    my $log = join('', <READLOG>);
    print $log;
  }
}


# Move the .diff files to another directory, then remove the source
# checkouts
mkdir("diffs") or die "can't make directory diffs: $!\n";

foreach my $subdir ("daikon", "diff", "dfec") {
  mkdir("diffs/$subdir") or die "can't make directory diffs/$subdir: $!\n";
  my $diffs = `find invariants/tests/$subdir-tests -name "*.diff"`;
  foreach my $file (split '\n',$diffs) {
    `mv $file diffs/$subdir`;
    die "can't move diffs to diffs/$subdir\n" if ($CHILD_ERROR);
  }
}

`rm -rf dfej invariants`;



# SUBROUTINES
sub daikon_checkout {
  print_log("Checking out Daikon...");
  `cvs -d $CVS_REP co invariants &> daikon_checkout.out`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub dfej_checkout {
  print_log("Checking out dfej...");
  `cvs -d $CVS_REP co dfej &> dfej_checkout.out`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub daikon_update {
  print_log("Updating Daikon...");
  my $daikon_dir = "invariants/java/daikon";
  chdir($daikon_dir) or die "can't chdir to $daikon_dir: $!\n";
  `cvs -d $CVS_REP up -r $CVS_TAG &> ../../../daikon_update.out`;
  chdir($DAIKONPARENT) or die "can't chdir to $DAIKONPARENT: $!\n";
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub dfej_configure {
  print_log("Configuring dfej...");
  chdir("dfej") or die "Can't chdir to dfej: $!\n";
  `./configure &> ../dfej_configure.out`;
  chdir($DAIKONPARENT) or die "Can't chdir to $DAIKONPARENT: $!\n";
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub daikon_compile {
  print_log("Compiling Daikon...");
  `make -C $INV/java all_directly &> daikon_compile.out`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub dfej_compile {
  print_log("Compiling dfej...");
  `make -j2 -C dfej/src &> dfej_compile.out`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub daikon_unit_test {
  print_log("Daikon unit tests...");
  my $command = "make -C $INV/java/daikon junit " .
    "&> daikon_unit_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


sub daikon_system_test {
  # Standard test suite
  my $TEST_SUITE = "text-diff";
  # Short test suites
#  my $TEST_SUITE = "do-print_tokens-text-diff";
#  my $TEST_SUITE = "do-StackAr-text-diff";
  print_log("Daikon system tests...");

  my $command = "make -C $INV/tests/daikon-tests clean " .
    "&> daikon_system_test_clean.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  $command = "make -j2 -C $INV/tests/daikon-tests $TEST_SUITE " .
    "&> daikon_system_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  $command = "make -C $INV/tests/daikon-tests summary " .
    "2>&1 | tee daikon_system_test_summary.out";
  my $result = `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    if (!($line =~ /^0\s/)) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}


sub diff_system_test {
  print_log("Diff system tests...");

  my $command = "make -j2 -C $INV/tests/diff-tests " .
    "&> diff_system_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  $command = "make -C $INV/tests/diff-tests summary " .
    "2>&1 | tee diff_system_test_summary.out";
  my $result = `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    if (!($line =~ /^OK\s/)) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}


# Use the version of dfec in invariants/front-end/c.  Could build dfec
# from source instead.
sub dfec_system_test {
  # Standard test suite
  my $TEST_SUITE = "summary-no-space";
  # Short test suites
  # my $TEST_SUITE = "print_tokens";
  print_log("Dfec System Tests...");

  my $command = "make -j2 -C $INV/tests/dfec-tests $TEST_SUITE " .
    "&> dfec_system_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  $command = "make -C $INV/tests/dfec-tests summary-only " .
    "2>&1 | tee dfec_system_test_summary.out";
  my $result = `$command`;
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  }

  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    if (!($line =~ /^OK\s/)) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}


# Appends its arguments to the log file.  If the quiet option was no
# specified, also prints its arguments to STDOUT.
sub print_log {
  print LOG @_;
  if (! $quiet) {
    print @_;
  }
}


# Source the file specified as an argument, and return the resulting
# environment in a hash
sub get_env {
  my ($file) = @_;
  my %newenv = ();
  my $newenv = `source $file; env`;
  foreach my $line (split '\n', $newenv) {
    my ($var, $val) = split '=', $line;
    $newenv{$var} = $val;
  }
  return %newenv;
}
