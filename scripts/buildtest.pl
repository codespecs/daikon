#!/usr/bin/env perl

# Builds and tests the software in the Daikon CVS repository.  If the
# --quiet option is selected, only generates output if a task fails
# (useful for cron job).  If the --nocleanup option is selected, does
# not remove generated files (good for debugging).

# This script ordinarily runs overnight; to invoke it by hand, execute the
# following commands as user daikonbuildtest (or as any other user):
#   cd $HOME/build
#   /usr/bin/env perl $HOME/research/invariants/scripts/buildtest.pl --nocleanup
# Make sure that . is either not in your path or at the end.  Otherwise,
# some of the kvasir perl tests will fail trying to run the test version of
# perl rather than the system version.

use strict;
use English;
$WARNING = 1;
use Cwd;
use File::Copy;

# Process the command-line args
my $usage =
    "Usage: buildtest.pl [--quiet] [--message=text]\n"
  . "                    [--rsync_location=machine:/path/invariants]\n"
  . "  Debugging flags:  [--nocleanup] [--skip_daikon] [--skip_daikon_build]\n"
  . "                    [--skip_build_dyncomp] [--reuse_dyncomp_jar=jarfile]\n"
  . "                    [--skip_kvasir] [--skip_cross_checker]\n"
  . "                    [--cvs_co_args=ARGS]\n";
my $quiet = 0;
my $nocleanup = 0;
# When set, print an additional message in the header of failing runs
my $message;
## These flags permit only part of the tests to be run; good for debugging.
# When on, skip compiling daikon.  Implies skip_daikon.
my $skip_daikon_build = 0;
# When on, skip Daikon unit tests, Daikon system tests, and diff system tests
my $skip_daikon = 0;
# When on, skip building the dyncomp version of rt.jar (dcomp_rt.jar).
# When on, also implies skip_daikon!
my $skip_build_dyncomp = 0;
# When on, reuse an existing dyncomp_rt.jar file.  (Building it is very slow.)
my $reuse_dyncomp_jar;
# When on, test Kvasir
my $test_kvasir = 1;
# When on run daikon simple as a cross checker -- note: takes 3+ hours
my $test_cross_checker = 1;
# If set, supply the arguments to the "cvs co" command.  Example:
# --cvs_co_args='-D date'
my $cvs_co_args = "";
# When set, get the sources by rsync from the given location, rather
# than by CVS.
my $rsync_location;

while (scalar(@ARGV) > 0) {
  my $arg = shift @ARGV;
  if ($arg eq "--quiet") {
    $quiet = 1;
  } elsif ($arg eq "--nocleanup") {
    $nocleanup = 1;
  } elsif ($arg eq "--skip_daikon_build") {
    $skip_daikon = $skip_daikon_build = 1;
  } elsif ($arg eq "--skip_daikon") {
    $skip_daikon = 1;
  } elsif ($arg eq "--skip_build_dyncomp") {
    $skip_daikon = $skip_build_dyncomp = 1;
  } elsif ($arg eq "--test_kvasir") {
    $test_kvasir = 1;
  } elsif ($arg eq "--skip_kvasir") {
    $test_kvasir = 0;
  } elsif ($arg eq "--skip_cross_checker") {
    $test_cross_checker = 0;
  } elsif ($arg =~ /^--cvs_co_args=(.*)$/) {
    $cvs_co_args = $1;
  } elsif ($arg =~ /^--rsync_location=(.*)$/) {
    $rsync_location = $1;
  } elsif ($arg =~ /^--message=(.*)$/s) {
    $message = $1;
  } elsif ($arg =~ /^--reuse_dyncomp_jar=(.*)$/s) {
    $reuse_dyncomp_jar = $1;
    $skip_build_dyncomp = 1;
  } else {
    die "Unrecognized argument $arg\n$usage\n";
  }
}

# Set the DAIKONPARENT variable
my $date = `date +%Y%m%d-%H%M%S`;
#my $date = 'dummy-date';
chomp $date;
my $DAIKONPARENT = cwd() . "/$date";
$ENV{"DAIKONPARENT"} = $DAIKONPARENT;

# Set other initial variables
my $CVS_REP = "/afs/csail.mit.edu/group/pag/projects/invariants/.CVS";
$ENV{"JAVAC"} = "javac -g";

# Whether or not to run Make in two-job mode
# my $J2 = "-j2";
my $J2 = "";

# The success of each step in the build/test process.  Non-zero means success.
my %success = ();

mkdir($DAIKONPARENT, 0777) or die "can't make directory $DAIKONPARENT: $!\n";
chdir($DAIKONPARENT) or die "can't chdir to $DAIKONPARENT: $!\n";

my $LOG = "buildtest.out";

print_log("$message\n") if defined $message;

$success{"daikon_checkout"} = daikon_checkout();

if (! $success{"daikon_checkout"}) {
  goto PRINT_FAILURES;
}

# Inherit the environment of the developer init file
%ENV = get_env("$DAIKONPARENT/daikon/scripts/daikon-dev.bashrc");

my $INV = $ENV{"INV"};
print_log("INV = $INV\n");
# my $CLASSPATH = $ENV{"CLASSPATH"};
# print_log("CLASSPATH = $CLASSPATH\n");
# my $PATH = $ENV{"PATH"};
# print_log("PATH = $PATH\n");

if (! $skip_daikon_build) {
  if ($success{"daikon_checkout"}) {
    $success{"daikon_update"} = daikon_update();
    $success{"tests_update"} = tests_update();
  }
  if ($success{"daikon_update"}) {
    $success{"daikon_compile"} = daikon_compile();
  }
}

if (! $skip_build_dyncomp) {
  if ($success{"daikon_compile"}) {
    $success{"build_dyncomp_jar"} = build_dyncomp_jar();
  }
}

if ($reuse_dyncomp_jar) {
  copy($reuse_dyncomp_jar, "$INV/java/dcomp_rt.jar") or die "File cannot be copied:\n  $reuse_dyncomp_jar\n  $INV/java/dcomp_rt.jar";
}


if (! $skip_daikon) {
  if ($success{"daikon_compile"}
      && $success{"tests_update"}) {
    $success{"daikon_unit_test"} = daikon_unit_test();
    if ($success{"daikon_unit_test"}) {
        $success{"daikon_system_test"} = daikon_system_test();
    }
  }
}

if ($test_kvasir and $success{"daikon_checkout"}) {
  $success{"kvasir_checkout"} = kvasir_checkout();
  if ($success{"kvasir_checkout"}) {
    $success{"kvasir_compile"} = kvasir_compile();
  }
  if ($success{"kvasir_compile"}) {
    $success{"kvasir_regression_test"} = kvasir_regression_test();
  }
  if ($success{"kvasir_compile"} and $success{"daikon_compile"}) {
    $success{"kvasir_daikon_test"} = kvasir_daikon_test();
  }
}

if ($test_cross_checker and $success{"daikon_system_test"}) {
    $success{"daikon_cross_checker"} = daikon_cross_checker();
}

PRINT_FAILURES:

# Print the output files for any steps that failed.  Output steps are
# sorted alphabetically, not in order of operation.  This is OK, since
# failures should occur rarely, and multiple failures even more
# rarely.
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

  # Handle error output and the quiet flag.
  #  * If quiet is not set, this output was printed earlier.
  #  * If quiet is set, the output was instead written to a log file (see
  #    function print_log).  If a step failed, we must print the log file now.
  # To summarize, if a step fails, the same output is printed whether or
  # not quiet is set.  However, if all steps succeed, there is no
  # output iff quiet is set.
  if ($quiet) {
    open LOG, $LOG or die "failed_steps = @failed_steps but can't open " . cwd() . "/$LOG: $!\n";
    print <LOG>;
    close LOG;
  }
}


# Move the .diff files to another directory, then remove the source
# checkouts
mkdir("diffs", 0777) or die "can't make directory diffs: $!\n";

foreach my $subdir ("daikon", "diff", "kvasir") {
  mkdir("diffs/$subdir", 0777) or die "can't make directory diffs/$subdir: $!\n";
  my $diffs = `find daikon/tests/$subdir-tests -name "*.diff"`;
  foreach my $file (split '\n',$diffs) {
    `cp -p $file diffs/$subdir`;
    die "can't copy diff file $file to diffs/$subdir\n" if ($CHILD_ERROR);
  }
}

if ((@failed_steps == 0) && (! $nocleanup)) {
  `rm -rf invariants`;
}

exit();


# SUBROUTINES

# Check the invariants module out from CVS
sub daikon_checkout {
  print_log("Checking out Daikon...");
  my $cmd;
  if ($rsync_location) {
      $cmd = "rsync -e 'ssh -x' -rav $rsync_location . ";
  } else {
      $cmd = "cvs -d $CVS_REP co $cvs_co_args invariants ";
  }
  my $cvs_success = buildtest_cmd ($cmd, "daikon_checkout.out");
  if (! $cvs_success) {
    return $cvs_success;
  }
  # Success
  return 1;
}


# Update the daikon directory to the ENGINE_V2_PATCHES tag
sub daikon_update {
  print_log("Updating Daikon...");
  my $daikon_dir = "daikon/java/daikon";
  chdir($daikon_dir) or die "can't chdir to $daikon_dir: $!\n";
  `cvs -d $CVS_REP up &> ../../../daikon_update.out`;
  chdir($DAIKONPARENT) or die "can't chdir to $DAIKONPARENT: $!\n";
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


# Update the tests directory to the ENGINE_V2_PATCHES tag
sub tests_update {
  print_log("Updating tests...");
  my $tests_dir = "daikon/tests";
  chdir($tests_dir) or die "can't chdir to $tests_dir: $!\n";
  `cvs -d $CVS_REP up &> ../../tests_update.out`;
  chdir($DAIKONPARENT) or die "can't chdir to $DAIKONPARENT: $!\n";
  if ($CHILD_ERROR) {
    print_log("FAILED\n");
    return 0;
  } else {
    print_log("OK\n");
    return 1;
  }
}


# Compile daikon using javac
sub daikon_compile {
  print_log("Compiling Daikon...");
  return buildtest_cmd ("make -C $INV/java clean all_directly ../daikon.jar",
                        "daikon_compile.out");
}

# Build the dyncomp rt.jar
sub build_dyncomp_jar {
  print_log("Building dcomp_rt.jar...");
  return buildtest_cmd ("make -C $INV/java dcomp_rt.jar",
                        "build_dyncomp_jar.out");
}

# Run the daikon JUnit unit tests
sub daikon_unit_test {
  print_log("Daikon unit tests...");
  return buildtest_cmd ("make -C $INV/java/daikon junit-all ",
                        "daikon_unit_test.out");
}


# Run the daikon system tests.  Scan the output for any nonzero
# ".diff" filesizes.
sub daikon_system_test {
  print_log("Daikon system tests...");

  my $succ = buildtest_cmd ("make -C $INV/tests/ clean",
                            "daikon_system_test_clean.out", "", "FAILED\n");
  if (!$succ) { return $succ;}

  my $log = "daikon_system_test.out";

  # Switch the two lines below if using a different RUN_JAVA variable
  # $command = "make RUN_JAVA=$RUN_JAVA $J2 -C $INV/tests/daikon-tests " .
  $succ = buildtest_cmd ("make $J2 -C $INV/tests/ diffs",
                         $log, "", "FAILED\n");
  if (!$succ) { return $succ; }

  # JML tests fail because JML doesn't support 1.5.  We could probably
  # fix this by creating a 1.4 version of Quant.
  #$succ = buildtest_cmd ("make -C $INV/tests/daikon-tests jml ",
  #                       $log, "", "FAILED\n");
  #if (!$succ) { return $succ; }

  $succ = buildtest_cmd ("make -C $INV/tests/daikon-tests inv-checker ",
                         $log, "", "FAILED\n");
  if (!$succ) { return $succ; }

  my $summary_file = "daikon_system_test_summary.out";
  $succ = buildtest_cmd ("make -C $INV/tests/ summary ",
                         $summary_file);
  if (!$succ) { return $succ; }

  open (SUM, $summary_file) or die "can't open $summary_file\n";
  while (my $line = <SUM>) {
    next if ($line =~ /^make/);
    next if ($line =~ /^All tests succeeded.$/);
    if (!($line =~ /^0\s/)) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}


# Run the diff system tests.  Scan the output for any "FAILED" tests.
sub diff_system_test {
  print_log("Diff system tests...");

  my $succ = buildtest_cmd ("make $J2 -C $INV/tests/diff-tests ",
                         "diff_system_test.out", "", "FAILED\n");
  if (!$succ) { return $succ; }

  my $summary_file = "diff_system_test_summary.out";
  $succ = buildtest_cmd ("make -C $INV/tests/diff-tests summary ",
                         $summary_file, "", "FAILED\n");
  if (!$succ) { return $succ; }

  open (SUM, $summary_file) or die "can't open $summary_file\n";
  while (my $line = <SUM>) {
    next if ($line =~ /^make/);
    if (!($line =~ /^OK\s/)) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}

# run the cross checker using daikon simple
sub daikon_cross_checker() {
    print_log ("Daikon cross checker...");

    my $succ = buildtest_cmd ("make -C $INV/tests cross-checker-good",
                              "daikon_cross_checker.out");
    return ($succ);
}




sub kvasir_checkout {
  print_log("Checking out Kvasir...");
  my $log = "$DAIKONPARENT/kvasir_checkout.out";
  chdir($INV) or die "can't chdir to $INV: $!\n";
  `cvs -d $CVS_REP co valgrind-3 2>&1 | tee $log`;
  symlink("valgrind-3/valgrind", "kvasir");
  chdir($DAIKONPARENT) or die "Can't chdir to $DAIKONPARENT: $!\n";
  `chmod -R a+r $INV/valgrind-3 2>&1 | tee -a $log`;
  if (-e "$INV/kvasir/fjalar/Makefile.am") {
    print_log("OK\n");
  } else {
    print_log("FAILED\n");
  }
}

sub kvasir_compile {
  print_log("Compiling Kvasir...");
  my $log = "$DAIKONPARENT/kvasir_compile.out";
  # chdir("$INV/kvasir") or die "can't chdir to $INV/kvasir: $!\n";

  # my $succ = buildtest_cmd ("cd $INV/kvasir && " .
  #                           "./configure --prefix=`pwd`/inst",
  #                           $log, "", "FAILED\n");
  my $succ = buildtest_cmd ("cd $INV/valgrind-3 && " .
                            "./auto-everything.sh",
                            $log, "", "FAILED\n");
  if (!$succ) { return 0; }

  $succ = buildtest_cmd ("cd $INV/kvasir && make", $log, "", "FAILED\n");
  if (!$succ) { return 0; }

  $succ = buildtest_cmd ("cd $INV/kvasir && make install", $log);
  return $succ;
}

sub kvasir_regression_test {
  # Standard test suite
  my $TEST_SUITE = "nightly-summary";
  print_log("Kvasir regression tests...");

  my $succ = buildtest_cmd ("make -C $INV/tests/kvasir-tests $TEST_SUITE ",
                            "kvasir_regression_test.out", "", "FAILED\n");
  if (!$succ) { return $succ; }

  my $summary_file = "kvasir_regression_test_summary.out";
  $succ = buildtest_cmd ("make -C $INV/tests/kvasir-tests $TEST_SUITE-only ",
                         $summary_file, "", "FAILED\n");
  if (!$succ) { return 0; }

  open (SUM, $summary_file) or die "can't open $summary_file\n";
  while (my $line = <SUM>) {
    next if ($line =~ /^make/);
    if ($line =~ /^FAILED\s/) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}

sub kvasir_daikon_test {
  # Standard test suite
  my $TEST_SUITE = "nightly-summary";
  print_log("Kvasir Daikon tests...");

  my $succ = buildtest_cmd ("make -C $INV/tests/kvasir-tests "
            . "$TEST_SUITE-w-daikon", "kvasir_daikon_test.out", "", "FAILED\n");
  if (!$succ) { return $succ; }

  my $summary_file = "kvasir_daikon_test_summary.out";
  $succ = buildtest_cmd ("make -C $INV/tests/kvasir-tests "
               . "$TEST_SUITE-only-w-daikon ", $summary_file, "", "FAILED\n");
  if (!$succ) { return $succ; }

  open (SUM, $summary_file) or die "can't open $summary_file\n";
  while (my $line = <SUM>) {
    next if ($line =~ /^make/);
    if ($line =~ /^FAILED\s/) {
      print_log("FAILED\n");
      return 0;
    }
  }

  print_log("OK\n");
  return 1;
}

# Appends its arguments to the log file.  If the quiet option was *not*
# specified, also prints its arguments to STDOUT.
sub print_log {
  open LOG, ">>$LOG" or die "can't open " . cwd() . "/$LOG for append: $!\n";
  print LOG @_;
  if (! $quiet) {
    print @_;
  }
  close LOG;
}

# Executes the command in the first argument and APPENDS its results into
# the file in the second argument.  By default:
#  * If the command succeeds, prints 'OK' to the log and returns 1.
#  * If the command fails, prints 'FAILED' to the log and returns 0.
# (Note that the return value is the inverse of the Unix/C exit status.)
# Optional 3rd and 4th arguments can override the strings 'OK' and 'FAILED'.
# If the command includes a pipe, the result depends only on the last item.
sub buildtest_cmd {
    my ($cmd, $file, $pass, $fail) = @_;
    if (!defined ($pass)) {
        $pass = "OK\n";
    }
    if (!defined ($fail)) {
        $fail = "FAILED\n";
    }
    my $full_cmd = "$cmd >>$file 2>&1";
    # print "Executing '$full_cmd'\n";
    my $err = system ($full_cmd);
    if ($err) {
        print_log($fail);
        return 0;
    } else {
        print_log("$pass");
        return 1;
    }
}

# Source the file specified as an argument, and return the resulting
# environment in a hash
sub get_env {
  my ($file) = @_;
  my %newenv = ();
  my $newenv = `source $file; env`;
  if ($CHILD_ERROR) {
    print("FAILED: source $file; env\n");
    print_log("FAILED: source $file; env\n");
    return 0;
  }
  foreach my $line (split '\n', $newenv) {
    my ($var, $val) = split '=', $line;
    $newenv{$var} = $val;
  }
  return %newenv;
}
