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
    fail("$usage\n");
  }
} else {
  fail("$usage\n");
}

# Set the DAIKONPARENT variable
#my $date = `date +%Y%m%d-%H%M%S`;
my $date = 'dummy-date';
chomp $date;
my $DAIKONPARENT = cwd() . "/$date";
$ENV{"DAIKONPARENT"} = $DAIKONPARENT;

#my $INV = $DAIKONPARENT . "/invariants";
#$ENV{"INV"} = $INV;
#$ENV{"CLASSPATH"} = "$INV/java:" . $ENV{"CLASSPATH"};

# Set other initial variables
my $CVS_REP = "/g4/projects/invariants/.CVS/";
my $CVS_TAG = "ENGINE_V2_PATCHES";
$ENV{"JAVAC"} = "javac -g";

my %success = ();

# If the --quiet option is specified, output is appended to this
# string, then printed at the end if anything failed.
my $message = "";

mkdir $DAIKONPARENT or fail("can't make directory $DAIKONPARENT: $!\n");
chdir $DAIKONPARENT or fail("can't chdir to $DAIKONPARENT: $!\n");

$success{"daikon_checkout"} = daikon_checkout();

print `env`;

# Inherit the environment of the group-wide init file
if ($success{"daikon_checkout"}) {
  %ENV = get_env("$DAIKONPARENT/invariants/scripts/pag-daikon.bashrc");
}

print `env`;
die;

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

#  $success{"dfej_checkout"} = dfej_checkout();
#  if ($success{"dfej_checkout"}) {
#    $success{"dfej_configure"} = dfej_configure();
#  }
#  if ($success{"dfej_configure"}) {
#    $success{"dfej_complie"} = dfej_compile();
#  }

# print the output files for any steps that failed
my @failed_steps = ();
foreach my $step (sort keys %success) {
  if (!$success{$step}) {
    push @failed_steps, $step;
  }
}
if (@failed_steps != 0) {
  foreach my $step (@failed_steps) {
    if (-e "${step}_summary.out") {
      print_maybe("*** ${step}_summary.out ***\n");
      print_maybe(`cat ${step}_summary.out`);
      print_maybe("\n\n");
    } elsif (-e "$step.out") {
      print_maybe("*** $step.out ***\n");
      print_maybe(`cat $step.out`);
      print_maybe("\n\n");
    } else {
      print_maybe("*** $step ***\n");
      print_maybe("<no output file>");
      print_maybe("\n\n");
    }
  }
  if ($quiet) {
    print $message;
  }
}

# Remove the source checkouts
if (@failed_steps == 0) {
  `rm -rf dfej invariants`;
}



# SUBROUTINES
sub daikon_checkout {
  print_maybe("Checking out Daikon...");
  `cvs -d $CVS_REP co invariants &> daikon_checkout.out`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub dfej_checkout {
  print_maybe("Checking out dfej...");
  `cvs -d $CVS_REP co dfej &> dfej_checkout.out`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub daikon_update {
  print_maybe("Updating Daikon...");
  my $daikon_dir = "invariants/java/daikon";
  chdir $daikon_dir or fail("can't chdir to $daikon_dir: $!\n");
  `cvs -d $CVS_REP up -r $CVS_TAG &> ../../../daikon_update.out`;
  chdir $DAIKONPARENT or fail("can't chdir to $DAIKONPARENT: $!\n");
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub dfej_configure {
  print_maybe("Configuring dfej...");
  chdir "dfej" or fail("Can't chdir to dfej: $!\n");
  `./configure &> ../dfej_configure.out`;
  chdir $DAIKONPARENT or fail("Can't chdir to $DAIKONPARENT: $!\n");
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub daikon_compile {
  print_maybe("Compiling Daikon...");
  `make -C $INV/java all_directly &> daikon_compile.out`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub dfej_compile {
  print_maybe("Compiling dfej...");
  `make -j2 -C dfej/src &> dfej_compile.out`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub daikon_unit_test {
  print_maybe("Daikon unit tests...");
  my $command = "make -C $INV/java/daikon junit " .
    "&> daikon_unit_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  } else {
    print_maybe("OK\n");
    return 1;
  }
}


sub daikon_system_test {
#  my $TEST_SUITE = "do-print_tokens-text-diff";
  my $TEST_SUITE = "text-diff";
#  my $TEST_SUITE = "do-StackAr-text-diff";
  print_maybe("Daikon system tests...");

  my $command = "make -j2 -C $INV/tests/daikon-tests $TEST_SUITE " .
    "&> daikon_system_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  }
  
  $command = "make -C $INV/tests/daikon-tests summary " .
    "2>&1 | tee daikon_system_test_summary.out";
  my $result = `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  }

  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    if (!($line =~ /^0\s/)) {
      print_maybe("FAILED\n");
      return 0;
    }
  }

  print_maybe("OK\n");
  return 1;
}


sub diff_system_test {
  print_maybe("Diff system tests...");

  my $command = "make -j2 -C $INV/tests/diff-tests " .
    "&> diff_system_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  }

  $command = "make -C $INV/tests/diff-tests summary " .
    "2>&1 | tee diff_system_test_summary.out";
  my $result = `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  }

  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    if (!($line =~ /^OK\s/)) {
      print_maybe("FAILED\n");
      return 0;
    }
  }

  print_maybe("OK\n");
  return 1;
}


# Use the version of dfec in invariants/front-end/c.  Could build dfec
# from source instead.
sub dfec_system_test {
  my $TEST_SUITE = "summary-no-space";
  print_maybe("Dfec System Tests...");

  my $command = "make -j2 -C $INV/tests/dfec-tests $TEST_SUITE " .
    "&> dfec_system_test.out";
  `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  }

  $command = "make -C $INV/tests/dfec-tests summary-only " .
    "2>&1 | tee dfec_system_test_summary.out";
  my $result = `$command`;
  if ($CHILD_ERROR) {
    print_maybe("FAILED\n");
    return 0;
  }

  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    if (!($line =~ /^OK\s/)) {
      print_maybe("FAILED\n");
      return 0;
    }
  }

  print_maybe("OK\n");
  return 1;
}


# Unrecoverable failure
sub fail {
  die "@_\n";
}


# If the quiet option was specified, appends its arguments to the
# string $message.  Else, prints its arguments immediately.
sub print_maybe {
  if ($quiet) {
    $message .= @_;
  } else {
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
