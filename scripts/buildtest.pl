#!/usr/bin/env perl -w

# Automatically builds and tests the software in the Daikon Distribution

use strict;
use English;
$WARNING = 1;
use Cwd;

#my $date = `date +%Y%m%d-%H%M%S`;
my $date = '20020215-173521';
chomp $date;
my $base_dir = cwd() . "/$date";

$ENV{"PATH"} = "/g2/jdk/bin:" . $ENV{"PATH"};

#make_directory();

chdir $base_dir or fail("Can't chdir to $base_dir: $!\n");

my $CVS_REP = "/g4/projects/invariants/.CVS/foo";
my $CVS_TAG = "ENGINE_V2_PATCHES";

#daikon_checkout();
#daikon_update();

my $INV = cwd() . "/invariants";
my $DAIKON_LIBS = join(":",glob("$INV/java/lib/*.jar"));
my $CLASSPATH = "$INV/java:$INV/java/ajax-ship:$DAIKON_LIBS";
my $JAVAC = "javac -g";

#daikon_compile();
#daikon_unit_test();
#daikon_system_test();
#diff_system_test();

#dfec_system_test();

#dfej_checkout();
#dfej_configure();
#dfej_compile();




# SUBROUTINES
sub make_directory {
  print "Making directory $date...";
  mkdir $date or fail("Can't mkdir $date: $!\n");
  print "OK\n";
}

sub daikon_checkout {
  print "Checking out Daikon...";
  `cvs -d $CVS_REP co invariants &> daikon-checkout.out`;
  check_error();
}

sub dfej_checkout {
  print "Checking out dfej...";
  `cvs -d $CVS_REP co dfej &> dfej-checkout.out`;
  check_error();
}

sub daikon_update {
  print "Updating Daikon...";
  chdir "invariants/java/daikon"
    or fail("Can't chdir to invariants/java/daikon: $!\n");
  `cvs -d $CVS_REP up -r $CVS_TAG &> ../../../daikon-update.out`;
  chdir "../../../" or fail("Can't chdir to ../../..: $!\n");
  check_error();
}

sub dfej_configure {
  print "Configuring dfej...";
  chdir "dfej" or fail("Can't chdir to dfej: $!\n");
  `./configure &> ../dfej-configure.out`;
  chdir ".." or fail("Can't chdir to ..: $!\n");
  check_error();
}

sub daikon_compile {
  print "Compiling Daikon...";
  `make CLASSPATH=$CLASSPATH JAVAC="$JAVAC" -C $INV/java/daikon all_directly &> daikon-compile.out`;
  check_error();
}

sub dfej_compile {
  print "Compiling dfej...";
  `make -j2 -C dfej/src &> dfej-compile.out`;
  check_error();
}

sub daikon_unit_test {
  print "Daikon Unit Tests...";
  `make CLASSPATH=$CLASSPATH -C $INV/java/daikon junit &> daikon-unit-test.out`;
  check_error();
}

sub daikon_system_test {
  my $TEST_SUITE = "do-print_tokens-text-diff";
  print "Daikon System Tests...";
  `make -j2 INV=$INV CLASSPATH=$CLASSPATH -C $INV/tests/daikon-tests $TEST_SUITE &> daikon-system-test.out`;
  fail("FAILED") if $CHILD_ERROR;
  my $result = `make -C $INV/tests/daikon-tests summary 2>&1 | tee daikon-system-test-summary.out`;
  fail("FAILED") if $CHILD_ERROR;
  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    fail("FAILED") if (!($line =~ /^0\s/));
  }
  print "OK\n";
}

sub diff_system_test {
  print "Diff System Tests...";
  chdir "$INV/tests/diff-tests"
    or fail("Can't chdir to $INV/tests/diff-tests: $!\n");
  `make -j2 CLASSPATH=$CLASSPATH &> diff-system-test.out`;
  fail("FAILED") if $CHILD_ERROR;
  my $result = `make summary 2>&1 | tee diff-system-test-summary.out`;
  fail("FAILED") if $CHILD_ERROR;
  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    fail("FAILED") if (!($line =~ /^OK\s/));
  }
  chdir "../../.." or fail("Can't chdir to ../../..: $!\n");
  print "OK\n";
}

# Use the version of dfec in invariants/front-end/c.  Could build dfec
# from source instead.
sub dfec_system_test {
  my $TEST_SUITE = "print_tokens";
  print "dfec System Tests...";
  `make -j2 INV=$INV -C $INV/tests/dfec-tests $TEST_SUITE &> dfec-system-test.out`;
  fail("FAILED") if $CHILD_ERROR;
  my $result = `make INV=$INV -C $INV/tests/dfec-tests summary-only 2>&1 | tee dfec-system-test-summary.out`;
  fail("FAILED") if $CHILD_ERROR;
  foreach my $line (split /\n/,$result) {
    next if ($line =~ /^make/);
    fail("FAILED") if (!($line =~ /^OK\s/));
  }
  print "OK\n";
}

sub check_error {
  fail("FAILED") if $CHILD_ERROR;
  print "OK\n";
}

sub fail {
  my ($msg) = @_;
  die "$msg\n";
}
