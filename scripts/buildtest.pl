#!/usr/bin/env perl

# Automatically builds and tests the software in the Daikon Distribution

use strict;
use English;
$WARNING = 1;

# my $date = `date +%Y%m%d-%H%M%S`;
my $date = '20020212-164755';

#  print "Making directory $date...";
#  mkdir $date or die "Can't mkdir $date: $!\n";
chdir $date or die "Can't chdir to $date: $!\n";
#  symlink("../Makefile.common", "Makefile") or die "Can't create symlink: $!\n";
#  print "OK\n";

$INV = `pwd` . "/invariants";
$CVS_REP = "/g4/projects/invariants/.CVS/";

#  print "Checking out source...";
#  `make checkout &> checkout.out`;
#  if (! $CHILD_ERROR) {
#    print "OK\n";
#  } else {
#    print "FAILED\n";
#    exit;  
#  }

#  print "Compiling Daikon...";
#  `make compile &> compile.out`;
#  if (! $CHILD_ERROR) {
#    print "OK\n";
#  } else {
#    print "FAILED\n";
#    exit;  
#  }

#  print "Daikon Unit Tests...";
#  `make junit &> junit.out`;
#  if (! $CHILD_ERROR) {
#    print "OK\n";
#  } else {
#    print "FAILED\n";
#    exit;  
#  }

print "Daikon System Tests...";
`make CLASSPATH=$CLASSPATH -C $(INV)/tests/daikon-tests

#  print "Diff System Tests..."

#  print "Compiling Dfej...";

#  print "Dfej System Tests...";

#  print "Compiling Dfec...";

#  print "Dfec System Tests...";
