#!/usr/bin/env perl

# Creates the lackwit databases.  Requires that LACKWIT_HOME is set
# correctly.  Requires that 'lh' is in your path.

use English;
use strict;
$WARNING = 1;

if (@ARGV < 1) {
  die "Usage: lwpp-init.pl <lackwitdb> [filename.c] ...\n";
}

my ($lackwitdb, @files) = @ARGV;

# Check args
-d $lackwitdb or die "$lackwitdb is not a directory\n";

# Check that LACKWIT_HOME is set correctly, and that the required
# files are present and readable
my $lackwit_home = $ENV{LACKWIT_HOME};
-d $lackwit_home or die "LACKWIT_HOME is not set correctly\n";

$ENV{LACKWITDB} = $lackwitdb;
$ENV{PATH} = "$lackwit_home/bin:" . $ENV{PATH};
$ENV{LACKWIT_ARGS} = "-ignore __restrict";

my $emitheaders_output = `~mernst/research/invariants/front-end/c/lackwit/bin/EmitHeaders $lackwit_home/lib/Default.sigs`;
die "EmitHeaders failed:\n$emitheaders_output\n" if ($CHILD_ERROR != 0);

unshift @files, "$lackwit_home/lib/libc.c";

foreach my $file (@files) {
  # An intermediate file is created for each source file to be
  # analyzed.  The intermediate files are created in the $lackwitdb
  # directory.  All files are written to the root of $lackwitdb,
  # regardless of leading directories.  It is not a big problem if
  # filenames clash -- each file is analyzed immediately after it is
  # created.  The files are left around for debugging purposes only.
  my $int_file = $file;
  $int_file =~ s!\.c!.int.c!;
  $int_file =~ s!(.*)/(.*)\.c!$2.c!; # strip leading directories
  $int_file = "$lackwitdb/$int_file";

  my $lh_output;
  if ($file =~ /libc\.c/) {
    my $lhflags = "";
    if (`uname` =~ /Linux/) {
      $lhflags = "-D_LINUX_IN_H -D_LIBIO_H";
    }
    $lh_output = `lh -\$ -w $lhflags --gen_c_file_name $int_file $file`;
  } else {
    $lh_output = `lh -w --gen_c_file_name $int_file $file`;
  }
  die "lh failed processing file $file:\n$lh_output\n" if ($CHILD_ERROR != 0);
  my $gcc_output = `gcc -c $int_file -o /dev/null 2>&1`;
  $gcc_output =~ /^(.*)Launching real compiler/s;
  die "Error processing $int_file\n$1\n" if ($1);
}
