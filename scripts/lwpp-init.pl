#!/usr/bin/env perl

# Creates the lackwit databases
# Requires that LACKWIT_HOME is set correctly

use strict;
$^W = 1; #enable warnings

if (@ARGV < 1) {
  die "Usage: lwpp-init.pl <lackwitdb> [filename.c] ...\n";
}

my ($lackwitdb, @files) = @ARGV;

# Check that LACKWIT_HOME is set correctly, and that the required
# files are present and readable
my $lackwit_home = $ENV{LACKWIT_HOME};
-d $lackwit_home or die "LACKWIT_HOME is not set correctly\n";
-r "$lackwit_home/lib/Default.sigs"
  or die "$lackwit_home/lib/Default.sigs does not exist or is not readable\n";
-r "$lackwit_home/lib/libc.c"
  or die "$lackwit_home/lib/libc.c does not exist or is not readable\n";

$ENV{LACKWITDB} = $lackwitdb;
$ENV{PATH} = "$lackwit_home/bin:" . $ENV{PATH};
$ENV{LACKWIT_ARGS} = "-ignore __restrict";

`EmitHeaders $lackwit_home/lib/Default.sigs`;

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

  `lh -\$ -w --gen_c_file_name $int_file $file`;
  my $ret = `gcc -c $int_file -o /dev/null 2>&1`;
  $ret =~ /^(.*)Launching real compiler/s;
  die "Error processing $int_file\n$1\n" if ($1);
}
