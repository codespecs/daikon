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

my $ret = `gcc -c $lackwit_home/lib/libc.c -o /dev/null 2>&1`;
$ret =~ /^(.*)Launching real compiler/s;
die "Error processing $lackwit_home/lib/libc.c\n$1\n" if ($1);

foreach my $file (@files) {
  `lh $file`;
  my $file_int = $file;
  $file_int =~ s!\.c!.int.c!;
  my $ret = `gcc -c $file_int -o /dev/null 2>&1`;
  $ret =~ /^(.*)Launching real compiler/s;
  die "Error processing $file_int\n$1\n" if ($1);
  unlink $file_int;
}
