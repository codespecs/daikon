#!/usr/bin/env perl

# Creates the lackwit databases.  Requires that LACKWIT_HOME is set
# correctly.  Requires that 'lh' is in your path (it appears
# in directory front-end/c/).

use English;
use strict;
$WARNING = 1;

if (@ARGV < 1) {
  die "Usage: lwpp-init.pl <lackwitdb> [lhargs...] [filenames...]\n";
}

my $lackwitdb = shift @ARGV;
# Check args
-d $lackwitdb or die "$lackwitdb is not a directory\n";
my $lhargs =
    "-D__builtin_next_arg\\(arg\\)='(((char*)arg)+1)' ";
my @files = ();

my $doingargs=1;

ARGPARSE: while (defined (my $arg = shift @ARGV)) {
    $arg =~ s/([\'\"\$])/\\$1/g;
    if (($doingargs)&&($arg =~ /^-/)) {
	$lhargs .= "${arg} ";
    } else {
	$doingargs=0;
	if ($arg =~ /^-/) {
	    #is -l, skip it
	    next ARGPARSE;
	} else {
	    if ($arg =~ /\.h$/) {
		#is hfile, skip it
		next ARGPARSE;
	    }
	}
	#is cfile
	push @files, $arg;
    }
}

# Check that LACKWIT_HOME is set correctly, and that the required
# files are present and readable
my $lackwit_home = $ENV{LACKWIT_HOME};
-e "$lackwit_home/bin/Lackwit"
  or die "Environment variable LACKWIT_HOME is not set correctly\n";

$ENV{LACKWITDB} = $lackwitdb;
$ENV{PATH} = "$lackwit_home/bin:" . $ENV{PATH};
$ENV{LACKWIT_ARGS} = "-ignore __restrict";

my $emitheaders_output = `EmitHeaders $lackwit_home/lib/Default.sigs`;
die "EmitHeaders failed:\n$emitheaders_output\n" if ($CHILD_ERROR != 0);

my $libc = "$lackwit_home/lib/libc.c";
unshift @files, $libc;

foreach my $file (@files) {
  # An intermediate file is created for each source file to be
  # analyzed.  The intermediate files are created in the $lackwitdb
  # directory.  All files are written to the root of $lackwitdb,
  # regardless of leading directories.  It is not a big problem if
  # filenames clash -- each file is analyzed immediately after it is
  # created.  The files are left around for debugging purposes only.
  my $int_file = $file;
  $int_file =~ s!\.c!.int.c!;
  $int_file =~ s!^(.*)/!!; # strip leading directories
  $int_file = "$lackwitdb/$int_file";

  my $lh_output;
  if ($file eq $libc) {
    my $lhflags = "";
    if (`uname` =~ /Linux/) {
      $lhflags = "-D_LINUX_IN_H -D_LIBIO_H";
    }
    $lh_output = `lh -\$ -w $lhflags --gen_c_file_name $int_file $file`;
  } else {
    $lh_output = `lh -w $lhargs --gen_c_file_name $int_file $file`;
  }
  die "lh failed processing file $file:\n$lh_output\n" if ($CHILD_ERROR != 0);
  my $gcc_output = `gcc -c $int_file -o /dev/null 2>&1`;
  $gcc_output =~ /^(.*)Launching real compiler/s;
  die "Error processing $int_file\n$1\n" if ($1);
}
