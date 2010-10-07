#!/usr/bin/env perl

# Creates a new test. Takes the given name, creates a directory
# if it doesn't exist, and creates empty versions of the goal files.
# also creates a .c file and associated MakeFile. The Makefile 
# will have to be modified for more complicated programs

use warnings;
use strict;

my @files = ("comp", "counts", "decls", "dtrace", "invs", "out");

die "usage: make-test.pl test-name\n" unless $#ARGV >= 0;
my $Makefile = "NAME := $ARGV[0]\n\ninclude ../Makefile.common\n";
my $cvsignore = "$ARGV[0]\n*.diff\n*.counts\n*.out\n*.comp\nran-kvasir\ndaikon-output\n";


mkdir("$ARGV[0]") or die "Unable to create $ARGV[0]: $!\n";


foreach my $file(@files) {
    `touch $ARGV[0]/$ARGV[0].$file.goal`;
}
`touch $ARGV[0]/$ARGV[0].c`;


open(MAKEFILE, ">$ARGV[0]/Makefile");
print MAKEFILE $Makefile;
close(MAKEFILE);

open(CVSIGNORE, ">$ARGV[0]/.cvsignore");
print CVSIGNORE $cvsignore;
close(CVSIGNORE);






