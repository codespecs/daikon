#!/usr/bin/env perl

# Run javac, but process its STDERR to give progress indications.

# To use with daikon, put
# JAVAC ?= javac-progress.pl
# in java/Makefile.user

use 5.006;
use strict;
use warnings;

$| = 1;

my $chars = 0;
if (grep($_ eq "--chars", @ARGV)) {
    $chars = 1;
    @ARGV = grep($_ ne "--chars", @ARGV);
}

my $javac = `which javac`;
chomp $javac;

my $width = ($ENV{COLUMNS} || 80);

sub abbrev {
    my($line) = @_;
    if ($line =~ /total/) {
        return $line;
    } elsif ($line =~ /parsing started/) {
        return "p";
    } elsif ($line =~ /parsing completed/) {
        return "\bP";
    } elsif ($line =~ /loading/) {
        if ($line =~ /rt\.jar/) {
            return "l";
        } else {
            return "L";
        }
    } elsif ($line =~ /wrote/) {
        return "w";
    } elsif ($line =~ /checking/) {
        return "c";
    } else {
        return $line;
    }
}

# XXX this quoting isn't quite right
# better to fork and avoid the shell
my $javac_args = join(" ", map("'$_'", @ARGV));

open(JAVAC, "$javac -verbose $javac_args 2>&1 |") or die "$!\n";
while (<JAVAC>) {
    if (/^\[/) {
        if ($chars) {
            print abbrev($_);
        } else {
            chomp;
            my $len = length($_);
            if ($len > $width) {
                $_ = substr($_, 0, $width);
            } else {
                $_ .= " " x ($width - $len);
            }
            print "$_\r";
        }
    } else {
        print;
    }
}
close JAVAC;
