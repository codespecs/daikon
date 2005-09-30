#!/usr/bin/env perl

# Run javac, but process its STDERR to give progress indications.

# To use with daikon, put
# JAVAC = javac-progress.pl --javac="javac-xlint -p '\./jtb/' javac"
# JAVAC_XLINT =
# in java/Makefile.user

use 5.006;
use strict;
use warnings;

$| = 1;

my $javac = "javac";
my $chars = 0;

my @remain;
for my $arg (@ARGV) {
    if ($arg eq "--chars") {
        $chars = 1;
    } elsif ($arg =~ /^--javac=(.*)$/s) {
        $javac = $1;
    } else {
        push @remain, $arg;
    }
}
@ARGV = @remain;

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
    } elsif ($line =~ /search path/) {
        return "S";
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
        s/total (\d+)(\d\d\d)ms/total $1.$2sec/;
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
