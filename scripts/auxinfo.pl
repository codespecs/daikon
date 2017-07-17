#!/usr/bin/env perl

## Usage: auxinfo [<ppt>]-<var> <auxinfo>

## Adds the specified aux information to the specified variable.
## If a program point is specified, only the specified variable
## at that program point is modified.  Any number of var/auxinfo pairs
## can be specified.

use English;
use strict;
$WARNING = 1;                   # "-w" flag
my $debug = 0;
my @vars;
my @auxs;
my $line;
my @dfile;

#process arguments
while (@ARGV) {
    push @vars, shift @ARGV;
    push @auxs, shift @ARGV;
}

#read the whole file (its usually not too long) into an array
while ($line = <STDIN>) {
    push @dfile,  ($line);
    chomp ($dfile[$#dfile]);
}

#process each var/aux pair
while (@vars) {
    my $var = shift @vars;
    my $aux = shift @auxs;
    my $ppt;
    if ($var =~ "-") {
        ($ppt, $var) = split (/-/, $var);
    } else {
        $ppt = ".";
    }
    if ($debug) {
        print STDERR "processing $ppt - $var ($aux)\n";
    }

    #loop through the decls looking for matches
    for (my $i = 0; $i <= $#dfile; $i++) {
        $line = $dfile[$i];
        if ($line eq "DECLARE") {
            $line = $dfile[++$i];
            if (!($line =~ /$ppt/)) {
                next;
            }
            if ($debug) {
                print STDERR "found matching ppt: $line\n";
            }
            $line = $dfile[++$i];
            while ($line && $line ne "\n") {
                my $varname = $line;;
                my $type = $dfile[++$i];
                my $rep = $dfile[++$i];
                my $comp = $dfile[++$i];
                $line = $dfile[++$i];
                    if ($debug) {
                        print STDERR "looking $varname | $type | $rep | $comp\n";
                    }
                if ($varname eq $var) {
                    if ($debug) {
                        print STDERR "FOUND $varname | $type | $rep | $comp\n";
                    }
                    if ($type =~ /#/) {
                        $type .= ", $aux";
                    } else {
                        $type .= " # $aux";
                    }
                    if ($debug) {
                        print STDERR "New type = $type\n";
                    }
                    $dfile[$i-3] = $type
                }
            }

        }
    }
}
foreach $line (@dfile) {
    print "$line\n";
}
