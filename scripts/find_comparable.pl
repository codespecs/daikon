#!/usr/bin/env perl

## Usage: find_comparable.pl < input_decl_file

## Looks for comparable variables in each ppt and prints them to separate
## files named with the program point.  Best executed in empty directory
##
## Only ppts that have at least some variables that are comparable to
## one another will be printed.

use English;
use strict;
$WARNING = 1;                   # "-w" flag
my $debug = 0;
my $verbose = 1;
my @vars;
my @new_types;
my $line;
my @dfile;
my $global_prefix = "::";
my $globals_only = 1;
my $out_file = 1;

#process arguments
while (@ARGV) {
    my $arg = shift @ARGV;
    if ($arg eq "-prefix") {
        $global_prefix = shift @ARGV;
    } else {
        print "Unknown argument $arg";
        exit;
    }
}

#read the whole file (its usually not too long) into an array
while ($line = <STDIN>) {
    push @dfile,  ($line);
    chomp ($dfile[$#dfile]);
}

# indexed by varname, value is number of occurrences
my %globals;
my %info;
my %mismatch_cnt;
my $exit_cnt = 0;
my @keys;
my $global_name = "";

#loop through the decls looking for numbered exit points
for (my $i = 0; $i <= $#dfile; $i++) {
    $line = $dfile[$i];
    if ($line eq "DECLARE") {
        $line = $dfile[++$i];
        if (!($line =~ /EXIT[0-9]/)) {
            next;
        }
        my $ppt = $line;
        if ($debug) {
            print STDERR "found matching ppt: $ppt\n";
        }
        $exit_cnt++;

        # map from comparability to variables
        my %compar;

        # get the next variable
        $line = $dfile[++$i];
        while ($line && $line ne "\n") {
            my $varname = $line;;
            my $type = $dfile[++$i];
            my $rep = $dfile[++$i];
            my $comp = $dfile[++$i];
            $line = $dfile[++$i];
            #if ($debug) {
            #    print STDERR "looking $varname | $type | $rep | $comp\n";
            #}
            if ($globals_only && ($varname !~ /^$global_prefix/)) {
                next;
            }

            if ($compar{$comp}) {
                $compar{$comp} .= " $varname($rep)";
            } else {
                $compar{$comp} = "$varname($rep)";
            }
        }

        # print out any comparable variables
        if ($out_file) {
            my $open = 0;
            foreach my $key (sort (keys (%compar))) {
                my @vars = split (/ /, $compar{$key});
                if ($#vars > 1) {
                    if (!$open) {
                        my $ppt_file = $ppt;
                        $ppt_file =~ s/\(.*:::/_/;
                        open (fp, "|sort > $ppt_file");
                        $open = 1;
                    }
                    print fp "    ";
                    foreach my $var (@vars) {
                        print fp "$var ";
                    }
                    print fp "\n";
                }
            }
            if ($open) {
                foreach my $key (sort (keys (%compar))) {
                    my @vars = split (/ /, $compar{$key});
                    if ($#vars == 1) {
                        print fp "    $vars[0]\n";
                    }
                }
                close fp;
            }
        } else {
            print "\n$ppt\n";
            foreach my $key (keys (%compar)) {
                my @vars = split (/ /, $compar{$key});
                if ($#vars > 1) {
                    print "    ";
                    foreach my $var (@vars) {
                        print "$var ";
                    }
                    print "\n";
                }
            }
        }
    }
}
