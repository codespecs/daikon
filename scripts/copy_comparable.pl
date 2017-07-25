#!/usr/bin/env perl

## Usage: copy_comparable.pl < input_decls_file > output_decls_file
##                           -template <ppt> [-prefix <global prefix>]
##
## Copies global comparability information from one ppt to others.
## Used when the comparability information is incorrect at some points.
## Locals are set to -1 (comparable to everything).
##

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
my $globals_only = 0;
my $out_file = 1;
my $template = "";

#process arguments
while (@ARGV) {
    my $arg = shift @ARGV;
    if ($arg eq "-prefix") {
        $global_prefix = shift @ARGV;
    } elsif ($arg eq "-template") {
        $template = shift @ARGV;
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

#loop through the decls looking for the specified template ppt
for (my $i = 0; $i <= $#dfile; $i++) {
    $line = $dfile[$i];
    if ($line eq "DECLARE") {
        $line = $dfile[++$i];
        if (!($line =~ /EXIT[0-9]/)) {
            next;
        }
        my $ppt = $line;
        if ($line =~ /$template/) {
            print STDERR "found template ppt: $ppt\n";
        } else {
            next;
        }

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
            if ($varname !~ /^$global_prefix/) {
                next;
            }

            $globals{$varname} = "$type : $rep : $comp";
        }
    }
}

# Loop through the file again and replace the comparablity info with
# that from the specified point
#loop through the decls looking for the specified template ppt
for (my $i = 0; $i <= $#dfile; $i++) {
    $line = $dfile[$i];
    if ($line eq "DECLARE") {
        $line = $dfile[++$i];
        #if (!($line =~ /EXIT[0-9]/)) {
        #    next;
        #}
        my $ppt = $line;
        if ($line =~ /$template/) {
            next;
        }

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
            if ($globals{$varname}) {
                ($type, $rep, $comp) = split (/ : /, $globals{$varname});
                $dfile[$i-1] = $comp;
            } elsif ($varname =~ /^$global_prefix/) {
                print STDERR "$varname not found in template in $ppt\n";
            } else { # must be local, make comparable to all
                $dfile[$i-1] = "-1";
            }
        }
    }
}

# write the file back out
foreach $line (@dfile) {
    print "$line\n";
}
