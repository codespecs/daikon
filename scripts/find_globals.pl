#!/usr/bin/env perl

## Usage: find_globals.pl < input_decls_file > output_decls_file
##                        [-prefix <global_var_prefix>
##
## Looks for global variables (those marked with the specified prefix that
## appear at every program point) and creates a new global ppt at the
## end that contains those variables.

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
        if ($global_name eq "") {
            ($global_name) = split (/\./, $line);
        }
        if ($debug) {
            print STDERR "found matching ppt: $line\n";
        }
        $exit_cnt++;

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
            if ($type =~ /isParam=true/) {
                next;
            }
            if ($globals{$varname}) {
                $globals{$varname}++;
                my @tinfo = split (/ \^ /, $info{$varname});
                my $match = 0;
                foreach my $ti (@tinfo) {
                    if ($ti eq "$type : $rep : $comp") {
                        $match = 1;
                        last;
                    }
                }
                if (!$match) {
                    $mismatch_cnt {$varname}++;
                    $info{$varname} .= " ^ $type : $rep : $comp";
                }
            } else {
                $keys[++$#keys] = $varname;
                $globals{$varname} = 1;
                $info{$varname} = "$type : $rep : $comp";
            }
        }
    }
}

if ($verbose) {
    print STDERR "$exit_cnt numbered exit routines\n";
}

# write the file back out
foreach $line (@dfile) {
    print "$line\n";
}

# Write out the new global declaration, noting any mismatches
my $global_cnt = 0;
print "\nDECLARE\n";
print $global_name . ":::GLOBAL\n";
foreach my $key (@keys) {
    my $cnt = $globals{$key};
    if ($cnt == $exit_cnt) {
        my $mmcnt = $mismatch_cnt {$key};
        if ($mmcnt) {
            $mmcnt++;
            print STDERR "$mmcnt sets of type information for global $key:\n";
            my @tinfo = split (/ \^ /, $info{$key});
            foreach my $ti (@tinfo) {
                print STDERR "    $ti\n";
            }
        } else {
            if ($key !~ /^$global_prefix/) {
                print STDERR "$key doesn't contain global prefix "
                             . "($global_prefix)\n";
            } else {
                print "$key\n";
                my ($type,$rep,$comp) = split (/ : /, $info{$key});
                print "$type\n$rep\n$comp\n";
                $global_cnt++;
            }
        }
    } else {
        if ($key =~ /$global_prefix/) {
            print STDERR "Global $key only appears in $cnt of $exit_cnt"
                . " numbered exit points\n";
        }
    }
}
if ($verbose) {
    print STDERR "$global_cnt global variables at GLOBAL ppt\n";
}
