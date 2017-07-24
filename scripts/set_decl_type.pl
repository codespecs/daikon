#!/usr/bin/env perl

## Usage: set_decl_type [<ppt>]-<var> <type>

## Sets the type of the specified specified variable.
## If a program point is specified, only the specified variable
## at that program point is modified.  Any number of var/type pairs
## can be specified.

use English;
use strict;
$WARNING = 1;                   # "-w" flag
my $debug = 0;
my $verbose = 1;
my @vars;
my @new_types;
my $line;
my @dfile;

#process arguments
while (@ARGV) {
    push @vars, shift @ARGV;
    push @new_types, shift @ARGV;
}

#read the whole file (its usually not too long) into an array
while ($line = <STDIN>) {
    push @dfile,  ($line);
    chomp ($dfile[$#dfile]);
}

#process each var/aux pair
while (@vars) {
    my $var = shift @vars;
    my $new_type = shift @new_types;
    my $ppt;
    if ($var =~ "-") {
        ($ppt, $var) = split (/-/, $var);
    } else {
        $ppt = ".";
    }
    if ($debug || $verbose) {
        print STDERR "processing $ppt - $var ($new_type)\n";
    }

    #loop through the decls looking for matches
    for (my $i = 0; $i <= $#dfile; $i++) {
        $line = $dfile[$i];
        if ($line eq "DECLARE") {
            $line = $dfile[++$i];
            if (!($line =~ /$ppt/) && ($line ne $ppt)) {
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
                #if ($debug) {
                #    print STDERR "looking $varname | $type | $rep | $comp\n";
                #}
                if ($varname eq $var) {
                    if ($debug) {
                        print STDERR "FOUND $varname | $type | $rep | $comp\n";
                    }
                    my $type_only;
                    my $aux;
                    if ($type =~ / /) {
                        ($type_only, $aux) = split ('  *', $type, 2);
                    } else {
                        $type_only = $type;
                        $aux = "";
                    }

                    $type = "$new_type $aux";
                    if ($debug) {
                        print STDERR "New type = $type\n";
                    }
                    $dfile[$i-3] = $type;
                    $dfile[$i-2] = $new_type;
                }
            }

        }
    }
}
foreach $line (@dfile) {
    print "$line\n";
}
