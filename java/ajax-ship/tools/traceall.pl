#!perl -w

# This script traces out part of the constraint graph.

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($filename, $start, $mode, $options, @ignorelist) = @ARGV;

if (!defined($mode)) {
    die "Usage: <filename> <start-with> <mode> [<options>] [ignorelist]\n";
}

if (!defined($options)) {
    $options = "";
}

if ($start =~ /^([a-zA-Z0-9_.]+)\.([a-zA-Z0-9_<>]+)/) {
    my($class, $method) = ($1, $2);

    undef $start;
    open(IN, $filename);
    while (<IN>) {
        if (/^([a-f0-9x"]+) \[label="method [a-zA-Z0-9_.]+ ([a-zA-Z0-9_<>]+)[a-zA-Z0-9_.() ,]+ in class ([a-zA-Z0-9_.]*)/) {
            my($addr, $thismethod, $thisclass) = ($1, $2, $3);
            
            if ($thisclass eq $class && $thismethod eq $method) {
                warn "Duplicate definitions for $class.$method, using:\n$_" if defined($start);
                $start = $addr;
            }
        }
    }
} else {
    $start = &canonize($start);
}

warn if !defined($start);

&build_data($filename, $mode);

warn if !&is_var($start);

my(%found) = &traceall($start, $mode, map {&canonize($_);} @ignorelist);
binmode(STDOUT);
&print_output($filename, $options, %found);
