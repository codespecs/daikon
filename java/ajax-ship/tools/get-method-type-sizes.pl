#!perl -w

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($filename) = @ARGV;

if (!defined($filename)) {
    die "Usage: <filename>\n";
}

&build_data($filename, "cl");

open(IN, $filename);
while (<IN>) {
    if (/^([a-f0-9x"]+) \[label="method( static)? [][a-zA-Z0-9_.]+ ([a-zA-Z0-9_<>]+)[a-zA-Z0-9_.() ,]+ in class ([a-zA-Z0-9_.]*)/) {
        my($addr, $thismethod, $thisclass) = ($1, $3, $4);
        my(%found) = &traceall($addr, "c");
        print scalar(keys(%found)), " for $thisclass.$thismethod\n";
    }
}
