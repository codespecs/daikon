#!perl -w

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($out, $prof) = @ARGV;

if (!defined($out)) {
    $out = "out";
}

if (!defined($prof)) {
    $prof = "java.prof";
}

my(%live1) = &readLiveMethods($out);
my(%live2) = &readLiveMethods($prof);

print "Methods in $prof that are not in $out:\n";

foreach $k (keys(%live2)) {
    if (!$live1{$k}) {
        print "$k\n";
    }
}
