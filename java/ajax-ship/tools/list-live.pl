#!perl -w

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($out, $package_prefix) = @ARGV;

if (!defined($out)) {
    $out = "out";
}

if (!defined($package_prefix)) {
    $package_prefix = "";
}

my(%live) = &readLiveMethods($out);
my($pp_len) = length($package_prefix);

foreach my $k (keys(%live)) {
    if (substr($k, 0, $pp_len) eq $package_prefix) {
        print $k, "\n";
    }
}
