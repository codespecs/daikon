#!/usr/bin/perl

# run-many.pl
# This script is used to run fjalar mutiltiple times
# for a single test scenario.  It assumes that arg0
# is a script that will invoke fjalar and that STDIN
# has been redirected to a file where each line is
# a set of arguments to be passed to the arg0 script.
# In practice, one adds the line:
#   TESTS_SCRIPT := <name of file containing args to fjalar>
# to the scenario Makefile and the rest is automatic.
# Each run's output is written to standard output.
# run-kvasir.sh decides whether a run failed.

use FindBin;
use File::Temp qw(tempfile);

my $kvasir = $ARGV[0];
my $kvasir_short = $kvasir;
$kvasir_short =~ s!^.*/(.*)$!$1!;
my $run_kvasir = "$FindBin::Bin/run-kvasir.sh";
my (undef, $run_out) = tempfile(UNLINK => 1);

my $failures = 0;

while (<STDIN>) {
    chomp;
    my $args = $_;
    my $lineno = $.;
    printf STDERR "%4d: $kvasir_short $args 2>&1\n", $lineno;
    my $rv = system("sh '$run_kvasir' '$run_out' $kvasir $args");
    open(my $fh, '<', $run_out) or die "Cannot read $run_out: $!";
    print do { local $/; <$fh> };
    close($fh);
    die "Aborting" if ($rv & 127) == 2;
    if ($rv != 0) {
        print STDERR "run-many.pl: line $lineno failed: $kvasir_short $args\n";
        $failures++;
    }
}

if ($failures > 0) {
    print STDERR "run-many.pl: $failures runs failed\n";
    exit 1;
}
