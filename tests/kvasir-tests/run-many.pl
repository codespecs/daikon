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

my $kvasir = $ARGV[0];
my $kvasir_short = $kvasir;
$kvasir_short =~ s!^.*/(.*)$!$1!;

my $failures = 0;

while (<STDIN>) {
    chomp;
    printf STDERR "%4d: $kvasir_short $_ 2>&1\n", $.;
    my $output = `$kvasir $_ 2>&1`;
    my $rv = $?;
    print $output;
    die "Aborting" if $rv == 2;
    my $status = $rv >> 8;
    # A run fails if it cannot be started ($rv == -1), if it is killed by a
    # signal ($rv & 127), or if it exits with status greater than 1.
    # Exit status 1 is not a failure, because some tests deliberately give a
    # program bad input, and the program exits with status 1.
    if ($rv == -1 || ($rv & 127) || $status > 1) {
        printf STDERR "run-many.pl: line %d failed (exit status %d, signal %d): %s %s\n", $., $status, $rv & 127, $kvasir_short, $_;
        print STDERR "run-many.pl: output of line $. follows:\n", $output;
        $failures++;
    }
}

if ($failures > 0) {
    print STDERR "run-many.pl: $failures runs failed\n";
    exit 1;
}
