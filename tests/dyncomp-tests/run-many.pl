#!/usr/bin/perl
my $kvasir = $ARGV[0];
my $kvasir_short = $kvasir;
$kvasir_short =~ s!^.*/(.*)$!$1!;

while (<STDIN>) {
    chomp;
    printf STDERR "%4d: $kvasir_short $_ 2>&1\n", $.;
    my $rv = system "$kvasir $_ 2>&1";
    die "Aborting" if $rv == 2;
}
