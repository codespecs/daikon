#!/usr/bin/perl

$inv_file = $ARGV[0];
$out = $inv_file.".purge";
open (IN, $inv_file) || die "couldn't open $inv_file\n";
open (OUT, ">$out") || die "couldn't open $out for output\n";

while (<IN>) {
    $line = $_;
    if($line !~ /\[.*cluster.*\]/ && $line !~ /reverse/) {
	print OUT $line;
    }
}

close IN;
close OUT;

