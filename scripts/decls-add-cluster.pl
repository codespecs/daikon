#!/usr/bin/perl -w

$decls_file = $ARGV[0];
$decls_file =~ /.*\/(\S*)\.decls/;
$decls_new = "$1_new.decls";

open (IN, $decls_file) || die "couldn't open $decls_file\n";
open (OUT, ">$decls_new") || die "couldn't open $decls_new for output\n";

while (<IN>) {
    $line = $_;
    print OUT $line;
    if($line =~ /:::/) {
	print OUT "cluster\n";
	print OUT "int\n";
	print OUT "int\n";
	print OUT "22\n";
    }
}

close IN;
close OUT;

