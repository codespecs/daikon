#!/usr/bin/env perl

# cleanup_pptname.pl - remove non-word characters from a program point
# name, as declared in a decls file.

my @unwanted = ("<", ">","\\\\", "\\/", ";", "\\\(", "\\\)");

my $pptfilename = $ARGV[0];

$pptfilename =~ s/:::/./;
$pptfilename =~ s/Ljava.lang././;
foreach my $token (@unwanted) {
    $pptfilename =~ s/$token//g;
}

$pptfilename =~ s/\(\s*(\S+)\s*\)/_$1_/;

#replace two or more dots in a row with just one dot
$pptfilename =~ s/\.\.+/\./g;

print $pptfilename;
