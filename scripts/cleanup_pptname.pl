#!/usr/bin/env perl

# cleanup_pptname.pl - remove unwanted characters from a program point
# name, as declared in a decls file.

my @unwanted = ("<", ">","\\\\", "\\/", ";", "\\\(", "\\\)");

my $pptfilename = $ARGV[0];

$pptfilename =~ s/:::/./;
$pptfilename =~ s/Ljava.lang././;
foreach my $token (@unwanted) {
    while ( $pptfilename =~ /$token/) {
	$pptfilename =~ s/$token//;
    }
}
$pptfilename =~ s/</./;
$pptfilename =~ s/>/./;
$pptfilename =~ s/\(\s*(\S+)\s*\)/_$1_/;

#replace two or more dots in a row with just one dot
while ($pptfilename =~ /\.\.+/) {
    $pptfilename =~ s/\.\.+/\./;
}

print $pptfilename;
