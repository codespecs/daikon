#!/usr/bin/env perl

use v5.14;    #to get given/when

# Turn off warnings (dangerous, but reduces output)
no if $] >= 5.018, warnings => "experimental::smartmatch";

use strict;
use Text::ParseWords;

my @tokens;
my $index;
my $found_class = 0;
my ($sec, $min, $hour, $mday, $mon, $year, $wday, $ydar, $isdst);

while (<>) {

    print;
    if (/^public class / ) {
        $found_class = 1;
        last;
    }
}

if (!$found_class) {
    die "No public class found";
}

# insert comment and code 
print "   // This was added after running jtb to remove serializable warning.\n";
print "   static final long serialVersionUID = ";
($sec, $min, $hour, $mday, $mon, $year, $wday, $ydar, $isdst) = localtime(time);
printf "%4d%02d%02dL;\n", ( ($year+1900), ($mon+1), $mday );
print "\n";

# copy remainder of file to output
while (<>) {
    print;
}
