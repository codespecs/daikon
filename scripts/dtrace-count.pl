#!/usr/bin/env perl

use strict;
use 5.006;
use warnings;

# $ppt_count{ppt_name} = [count, num_lines];
my %ppt_count;

my %long_names;

$/ = ""; # Read by paragraph
while (<>) {
    next if /^VarComparability/ or /^DECLARE/; # Skip .decls-like paras
    /^(.*):::([A-Z\d]+)$/m or die "Can't parse PPT name from <$_>";
    my $name = "$1:::$2";
    $long_names{$1}{$2} = 1;
    s/\n+$//;
    my $line_count = ($_ =~ tr/\n/\n/) + 1;
    if (exists $ppt_count{$name}) {
	$ppt_count{$name}[0]++;
	die "Mismatched line count ($ppt_count{$name}[1] vs. $line_count)".
	  " for $name"
	    unless $ppt_count{$name}[1] == $line_count;
    } else {
	$ppt_count{$name}[0] = 1;
	$ppt_count{$name}[1] = $line_count;
    }
}

for my $short_name (sort keys %long_names) {
    print "$short_name";
    for my $ppt_part (sort keys %{$long_names{$short_name}}) {
	my $name = "${short_name}:::$ppt_part";
	print " $ppt_part/$ppt_count{$name}[1]: $ppt_count{$name}[0]";
    }
    print "\n";
}

