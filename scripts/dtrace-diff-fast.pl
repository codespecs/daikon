#!/usr/bin/env perl

# dtrace-diff-fast.pl
# arguments:  dtrace-diff declsfile dtrace1 dtrace2
# (declsfile is ignored, but accepted for compatibility with dtrace-diff.pl)
# Outputs the first difference that isn't a large pointer-like integer


use strict;
use 5.006;
use warnings;
BEGIN {
    require English;
    if ($^V ge 5.8.0) {
        English->import("-no_match_vars"); # avoid speed penalty
    } else {
        English->import();
    }
}

if (scalar(@ARGV) != 3) {
  die "Usage: $0 <declsname> <dtrace1> <dtrace2>\n";
}
my ($decls_name, $dt1_name, $dt2_name) = @ARGV;
if ($dt1_name =~ /\.gz$/) {
    open(DT1, "gzip -dc $dt1_name |") or die "Can't open $dt1_name: $!\n";
} else {
    open(DT1, "<$dt1_name") or die "Can't open $dt1_name: $!\n";
}
if ($dt2_name =~ /\.gz$/) {
    open(DT2, "gzip -dc $dt2_name |") or die "Can't open $dt2_name: $!\n";
} else {
    open(DT2, "<$dt2_name") or die "Can't open $dt2_name: $!\n";
}
for (;;) {
    my $dt1_line = <DT1>;
    my $dt2_line = <DT2>;
    no warnings 'uninitialized';
    next if $dt1_line eq $dt2_line and defined $dt1_line; # common case
    if (!defined $dt1_line and !defined $dt2_line) {
        # Both dtrace files finished at the same time, without differences
        exit 0;
    } elsif (!defined $dt1_line) {
        print "$dt1_name ended before $dt2_name\n";
        exit 1;
    } elsif (!defined $dt2_line) {
        print "$dt2_name ended before $dt1_name\n";
        exit 1;
    }
    chomp $dt1_line;
    chomp $dt2_line;
    if ($dt1_line =~ /^\d+$/ and $dt2_line =~ /^\d+$/) {
        if ($dt1_line > 2**16 and $dt2_line > 2**16 and
            abs($dt1_line - $dt2_line) < 2**16) {
            next;
        }
    } elsif ($dt1_line =~ /^\[/ and $dt2_line =~ /^\[/) {
	$dt1_line =~ s/\d{4,}/<big-number>/g;
	$dt2_line =~ s/\d{4,}/<big-number>/g;
	next if $dt1_line eq $dt2_line;
    }
    print "$dt1_name and $dt2_name differ at line $.:",
      " `$dt1_line' vs. `$dt2_line'\n";
    exit 1;
}
