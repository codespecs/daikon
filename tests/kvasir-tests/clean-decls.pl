#!/usr/bin/env perl

my %structs;
my $unique_count = 1;

while (<>) {
    if ((/unnamed_0x([0-9a-f]+)/) || (/^\s+dec\-type\s+unnamed_0x([0-9a-f]+)/)) {
        $structs{$1} = $unique_count++ if not exists $structs{$1};
        s/unnamed_0x([0-9a-f]+)/unnamed_DWARF_ID_$structs{$1}/;
    }
    print;
}
