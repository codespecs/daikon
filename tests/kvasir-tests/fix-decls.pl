#!/usr/bin/env perl

my $first_line = 1;

while (<>) {
    if (/^input-language /) {
        if ($first_line == 1) {
            $first_line = 0;
        } else {
            print "\n";
        }
    }
    print;
}
