#!/usr/bin/env perl

# this script is just used for the gtest regression test
# it removes invariants containing elapsed time information

while (<>) {

    if (/elapsed_time_/) {next};
    if (/^return == [0-9]{3,5}$/) {next};
    if (/^return one of \{ [0-9]{3,5}, [0-9]{3,5} \}$/) {next};

    print;
}
