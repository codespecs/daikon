#!/usr/bin/env perl

# this script is just used for the gtest regression test
# it removes 'counts' output that is current directory dependent

while (<>) {
    if (/IsPathSeparator/) {next};
    if (/CStringEquals/) {next};
    print;
}
