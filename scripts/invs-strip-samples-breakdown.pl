#! /uns/bin/perl -wpi.bak
# Remove "Samples breakdown" line.
# Usage:  invs-strip-samples-breakdown.pl file ...

# Trivial script, but it's convenient to have it as a separate command.

s/^    Samples breakdown.*\n$//;
