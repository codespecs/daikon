#!/usr/bin/env perl
# prune-xlint.pl

# Removes certain "unchecked" warning messages from "javac -Xlint" output.
# Returns non-zero status if any other warnings (or any errors) exist.

# Each warning that is removed is an "unchecked" warning message, such that
# the offending line of source code contains a comment of the form "//
# unchecked".  This permits suppression of warning messages via an inline
# source code comment.  Other (non-"unchecked") warnings and errors on the
# line with "// unchecked" are retained.

use strict;
use English;
$WARNING = 1;

my $file_line_re = "^[^\n]+:[0-9]+:";

my $removed_warnings = 0;

my $status = 0;                 # zero means normal completion

# Queue up input until we have a whole record, then output it.
my $data = "";

while (defined(my $line = <>)) {
  $data .= $line;
  my @parts = split(/($file_line_re.*?^ *\^ *\n|^[0-9]+ (?:warnings|errors)\n)/sm, $data);
  if (scalar(@parts) == 1) {
    next;
  }
  # print "parts: " . join("*****", @parts);
  if (scalar(@parts) > 3) {
    die "bad parts (" . scalar(@parts) . "): " . join("*****", @parts);
  }

  print $parts[0];
  $data = (defined($parts[2]) ? $parts[2] : "");
  my $record = $parts[1];

  # if ($record eq "") { next; }
  if ($record =~ /^([0-9]+) errors\n$/) {
    print $record;
    next;
  }
  if ($record =~ /^([0-9]+) warnings\n$/) {
    my $remaining_warnings = ($1 - $removed_warnings);
    if ($remaining_warnings > 0) {
      print "$remaining_warnings warnings\n";
    }
    next;
  }
  if ($record !~ /$file_line_re/) {
    die "this can't happen";
  }
  if ($record =~ /: warning: \[unchecked\] unchecked.*\/\/ unchecked/s) {
    $removed_warnings++;
    next;
  }
  $status = 1;
  print $record;
}

print $data;

exit $status;
