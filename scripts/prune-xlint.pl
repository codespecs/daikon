#!/usr/bin/env perl
# prune-xlint.pl

# Removes certain "unchecked" warning messages from "javac -Xlint" output.
# Each warning that is removed is an "unchecked" warning message, such that
# the offending line of source code contains a comment of the form "//
# unchecked".  This permits suppression of warning messages via an inline
# source code comment.

use strict;
use English;
$WARNING = 1;

my $file_line_re = '^[^\n]+:[0-9]+:';

# Slurping in the whole file is slightly inefficient, and it delays output,
# but it made this easy to program.  It should perhaps be changed.

undef $/;
my $contents = <>;

my @pages = split(/($file_line_re.*?^ *\^ *\n|^[0-9]+ (?:warnings|errors)\n)/sm, $contents);

# print "pages: ", scalar(@pages), "\n";
# print join("FOO", @pages);

my $removed_warnings = 0;

my $status = 0;                 # zero means normal completion

my @output_pages = ();
for my $page (@pages) {
  if ($page eq "") { next; }
  if ($page =~ /: warning: \[unchecked\] unchecked.*\/\/ unchecked/s) {
    $removed_warnings++;
    next;
  }
  if ($page =~ /^([0-9]+) warnings\n$/) {
    my $remaining_warnings = ($1 - $removed_warnings);
    if ($remaining_warnings > 0) {
      push(@output_pages, "$remaining_warnings warnings\n");
    }
    next;
  }
  if ($page =~ /$file_line_re/) {
    $status = 1;
  }
  push(@output_pages, $page);
}

print join('', @output_pages);

# print "pages: ", scalar(@output_pages), "\n";
# print join("BAR", @output_pages);

exit $status;
