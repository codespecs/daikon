#!/usr/bin/env perl
# prune-xlint.pl
# For documentation, see javac-xlint, which is a wrapper around this script.

# Removes certain warning messages from "javac -Xlint" output.
# Returns non-zero status if any other warnings (or any errors) exist.

use strict;
use English;
$WARNING = 1;

$OUTPUT_AUTOFLUSH = 1;

my $debug = 0;
# $debug = 1;

my $file_line_re = "^[^\n]+:[0-9]+:";

my @ORIG_ARGV = (@ARGV);
my $file_regexp;
# print "prune-xlint.pl args: " . scalar(@ARGV) . "; #1=$ARGV[0]\n";
if ((scalar(@ARGV) > 1) && ($ARGV[0] eq "-p")) {
  shift(@ARGV);                 # get rid of "-p"
  $file_regexp = shift (@ARGV);
  if ($debug) { print "found arg: $file_regexp\n"; }
}

if (scalar(@ARGV) > 1) {
  # Could also
  die "Too many arguments to prune-xlint.pl (or unrecognized option): prune-xlint.pl " . join(" ", @ORIG_ARGV) ;
}


my $removed_warnings = 0;

my $status = 0;                 # zero means normal completion

# Queue up input until we have a whole record, then output it.
my $data = "";

while (defined(my $line = <>)) {
  # print "line = <<<$line>>>\n";
  if ($line =~ /^\[/) {
    # Let "javac -verbose" messages through immediately
    print $line;
    next;
  }
  $data .= $line;
  my @parts = split(/($file_line_re.*?^[ \t]*\^ *\r?\n|^[0-9]+ (?:warnings?|errors?)\r?\n)/sm, $data);
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

  if ($debug) {
    print "record = <<<$record>>>\n";
  }

  # if ($record eq "") { next; }

  ## Summary of number of errors/warnings
  if ($record =~ /^([0-9]+) errors?\r?\n$/) {
    print $record;
    next;
  }
  if ($record =~ /^([0-9]+) warnings?\r?\n$/) {
    my $remaining_warnings = ($1 - $removed_warnings);
    if ($remaining_warnings > 0) {
      print "$remaining_warnings warnings\n";
    }
    next;
  }

  if ($record !~ /$file_line_re/) {
    # print "START OF RECORD\n";
    # print "$record\n";
    # print "END OF RECORD\n";
    die "this can't happen";
  }

  ## Remove annotated warnings.
  if ($record =~ /: warning: \[(.*)\].*\/\/ ?\1/s) {
    if ($debug) { print "suppressed a \"$1\" warning\n"; }
    $removed_warnings++;
    next;
  }
  ## Remove all warnings in the pruned directories.  Parens around
  ## $file_regexp are crucial, as it might contain a top-level alternation
  ## ("|").
  if (defined($file_regexp) && ($record =~ /($file_regexp).*:[0-9]+: warning: /)) {
    if ($debug) { print "suppressed a warning in pruned directory\n"; }
    $removed_warnings++;
    next;
  }

  $status = 1;
  print $record;
}

print $data;

exit $status;
