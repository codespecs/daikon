#!/usr/bin/env perl

# Print the first two lines of each record in the file.

use strict;
use English;
$WARNING = 1;

$/ = "";

my $para;
while (defined($para = <>)) {
  if ($para =~ /^(.*\n.*\n)/) {
    print "$1\n";
  } else {
    print $para;
  }
}
