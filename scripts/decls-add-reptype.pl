#!/usr/bin/env perl
# decls-add-reptype.pl -- add representation type to .decls files

# Run like this:  decls-add-reptype.pl file ...

use English;
# use strict;
$WARNING = 1;			# "-w" flag
$INPLACE_EDIT = "bak";		# "-ibak" flag

BEGIN {
  $debug = 0;
  # $debug = 1;

  $ppt = undef;
  $type = undef;
}

if (/^$/) {
  undef($ppt);
  if ($debug) { print STDERR "[blank line]\n"; }
} elsif (/^DECLARE$/) {
  undef($ppt);
} elsif (! defined($ppt)) {
  $ppt = $_;
  chomp($ppt);
  if ($debug) { print STDERR "ppt = $ppt\n"; }
} elsif (! defined($var)) {
  $var = $_;
  chomp($var);
  if ($debug) {  print STDERR "var = $var\n"; }
} elsif (! defined($type)) {
  $type = $_;
} else {
  # Print the representation type
  $type =~ s/\baddress\b/int/g;
  $type =~ s/\bchar\b/int/g;
  $type =~ s/\blong\b/int/g;
  print $type;

  undef($var);
  undef($type);
}
