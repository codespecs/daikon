#! /uns/bin/perl -wpi.bak
# Remove variables from .dtrace files.
# Usage:  trace-purge-vars.pl [-v] regexp file ...

# -v flag means to retain rather than discard variables matching the
# regular expression.  (Its name is taken from grep.)

# I'm intentionally not operating by paragraphs, because I want the regexp
# to only apply to the variable lines.


BEGIN {
  $debug = 0;
  # $debug = 1;

  $discard = 1;
  $regexp = shift(@ARGV);
  if ($regexp eq "-v") {
    $discard = 0;
    $regexp = shift(@ARGV);
  }

  $ppt = undef;
}


if (/^$/) {
  undef($ppt);
  if ($debug) { print STDERR "[blank line]\n"; }
} elsif (! defined($ppt)) {
  $ppt = $_;
} else {
  $var = $_;
  $_ = "";
  $val = <>;
  $mod = <>;
  $match = ($var =~ /$regexp/);
  if ($match ? $discard : !$discard) {
    # nothing to do
  } else {
    print $var;
    print $val;
    print $mod;
  }
}
