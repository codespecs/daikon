#! /uns/bin/perl -wpi.bak
# Remove program points (functions) from .dtrace files.
# Usage:  trace-purge-fns.pl [-v] regexp file ...

# -v flag means to retain rather than discard functions matching the
# regular expression.

# I'm intentionally not operating by paragraphs, because I want the regexp
# to only apply to the first line.

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

  $discarding = 0;

  $firstppt = 1;
}

if (/^$/) {
  undef($ppt);
  $discarding = 0;
  $_ = "";
  if ($debug) { print STDERR "[blank line]\n"; }
} elsif (! defined($ppt)) {
  $ppt = $_;
  if ($ppt =~ /^\#/) {
    $discarding = 0;
  } elsif ($ppt =~ /$regexp/) {
    $discarding = $discard;
  } else {
    $discarding = ! $discard;
  }
  if (!$discarding) {
    if ($firstppt) {
      # Avoid outputting a blank line first.
      $firstppt = 0;
    } else {
      print "\n";
    }
  }
}

if ($discarding) {
  $_ = "";
}
