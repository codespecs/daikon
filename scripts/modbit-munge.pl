#! /uns/bin/perl -wpi.bak

# Fix up mod bits for the argument .dtrace files.
# "Missing" (numeric value = 2) modbits are never modified.
# Modes:
#   -allmod	Set all modbits to 1.
#   -changed	Set modbits to 1 iff the printed representation has changed.
#   -addchanged	Set modbits to 1 if the printed representation has changed.
#               Leave other modbits as is.  This is the default.
#   -random r   Set modbits to 1 with probability r, or if
#		the printed representation has changed, and to 0 otherwise.
#   -count	Don't set mod bits; just count the number of 0, 1, and 2,
#		printing to standard error.  (Unfortunately, this also
#		rewrites the file, changing its timestamp.)


BEGIN {
  $debug = 0;
  # $debug = 1;

  $ppt = undef;
  $action = shift(@ARGV);

  # These are numbers instead of strings to make comparisons faster.
  $allmod = 0;
  $changed = 0;
  $addchange = 0;
  $random = 0;
  $random_frac = 0;
  $count = 0;
  @counts = (0, 0, 0);

  if ($action eq "-allmod") {
    $allmod = 1;
  } elsif ($action eq "-changed") {
    $changed = 1;
  } elsif ($action eq "-addchanged") {
    $addchange = 1;
  } elsif ($action eq "-random") {
    $random = 1;
    $random_frac = shift(@ARGV);
    if (($random_frac == 0) && ($random_frac ne "0") && ($random_frac ne "0.0")) {
      die "Argument to -random should be between 0 and 1; you supplied a non-number $random_frac";
    } elsif (($random_frac < 0) || ($random_frac > 1)) {
      die "Argument to -random should be between 0 and 1; you supplied $random_frac";
    }
    srand;			# not necessary in Perl 5.004 and later
  } elsif ($action eq "-count") {
    $count = 1;
  } else {
    $addchange = 1;
    unshift(@ARGV, $action);
  }
}

if (/^$/) {
  undef($ppt);
  undef($var);
  undef($val);
  if ($debug) { print STDERR "[blank line]\n"; }
} elsif (! defined($ppt)) {
  $ppt = $_;
  chomp($ppt);
  if ($debug) { print STDERR "ppt = $ppt\n"; }
} elsif (! defined($var)) {
  $var = $_;
  chomp($var);
  if ($debug) {  print STDERR "var = $var\n"; }
} elsif (! defined($val)) {
  $val = $_;
  chomp($val);
  if ($debug) {  print STDERR "val = $val\n"; }
} else {
  $mod = $_;
  chomp($mod);
  if ($debug) {  print STDERR "mod = $mod\n"; }
  if (! (($mod eq "0") || ($mod eq "1") || ($mod eq "2"))) {
    die "Bad modbit $mod for variable $var with value $val in $ppt";
  }
  $counts[$mod]++;
  if ($mod == 2) {
    # nothing to do
  } else {
    $lastval = $last{$ppt}{$var};
    if ($allmod) {
      $mod = "1";
    } elsif ($count) {
      # do nothing
    } elsif ($changed) {
      if ((! defined($lastval)) || ($val ne $lastval)) {
	# print STDERR "changed because last $lastval != current $val\n";
	$mod = "1";
      } else {
	$mod = "0";
      }
    } elsif ($addchange) {
      if ((! defined($lastval)) || ($val ne $lastval)) {
	# print STDERR "changed because last $lastval != current $val\n";
	$mod = "1";
      }
    } elsif ($random) {
      if ((! defined($lastval)) || ($val ne $lastval)) {
	# print STDERR "changed because last $lastval != current $val\n";
	$mod = "1";
      } elsif (rand() < $random_frac) {
	$mod = "1";
      } else {
	$mod = "0";
      }
    }
    $_ = $mod . "\n";
  }
  $last{$ppt}{$var} = $val;
  undef($var);
  undef($val);
  undef($mod);
}

END {
  if ($count) {
    print STDERR "Unmodified: $counts[0]\n";
    print STDERR "Modified: $counts[1]\n";
    print STDERR "Missing: $counts[2]\n";
  }
}
