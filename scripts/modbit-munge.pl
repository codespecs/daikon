#! /uns/bin/perl -wpi.bak

# Fix up mod bits for the argument .dtrace files.
# "Missing" modbits are never modified.
# Modes:
#   -allmod	Set all modbits to 1
#   -changed	Set modbits to 1 iff the printed representation has changed
#   -addchanged	Set modbits to 1 if the printed representation has changed
#               This is the default.


BEGIN {
  $debug = 0;
  # $debug = 1;

  $ppt = undef;
  $action = shift(@ARGV);
  $allmod = 0;
  $changed = 0;
  $addchange = 0;
  if ($action eq "-allmod") {
    $allmod = 1;
  } elsif ($action eq "-changed") {
    $changed = 1;
  } elsif ($action eq "-addchanged") {
    $addchange = 1;
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
    die "Bad modbit $mod";
  }
  if ($mod eq "2") {
    # nothing to do
  } else {
    $lastval = $last{$ppt}{$var};
    if ($allmod) {
      $mod = "1";
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
    }
    $_ = $mod . "\n";
  }
  $last{$ppt}{$var} = $val;
  undef($var);
  undef($val);
  undef($mod);
}
