#! /uns/bin/perl -wn
# modbig-count.pl

# Count number of each modbit value in the argument .dtrace files.

# (This does not properly deal with "this_invocation_nonce".  -MDE 9/2003)

my $debug;
my @counts;
my $ppt;
my $var;
my $val;

BEGIN {
  $debug = 0;
  # $debug = 1;

  $ppt = undef;

  @counts = (0, 0, 0);

}

if (/^$/) {
  undef($ppt);
  undef($var);
  undef($val);
  if ($debug) { print STDERR "[blank line]\n"; }
} elsif (! defined($ppt)) {
  $ppt = $_;
  if ($debug) { print STDERR "ppt = $ppt\n"; }
} elsif (! defined($var)) {
  $var = $_;
  if ($debug) {  print STDERR "var = $var\n"; }
} elsif (! defined($val)) {
  $val = $_;
  if ($debug) {  print STDERR "val = $val\n"; }
} else {
  my $mod = $_;
  if ($debug) {  print STDERR "mod = $mod\n"; }
  if (! (($mod eq "0\n") || ($mod eq "1\n") || ($mod eq "2\n"))) {
    die "Bad modbit $mod for variable $var with value $val in $ppt";
  }
  $counts[$mod]++;

  undef($var);
  undef($val);
}

END {
  print "Unmodified: $counts[0]\n";
  print "Modified: $counts[1]\n";
  print "Missing: $counts[2]\n";
}
