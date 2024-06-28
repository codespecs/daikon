: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -wpi.bak $0 "$@"'
  if 0;
# Fix up mod bits for the argument .dtrace files (in place).
# Usage:  modbit-munge.pl myprog.dtrace ...

# "Missing" (numeric value = 2) modbits are never modified.
# Arguments:
#  -allmod	  Set all modbits to 1.
#  -changed	  Set modbits to 1 iff the printed representation has changed.
#  -addchanged	  Set modbits to 1 if the printed representation has changed.
#                 Leave other modbits as is.  This is the default.
#  -random r	  Set modbits to 1 with probability r, or if
#		  the printed representation has changed, and to 0 otherwise.
#  -random-half	  Like "-random .5"; for convenience.
#  -random-prop	  Set modbits to 1 with the same probability as in the input
#		  file (accounting for setting to 1 if representation changed).
#		  This replaces the command
#		    modbit-munge.pl -random `prop-modified.pl $file` $file


## Superseded by modbit-count.pl
#   -count	Don't set mod bits; just count the number of 0, 1, and 2,
#		printing to standard error.  (Unfortunately, this also
#		rewrites the file, changing its timestamp.)


# Subroutine definition comes early to avoid
#   main::dos_chomp() called too early to check prototype at ...
# warnings.
sub dos_chomp( $ ) {
  my ($arg) = @_;
  if ($arg !~ s/\r\n$//) {
    chomp($arg);
  }
  return $arg;
}


my $debug;
my $ppt;
my $allmod;
my $changed;
my $addchange;
my $random;
my $random_frac;

BEGIN {
  $debug = 0;
  # $debug = 1;

  if (scalar(@ARGV) == 0) {
    die "No arguments provided to modbit-munge.pl";
  }

  $ppt = undef;
  my $action = shift(@ARGV);

  # These are numbers instead of strings to make comparisons faster.
  $allmod = 0;
  $changed = 0;
  $addchange = 0;
  $random = 0;
  $random_frac = 0;
  # $count = 0;
  # @counts = (0, 0, 0);

  if ($action eq "-allmod") {
    $allmod = 1;
  } elsif ($action eq "-changed") {
    $changed = 1;
  } elsif ($action eq "-addchanged") {
    $addchange = 1;
  } elsif ($action eq "-random-prop") {
    $random = 1;
    $random_frac = `prop-modified.pl @ARGV`;
    $random_frac = dos_chomp($random_frac);
    # print STDERR "prop-modified = $random_frac for @ARGV\n"
  } elsif ($action eq "-random-half") {
    $random = 1;
    $random_frac = .5;
  } elsif ($action eq "-random") {
    $random = 1;
    $random_frac = shift(@ARGV);
    srand;			# not necessary in Perl 5.004 and later
  } elsif ($action eq "-count") {
    die "Use modbit-count.pl instead";
    # $count = 1;
  } else {
    $addchange = 1;
    unshift(@ARGV, $action);
  }
  if ($random) {
    if (($random_frac == 0) && (($random_frac ne "0") && ($random_frac ne "0.0"))) {
      die "Argument to -random should be between 0 and 1; you supplied a non-number $random_frac";
    } elsif (($random_frac < 0) || ($random_frac > 1)) {
      die "Argument to -random should be between 0 and 1; you supplied $random_frac";
    }
  }

  if (scalar(@ARGV) == 0) {
    die "No .dtrace file arguments provided to modbit-munge.pl";
  }
}

if (/^$/) {
  if (defined($var)) {
    die "No value for variable $var in ppt $ppt";
  }
  if (defined($val)) {
    die "No modbit for variable $var with value $val in ppt $ppt";
  }
  undef($ppt);
  undef($var);
  undef($val);
  if ($debug) { print STDERR "[blank line]\n"; }
} elsif (! defined($ppt)) {
  $ppt = $_;
  $ppt = dos_chomp($ppt);
  if ($debug) { print STDERR "ppt = $ppt\n"; }
} elsif (! defined($var)) {
  $var = $_;
  $var = dos_chomp($var);
  if ($var =~ /^[0-9]+$/) {
    die "Bad variable name $var in ppt $ppt";
  }
  if ($debug) {  print STDERR "var = $var\n"; }
} elsif (! defined($val)) {
  $val = $_;
  $val = dos_chomp($val);
  if ($debug) {  print STDERR "val = $val\n"; }
  if ($var eq "this_invocation_nonce") {
    undef($var);
    undef($val);
  }
} else {
  $mod = $_;
  $mod = dos_chomp($mod);
  if ($debug) {  print STDERR "mod = $mod\n"; }
  if (! (($mod eq "0") || ($mod eq "1") || ($mod eq "2"))) {
    die "Bad modbit $mod for variable $var with value $val in ppt $ppt";
  }
  # $counts[$mod]++;
  if ($mod == 2) {
    # nothing to do
  } else {
    $lastval = $last{$ppt}{$var};
    if ($val eq "uninit") {
      # Hack for C front end, which produces modbits that are always "0" or "1"
      $mod = 2;
    } elsif ($allmod) {
      $mod = "1";
    # } elsif ($count) {
    #   # do nothing
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
  # if ($count) {
  #   print STDERR "Unmodified: $counts[0]\n";
  #   print STDERR "Modified: $counts[1]\n";
  #   print STDERR "Missing: $counts[2]\n";
  # }
}
