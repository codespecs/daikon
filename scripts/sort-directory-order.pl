#!/usr/bin/env perl

## Sorts the input lines by directory order:
##   everything in a given directory first, then subdirectories one after another

## This can be used with "find" to put the results in a more sensible order
## for use in TAGS files, etc.:  instead of
##   etags `findfile '*.java'`
## do
##   etags `findfile '*.java' | sort-directory-order.pl`

## Mac OS X has a "-s" option to "find" that is similar (but not
## necessarily identical) to "find | sort".

my $debug = 0;
# $debug = 1;

if ($debug) {
  print "test1: " . parents_first("./daikon/derive/binary/SequencesUnionFactory.java.jpp",
                                  "./daikon/derive/package.html") . "\n";
  exit;
}


my @lines = <>;
my @sorted_lines = sort parents_first @lines;
print @sorted_lines;




sub num_slashes ( $ ) {
  my ($tmp) = @_;
  return ($tmp =~ tr/\///);
}

sub longest_common_prefix {
  my $prefix = shift;
  for (@_) {
    chop $prefix while (! /^\Q$prefix\E/);
  }
  return $prefix;
}


# Sort, placing entries in parent directories first.
sub parents_first ( $$ ) {
  my ($a, $b) = @_;

  my $num_slashes_a = num_slashes($a);
  my $num_slashes_b = num_slashes($b);
  if ($debug) { print "slashes: $num_slashes_a $num_slashes_b\n"; }
  if ($num_slashes_a == $num_slashes_b) {
    return ($a cmp $b);
  }

  my $prefix = longest_common_prefix($a, $b);
  my $num_slashes_prefix = num_slashes($prefix);
  my $num_extra_slashes_a = $num_slashes_a - $num_slashes_prefix;
  my $num_extra_slashes_b = $num_slashes_b - $num_slashes_prefix;
  if ($debug) { print "prefix ($num_slashes_prefix slashes): $prefix\n"; }
  if ($debug) { print "num_extra_slashes $num_extra_slashes_a $num_extra_slashes_b\n"; }

  if (($num_extra_slashes_a == 0) && ($num_extra_slashes_b != 0)) {
    return -1;
  } elsif (($num_extra_slashes_b == 0) && ($num_extra_slashes_a != 0)) {
    return 1;
  } else {
    return ($a cmp $b);
  }

}
