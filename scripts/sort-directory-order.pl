#!/usr/bin/env perl

## Sorts the input lines by directory order:
##   first, every file in a given directory, in sorted order
##   then, process subdirectories recursively, in sorted order
## Example output order:
##   annotatenullable.txt
##   chicory.txt
##   varinfoaux.txt
##   varinfoname.txt
##   asm/asmfile.txt
##   asm/dsforest.txt
##   asm/pptfile.txt
##   asm/testredundantvars.txt
##   asm/x86instruction.txt
##   chicory/arrayinfo.txt
##   chicory/chicorypremain.txt
##   derive/derivation.txt
##   derive/derivationfactory.txt
##   derive/valueandmodified.txt
##   derive/binary/binaryderivation.txt
##   derive/binary/binaryderivationfactory.txt
##   derive/binary/sequencespredicatefactoryfloat.txt
##   derive/binary/sequencespredicatefloat.txt
##   derive/ternary/sequencefloatarbitrarysubsequence.txt
##   derive/ternary/sequencefloatarbitrarysubsequencefactory.txt
##   derive/ternary/ternaryderivation.txt
##   derive/ternary/ternaryderivationfactory.txt
##   derive/unary/sequenceinitial.txt
##   derive/unary/sequenceinitialfactory.txt
##   derive/unary/unaryderivation.txt
##   derive/unary/unaryderivationfactory.txt

## This can be used with "find" to put the results in sorted order.
## (find's default order has to do with where files happen to be stored on
## disk.)  Sorted order may be more sensible to a user, or may make other
## commands more deterministic.  (But for determinism alone, you might as
## well just use "sort", which is faster than this script.)
##
## For example, when making a TAGS file, instead of
##   etags `find . -iname '*.java' -print`
## do
##   etags `find . -iname '*.java' -print | sort-directory-order.pl`

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
