#!/usr/bin/env perl
# prop-modified trace-file ...
# Return the percentage of non-compulsory modification bits that are set
# in each of the specified dtrace files.

use English;
# use strict;
$WARNING = 1;			# "-w" flag

for $file (@ARGV) {
  system("cp -pf $file $file-preserve-prop-modified");
  if (-f "$file.bak") {
    system("cp -pf $file.bak $file.bak-preserve-prop-modified");
  }
  # This is necessary because the original file might not be valid.
  system("modbit-munge.pl -addchanged $file");
  $orig = `modbit-count.pl $file`;
  system("modbit-munge.pl -changed $file");
  $min = `modbit-count.pl $file`;
  rename("$file-preserve-prop-modified", $file);
  if (-f "$file.bak-preserve-prop-modified") {
    rename("$file.bak-preserve-prop-modified", "$file.bak");
  } else {
    unlink "$file.bak";
  }

  if ($orig !~ /^Unmodified: ([0-9]+)\nModified: ([0-9]+)\nMissing: ([0-9]+)\n$/) {
    die "Bad orig: $orig";
  }
  $orig_unmod = $1;
  $orig_mod = $2;
  if ($min !~ /^Unmodified: ([0-9]+)\nModified: ([0-9]+)\nMissing: ([0-9]+)\n$/) {
    die "Bad min: $min";
  }
  $min_unmod = $1;
  $min_mod = $2;

  # For debugging
  # print $orig;
  # print $min;

  if (($orig_unmod + $orig_mod) != ($min_unmod + $min_mod)) {
    die "Sums don't sum:\n$orig$min";
  }
  print (($orig_mod-$min_mod)/$orig_mod);
  print "\n";
}
