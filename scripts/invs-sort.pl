#! /uns/bin/perl -wn
# Lexically sort invariants (from the Python engine)
# Also suppress warnings about non-symmetric comparabilities.
# This makes two invariant output files much easier to compare (via diff or
# by eye).
# Output goes to standard out.  Input comes from argument file(s) or from
# standard in if no files are specified.
# Run like this:
#   invs-sort.pl foo.invs > file.invs-sorted
# or as a filter:
#   cat foo.invs | invs-sort.pl | ...

my $parasep;

BEGIN {
  # Read a paragraph at a time
  $parasep = "===========================================================================\n";
  $/ = $parasep;
  # use /m in pattern instead
  # $* = 1;			# multi-line patterns
}

s/$parasep//m;
s/^variable comparability is non-symmetric: [^\n]+\n//gm;

## Don't do this; just don't split when starts with tab.
# # Put info about sequences on the same line, because we're about to sort by lines.
# while (s/^\(  [^\n]*\)\n\(\(\t[^\n]*\n\)*\)\t\([^\n]*\n\)/

if ($_ eq "") {
  next;
}
# This does not seem to work.  (Maybe because of redef of $/)
# chomp($_);
s/\n\Z//;
# Don't split on "\n"; keep lines that start with tab with their parents.
my @invs = split(/\n /, $_);
my $ppt = shift(@invs);
@invs = sort(@invs);
unshift(@invs, $ppt);
print $parasep;
print join("\n ", @invs);
print "\n";
