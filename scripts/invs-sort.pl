#! /uns/bin/perl -wn
# Lexically sort invariants (from the Python engine)

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
# Don't split on "\n"; keep lines that start with tab with their parents.
my @invs = split(/\n /, $_);
my $ppt = shift(@invs);
$invs[$#invs] =~ s/\n\Z//;
@invs = sort(@invs);
unshift(@invs, $ppt);
print $parasep;
print join("\n ", @invs);
print "\n";
