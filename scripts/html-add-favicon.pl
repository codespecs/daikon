#!/usr/bin/env perl

# This program takes as arguments a .png file for a "favicon" and a set of
# .html files.  It makes each HTML file use the given favicon.
#
# Usage:
#   html-add-favicon.pl atT.png `find . -iname '*.html'`
#
# Both the .png and the .html filenames should be relative (not absolute),
# as in the given example.

use strict;
use English;
$WARNING = 1;

use File::Basename;

if (scalar(@ARGV) < 2) {
  die "Not enough arguments";
}

my $ico_file = shift(@ARGV);
if ($ico_file !~ /\.png$/) {
  die "Only handles .png files";
}

for my $arg (@ARGV) {

  my $linkdir = dirname($arg);
  $linkdir =~ s/^\.(\/|$)//;
  # Replace each directory component with "..".
  $linkdir =~ s/[^\/]+/../g;
  if ($linkdir ne "") {
    $linkdir .= "/";
  }
  my $headtext = "<link rel=\"icon\" type=\"image/png\" href=\"$linkdir$ico_file\" />";

  # This is ugly, but it handles two capitalizations of "</head>".
  `preplace '(<link rel="icon"[^>]*>\n)?</head>' '$headtext\n</head>' $arg`;
  `preplace '(<link rel="icon"[^>]*>\n)?</HEAD>' '$headtext\n</HEAD>' $arg`;
}
