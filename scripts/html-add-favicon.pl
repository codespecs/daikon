#!/usr/bin/env perl
use strict;
use English;
$WARNING = 1;

# Usage:
#   html-add-favicon.pl atT.png `findfile '*.html'`


use File::Basename;

if (scalar(@ARGV) < 2) {
  die "Not enough arguments";
}

my $ico_file = shift(@ARGV);
if ($ico_file !~ /\.png$/) {
  die "Only handles .png files";
}

# Each file argument should be relative to the directory containing the icon file.
for my $arg (@ARGV) {

  my $linkdir = dirname($arg);
  $linkdir =~ s/^\.(\/|$)//;
  # Replace each directory component with "..".
  $linkdir =~ s/[^\/]+/../g;
  if ($linkdir ne "") {
    $linkdir .= "/";
  }
  `preplace '</head>' '<link rel="icon" type="image/png" href="$linkdir$ico_file" />\n</head>' $arg`;
  `preplace '</HEAD>' '<link rel="icon" type="image/png" href="$linkdir$ico_file" />\n</HEAD>' $arg`;
}
