#!/bin/zsh

# Convert a black on white Postscript image (as produced by dot) into
# a black-on-transparent PNG of the specified width, with height
# computed to preserve the aspect ratio. We do this by rendering the
# .eps file into a bitmap 4 times larger (in each dimension) than the
# desired output, shrinking it by a factor of 4 to get an anti-aliased
# grayscale image, and then using that as an alpha mask along with an
# all-black image of the same size to make a black-on-transparent
# result.

psfile=$1
width=$2
temp=/tmp/eps2png$$.pgm
# We need at least version 2.38, not 2.37.5 as in RedHat.
pnmtopng=/g6/users/smcc/bin/pnmtopng

pstopnm -portrait -stdout -xsize $[4 * $width] $psfile | pnmcrop | pnmscale 0.25 | pnmmargin -white 20 | ppmtopgm >$temp
$pnmtopng -alpha <(pnminvert $temp) <(pbmmake -black $(pnmfile $temp | perl -ne 'print "$1 $2" if /(\d+) by (\d+)/')) >${psfile%.eps}.png
rm $temp
