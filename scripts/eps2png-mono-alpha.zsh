#!/usr/bin/env zsh
# Old shebang line: #!/bin/zsh -f

# Convert a black on white Postscript image (as produced by dot) into
# a black-on-transparent PNG of the specified width, with height
# computed to preserve the aspect ratio. We do this by rendering the
# .eps file into a bitmap 4 times larger (in each dimension) than the
# desired output, shrinking it by a factor of 4 to get an anti-aliased
# grayscale image, and then using that as an alpha mask along with an
# all-black image of the same size to make a black-on-transparent
# result. Also, we use Perl to overwrite the useless "number of
# significant bits" chunk that pnmtopng writes with a chunk that
# specifies that the background color should be white. This is for the
# benefit of Netscape 4.x, which can't handle true transparency and
# would otherwise render the image as black on black background.

psfile=$1
width=$2
temp=/tmp/eps2png$$.pgm
# Default to whatever's on our path
pnmtopng=`which pnmtopng`
pnmtopng238=`which pnmtopng-2.38`
if [ -x /g6/users/smcc/bin/pnmtopng ]; then
    # We need at least version 2.38, not 2.37.5 as in Red Hat.
    pnmtopng=/g6/users/smcc/bin/pnmtopng
elif [ -n "$pnmtopng238" ]; then
    # This is what we've called it on our Debian boxes, which as of
    # now also have a broken version in /usr/bin (Debian bug #274907)
    pnmtopng=$pnmtopng238
fi
# echo Using $pnmtopng

if [ ! -x $pnmtopng ]; then
    echo "Can't find pnmtopng!" >&2
    exit 1;
fi

pstopnm -portrait -stdout -xsize $[4 * $width] $psfile \
  | pnmcrop \
  | pnmscale 0.25 \
  | pnmmargin -white 20 \
  | ppmtopgm >$temp && \
$pnmtopng -alpha <(pnminvert $temp) \
  <(pbmmake -black $(pnmfile $temp \
                     | perl -ne 'print "$1 $2" if /(\d+) by (\d+)/')) \
  | perl -0777 -pe 's/sBIT\x1\cH\204\.\375M/bKGD\0\377\207\217\314\277/' \
  >${psfile%.eps}.png && \
rm $temp
