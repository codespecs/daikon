#!/usr/bin/perl -w -p -i.bak
# striplines.pl
# Strips '#line' directives out of a file.  The file is modified in place,
# but a backup is made to ".bak".

if (/^\#line/) {
  $_ = undef;
}
