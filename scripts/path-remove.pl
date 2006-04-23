#!/usr/bin/env perl

# Remove duplicates, non-existent directories, and things that I don't want
# from my path.  Works for either space- or colon- delimiated paths.
# Usage:
#   # csh
#   setenv PATH `echo $PATH | $HOME/bin/share/path-remove.pl`
#   # sh
#   set path = (`echo $path | $HOME/bin/share/path-remove.pl`)

my $debug = 0;
# $debug = 1;

$splitchar = ":";
@result = ();
while (<>) {
      chomp;
      if (/:/) {
	split(":");
	$splitchar = ":";
      } elsif (/ /) {
	split(" ");
	$splitchar = " ";
      } else {
	# no separators; assume colon, but don't set splitchar.
	split(":");
      }
      if ($debug) {
	print STDERR "splitchar: $splitchar\n";
	print STDERR "" . scalar(@_) . " initial components\n";
      }
      foreach $temp (@_) {
	if ((-e $temp)
	    && (!defined($already_seen{$temp}))
	    && ($temp !~ /vortex\/(M3|Smalltalk)\/bin\/shell$/)) {
	      $already_seen{$temp} = 1;
	      push(@result, $temp);
          }
      }
}

if ($debug) {
  print STDERR "" . scalar(@result) . " final components\n";
}

print join($splitchar, @result);
