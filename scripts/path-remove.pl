#!/usr/bin/env perl

# Remove duplicates, non-existent directories, and things that I don't want
# from my path.  Works for either space- or colon- delimiated paths.
# Usage:
#   setenv PATH `echo $PATH | $HOME/bin/share/path-remove.pl`
#   set path = (`echo $path | $HOME/bin/share/path-remove.pl`)


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
      foreach $temp (@_) {
	if ((-e $temp)
	    && (!defined($already_seen{$temp}))) {
	      $already_seen{$temp} = 1;
	      push(@result, $temp);
          }
      }
}

print join($splitchar, @result);
