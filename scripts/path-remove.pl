#!/usr/bin/perl

# Remove duplicates and things that I don't want from my path.

$splitchar = ":";
@result = ();
while (<>) {
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
	if ((!defined($already_seen{$temp}))
	    && ($temp !~ /vortex\/(M3|Smalltalk)\/bin\/shell$/)) {
	      $already_seen{$temp} = 1;
	      push(@result, $temp);
          }
      }
}

print join($splitchar, @result);
