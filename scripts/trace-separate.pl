: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -wn $0 "$@"'
  if 0;
# trace-separate.pl -- Read data from one or more dtrace files and
# group samples by class they were taken from, writing new trace files
# with grouped data.
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2002-01-29 17:35:57 mistere>

# Probably not useful for C traces, as they are all in the 'std' class.

use FileHandle;

BEGIN {
  # Write .dtrace.gz files (instead of .dtrace)

  # Read by paragraph
  $/ = "\n\n";

  # Map from class -> filehandle
  %files = ();
}

($paragraph = $_) =~ s|^\n+||m;             # Remove leading newlines
($tmp = $paragraph) =~ s|^//[^\n]*$|\n|mg;  # Remove comments 

# Identify class

if ($tmp =~ m|^\s*$|) {
} elsif ($tmp =~ m|^(.+):::OBJECT\n|s) {
  $clazz = $1;
} elsif ($tmp =~ m|^(.+):::CLASS\n|s) {
  $clazz = $1;
} elsif ($tmp =~ m|^(.+)\.[^(]+\([^\)]*\)[^:]+:::\w+\n|s) {
  $clazz = $1;
} else {
  print STDERR "Could not find ppt name in paragraph:\n";
  print STDERR $tmp;
  exit 1;
}

# Write record to separate file

{
  my $fh = $files{$clazz};
  if (! defined($fh)) {
    $fh = new FileHandle;
    my $safeclazz = $clazz;
    $safeclazz =~ s|\W|_|g;
    open $fh, ">separate_$safeclazz.dtrace" or die("Error opening output file: $!");
    print $fh "\n";
    $files{$clazz} = $fh;
  }
  print $fh $paragraph;
}

END {
  # Close all files
  for my $ppt (keys %files) {
    my $fh = $files{$ppt};
    print $fh "// EOF\n";
    close $fh;
  }
}


# Local Variables:
# mode: cperl
# End:
