: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -wn $0 "$@"'
  if 0;
# trace-separate.pl -- Read data from one or more dtrace files and
# group samples by class they were taken from, writing new trace files
# with grouped data.
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2002-01-29 17:53:46 mistere>

# Probably not useful for C traces, as they are all in the 'std' class.

use FileHandle;
use Compress::Zlib;

BEGIN {
  # Write .dtrace.gz files (instead of .dtrace)
  $compress = 1;

  # Read by paragraph
  $/ = "\n\n";

  # Map from class -> filehandle
  %files = ();
}

($paragraph = $_) =~ s|^\n+||m;             # Remove leading newlines
($tmp = $paragraph) =~ s|^//[^\n]*$|\n|mg;  # Remove comments 

# Identify class

if ($tmp =~ m|^\s*$|) {
  next;
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
    my $safeclazz = $clazz;
    $safeclazz =~ s|\W|_|g;
    my $filename = "separate_$safeclazz.dtrace";
    if ($compress) {
      $fh = gzopen("$filename.gz", "wb") or die("Error opening output file: $gzerrno");
      $fh->gzwrite("\n");
    } else {
      $fh = new FileHandle;
      open $fh, ">$filename" or die("Error opening output file: $!");
      print $fh "\n";
    }
    $files{$clazz} = $fh;
  }
  if ($compress) {
    $fh->gzwrite($paragraph);
    $fh->gzwrite("\n");
  } else {
    print $fh $paragraph;
    print $fh "\n";
  }
}

END {
  # Close all files
  for my $ppt (keys %files) {
    my $fh = $files{$ppt};
    if ($compress) {
      $fh->gzwrite("// EOF\n");
      $fh->gzclose;
    } else {
      print $fh "// EOF\n";
      close $fh;
    }
  }
}


# Local Variables:
# mode: cperl
# End:
