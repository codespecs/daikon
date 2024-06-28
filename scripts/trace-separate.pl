: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -wn $0 "$@"'
  if 0;
# trace-separate.pl -- Read data from one or more dtrace files and
# group samples by class (for Java) or method (for C) they were taken
# from, writing new trace files with grouped data.
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2024-06-26 19:15:26 mernst>

use FileHandle;
use Compress::Zlib;

my $compress;
my $per_method;
my %files;
my $paragraph;
my $tmp;
my $bucket_name;

BEGIN {
  # Write .dtrace.gz files (instead of .dtrace)
  $compress = 1;

  # Per-method buckets (for Java; C always does per-method)
  $per_method = 0;
  if (($#ARGV >= 0) && ($ARGV[0] eq "--per_method")) {
    $per_method = 1;
    shift @ARGV;
  }

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
} elsif ($tmp =~ m/^(input-language|decl-version|var-comparability)/) {
  next;
} elsif ($tmp =~ m|^(?:ppt )?std\.(\w+)|s) {
  # C programs: std.method(...)
  $bucket_name = $1;
} elsif ($tmp =~ m|^(?:ppt )?(.+):::OBJECT\n|s) {
  $bucket_name = $1;
} elsif ($tmp =~ m|^(?:ppt )?(.+):::CLASS\n|s) {
  $bucket_name = $1;
} elsif ($tmp =~ m|^(?:ppt )?(.+)(\.[^(]+)\([^\)]*\)[^:]*:::\w+\n|s) {
  $bucket_name = $1;
  if ($per_method) {
    $bucket_name .= $2;
  }
} else {
  print STDERR "Could not find ppt name in paragraph:\n";
  print STDERR $tmp;
  exit 1;
}

# Write record to separate file

{
  my $fh = $files{$bucket_name};
  if (! defined($fh)) {
    my $safeclazz = $bucket_name;
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
    $files{$bucket_name} = $fh;
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
      $fh->gzclose();
    } else {
      print $fh "// EOF\n";
      close $fh;
    }
  }
}


# Local Variables:
# mode: cperl
# End:
