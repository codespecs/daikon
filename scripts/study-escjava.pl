: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# study-escjava.pl -- Invoke either the true escjava or whodini
# Michael Ernst <mernst@lcs.mit.edu>

# This script acts like an enhanced version of escjava.  It invokes either
# the true escjava or whodini.  It invokes whodini if any relevant
# annotation files (.who.txt-esc) exist; if so, it must pass along those
# annotation files and possibly also other .java files besides those on the
# command line.

# In particular:
#  * If .txt file does NOT exist, just pass through the
#    argument(s), as given, to escjava
#  * If .txt file DOES exist, pass the following to whodini:
#     * the txt file
#     * any file(s) requested on the script's command line
#     * if the calling code was requested, ensure that the ADT
#       is also included in the call to whodini

my $escjava_executable = "/g2/users/mernst/bin/Linux-i686/escjava";
my $whodini_executable = "whodini.pl";
my $txtfiledir = "$ENV{HOME}/.houdini";
my $cwd = `pwd`;
chomp($cwd);

# This list includes both arguments and also ADTs they use.
# We won't use it if no txt-esc file is found.
my %javafiles = ();

# List of relevant whodini annotation (.who.txt-esc) files.
my @txtescfiles = ();

my $debug = 0;

# Process arguments
if ((scalar(@ARGV) > 0) && ($ARGV[0] =~ 'escjava$')) {
  shift(@ARGV);
}
if ((scalar(@ARGV) > 0) && ($ARGV[0] eq '-d')) {
  shift(@ARGV);
  $debug = 1;
}

# Create a list of all files listed on the command line, plus any
# ADTs that they use.
for my $arg (@ARGV) {
  $javafiles{$arg} = 1;
  # Look for the ADT file.
  # This assumes that the ADT for FooCheck.java is Foo.java (in the same
  # directory).
  my $adtfile = $arg;		# new var to avoid side-effecting @ARGV
  if (($adtfile =~ s/Check\.java/.java/)
      && -f $adtfile) {
    $javafiles{$adtfile} = 1;
  }
}

# Find whodini annotation files for all java files from previous step.
for my $jfile (keys %javafiles) {
  my $base = $jfile;
  $base =~ s/\.java$//;
  # print "$jfile => $base\n";
  my $txtescfile = "$txtfiledir/$base.who.txt-esc";
  if (-f $txtescfile) {
    push @txtescfiles, $txtescfile;
  } elsif ($debug) {
    print STDERR "Didn't find $txtescfile\n";
  }
}

# Call escjava, or call whodini, or err if too many background files found.
if (scalar(@txtescfiles) == 0) {
  my $cmd = $escjava_executable . " " . join(' ', @ARGV);
  if ($debug) { print STDERR "Executing: $cmd\n"; }
  # this call never returns
  exec $cmd;
} elsif (scalar(@txtescfiles) > 1) {
  die "Found multiple whodini background files: " . join(' ', @txtescfiles);
} else {
  my $cmd = "$whodini_executable $txtescfiles[0] " . join(' ', keys %javafiles);
  if ($debug) { print STDERR "Executing: $cmd\n"; }
  # this call never returns
  exec $cmd;
}

die "Execution cannot reach this point";
