: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# study-escjava.pl -- Invoke either the true escjava or whodini
# Michael Ernst <mernst@lcs.mit.edu>



# * If .txt file does NOT exist, just pass through the
#   argument(s), as given, to escjava
# 
# * If .txt file DOES exist, pass the following to whodini:
#   * the txt file
#   * any file(s) requested on the script's command line
#   * if the calling code was requested, ensure that the ADT
#     is also included in the call to whodini
# 
# >See, for example, $inv/papers/esc-paper/study/stackar.who.txt-esc
# 
# This IS an example of a txt file for whodini, if that's what you were
# asking.  Presumably I will configure the studyXX accounts to have
# ~/.houdini/experiment{1,2}.txt as appropriate, and you would check for them
# there.  I could setup study00 with that now, if you like.

use File::Basename;

my $escjava_executable = "/g2/users/mernst/bin/Linux-i686/escjava";
my $whodini_executable = "whodini.pl";
my $txtfiledir = "$ENV{HOME}/.houdini";
my $cwd = `pwd`;
chomp($cwd);

# my $cwdbase = $cwd;
# $cwd =~ s:^.*/::;

# This includes both arguments and also ADTs they use.
# We won't use it if no txt-esc file is found.
my @javafiles = ();

my @txtescfiles = ();

my $debug = 0;

if ((scalar(@ARGV) > 0) && ($ARGV[0] =~ 'escjava$')) {
  shift(@ARGV);
}

if ((scalar(@ARGV) > 0) && ($ARGV[0] eq '-d')) {
  shift(@ARGV);
  $debug = 1;
}

for my $arg (@ARGV) {
  if (! -f $arg) {
    print "escjava: file $arg does not exist\n";
    exit(1);
  }
  push @javafiles, $arg;
  # Look for the ADT file.
  # This assumes that the ADT for FooCheck.java is Foo.java (in the same
  # directory).
  my $adtfile = $arg;		# new var to avoid side-effecting @ARGV
  if (($adtfile =~ s/Check\.java/.java/)
      && -f $adtfile) {
    push @javafiles, $adtfile;
  }
}

for my $jfile (@javafiles) {
  my $base = $jfile;
  $base =~ s/\.java$//;
  print "$jfile => $base\n";
  my $txtescfile = "$txtfiledir/$base.who.txt-esc";
  if (-f $txtescfile) {
    push @txtescfiles, $txtescfile;
  } elsif ($debug) {
    print STDERR "Didn't find $txtescfile\n";
  }
}

if (scalar(@txtescfiles) == 0) {
  my $cmd = $escjava_executable . " " . join(' ', @ARGV);
  if ($debug) { print STDERR "Executing: $cmd\n"; }
  # this call never returns
  exec $cmd;
} elsif (scalar(@txtescfiles) > 1) {
  die "Found multiple whodini background files: " . join(' ', @txtescfiles);
} else {
  my $cmd = "$whodini_executable $txtescfiles[0] " . join(' ', @javafiles);
  if ($debug) { print STDERR "Executing: $cmd\n"; }
  # this call never returns
  exec $cmd;
}

die "Execution cannot reach this point";
