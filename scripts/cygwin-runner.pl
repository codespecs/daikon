: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# cygwin-runner.pl -- Run a windows command as if it were a unix (cygwin) command
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2002-03-18 12:26:39 mistere>

# This script takes a command with arguments and translates those
# arguments from cygwin-style filenames into windows-style filenames.
# Its real advantage is the little bit of intelligence it has as far as
# which things are files and which are not.

# The first argument to cygwin-runner.pl is the directory that the
# target program lives in.  The second argument is the name of the
# program to be invoked.  This can contain path information, which
# will be stripped and ignored.  The rest of the arguments are passed
# on to the target program after filename translation.  For instance,
# "cywin-runner.pl /java/winbin /java/cygbin/javac -version" will run
# "/java/winbin/javac -version" in windows filespace.

# Therefore, a useful way to run this program is as follows:
#
# * Create a new cygbin directory for each winbin directory you have
#
# * Add a file called .runner to cygwin, with contents like:
#       #!/bin/bash
#       [...path-to...]/cygwin-runner.pl /java/winbin $0 $*
#   The first line is sh-bang notation to say that we want bash to
#   run this.  The second line runs the cygwin-runner script, passing
#   it the path to the winbin directory, the name of the program the
#   user ran ($0), , and the rest of the arguments
#
# * For each name in winbin, symlink that name to .runner in cygbin
#       $ cd cygbin
#       $ ln -s .runner java
#       $ ln -s .runner javac
#       ...
# * Put cygbin on your cygwin path, and winbin on your windows path.
#   If both paths are derived from the same setting in your setup, then
#   put cygbin first.  Cygwin will see the cygbin files (symlinks) and
#   run them, whereas windows will not try to run the symlinks and will
#   fall back to the winbin files.
#

# -------------------------------------------------------------------------

# Autoflush
$| = 1;

# Trim a string
sub trim ( $ ) {
    my ($str) = (@_);
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    return $str;
}

# Convert a cygwin filename to a (spaceless) windows filename
sub filetowin( $ ) {
    my ($unix) = @_;
    my $win = trim(`cygpath -ws $unix`);
    if ($win ne '') {
	# The file actually existed.

	my ($unixdir, $unixnotdir);
	my ($windir, $winnotdir);
	if ($unix =~ m|^(.*/)(.*)$|) {
	    ($unixdir, $unixnotdir) = ($1, $2);
	    $win =~ m|^(.*\\)(.*)$|;
	    ($windir, $winnotdir) = ($1, $2);
	} else {
	    ($unixdir, $unixnotdir) = ("", $unix);
	    ($windir, $winnotdir) = ("", $win);
	}
	# If the original filename didn't have spaces, use that name
	# instead, so we preserve the filename extension.
	unless ($unixnotdir =~ m/\s/) {
	    $win = $windir . $unixnotdir;
	}
	return $win;
    }

    # Repeated prune off directories until we have none left
    my ($first, $rest) = ($unix, "");
    while ($first =~ m|./|) {
	$first =~ m|^(.+)/(.*)$| or die;
	$first = $1;
	$rest = "\\" . $2 . $rest;
	if (-e $first) {
	    return trim(`cygpath -ws $first`) . $rest;
	}
    }

    print STDERR "cygwin-runner{$program}: Warning: Unknown cygwin path $unix\n";
    return $unix;
}

# Convert a :-separated list of cygwin filenames to a ;-separated list of (spaceless) windows filenames
sub pathtowin( $ ) {
    my ($path) = @_;
    my @unixelems = split(/[;:]/, $path);
    my @winelems = map { filetowin($_); } @unixelems;
    return join(';', @winelems);
}

# Convert some arbitrary command-line argument from cygwin filenames to windows filenames
sub smartconvert {
    my ($arg) = @_;

    # If it is a filename, we assume it really is a filename
    if (-e $arg) {
	return filetowin($arg);
    }

    # If its dirname is a filename, we assume it really is a filename
    my $dirname = $arg; $dirname =~ s|^(.*)/.*|$1|;
    if (-e $dirname) {
	return filetowin($arg);
    }

    # If it is a :-separated list and conatains at least one filename,
    # we assume it is a path
    if (($arg =~ /:/) && (scalar(grep { (-e $_) } split(':', $arg)))) {
	$arg = pathtowin($arg);
    }

    # If it has an '=' sign in it, split on that and recurse on each
    # half (go heuristics!)
    if ($arg =~ /^(.*)=(.*)$/) {
	return smartconvert($1) . "=" . smartconvert($2);
    }

    # We have no idea, so leave it alone
    return $arg;
}

# Arguments to cygwin-runner itself
local $bin = shift @ARGV;
local $program = shift @ARGV;
$program =~ s/^.*?(\w+)$/$1/ or die("Could not parse program name");

# Sanity checks of arguments to cygwin-runner itself
die("$program: $bin is not a directory") unless (-d $bin);
die("$program: $bin/$program does not exist or is not executable") unless (-x "$bin/$program");

# Translate the arguments
my @ARGS = map { smartconvert($_); } @ARGV;

my $command = filetowin($bin) . "\\" . $program . " " . join(' ', @ARGS);
my $wincwd = filetowin(trim(`pwd`));
$toexec = "CMD /D /C \"cd $wincwd && $command\"";
#print "[[ $toexec ]]\n";

# Use exec instead of backticks so that error code is propogated
exec($toexec);
