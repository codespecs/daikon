: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# houdini.pl -- Iteratively invoke ESC/Java on a source file until a fixed point is reached
# Jeremy Nimmer <jnimmer@lcs.mit.edu>

# Given a (possibly-annotated) source file, and some associated
# txt-esc file, produces ESC/Java output from processing the original
# source file, except uses as many of the annotations from the txt-esc
# file as possible to aid the verfication.

use Carp;
use File::Copy;
use File::Path;
use POSIX qw(tmpnam);

# Algorithm for this script:
# 0 We are given a (possibly-annotated) source file and a txt-esc file
# 1 Read the txt file into memory, adding a nonce to the end of each invariant: "; // gensym"
# 2 Write the txt file (from memory) to a temp file
# 3 Copy the source file to a temp file
# 4 Merge the temp text file into the temp source file
# 5 Run ESC on the temp source file and slurp the results
# 6 Remove the two temporary files
# 7 Grep the results for "/*@ ...; // gensym */"
# 8 If any matching lines are found, remove them from the txt file in memory and go to step 2
# 9 Otherwise, dump the slurped output to stdout and exit

my $debug = 0;

my $tmpdir;
BEGIN {
    $tmpdir = tmpnam();
    die ("temporary filename already exists") if (-e $tmpdir);
    mkdir($tmpdir);
}
END {
    rmtree($tmpdir) if ($tmpdir);
}

sub debug {
    while ($debug && @_) {
	print shift, "\n";
    }
}

sub slurpfile {
    # returns the contents of the first argument (filename) as a list
    my $name = shift;
    open(F, $name) or die("Cannot open $name");
    my @result = <F>;
    close(F);
    return @result;
}

my $gensym_counter = 0;
sub gensym {
    # returns some unique nonce
    my $result = $gensym_counter++;
    while (length($result) < 4) {
	$result = "0" . $result;
    }
    return $result;
}

sub notdir {
    # returns the non-dir part of the filname
    my $name = shift;
    $name =~ s|^.*/||;
    return $name;
}

sub reltmp {
    # returns the non-dir part of the filname, with $tmpdir prepended
    my $name = shift;
    return $tmpdir . "/" . notdir($name);
}

sub writetmp {
    # arg 1 is a filename (no slashes); args 2+ are the contents; returns the filename
    my $name = reltmp(scalar(shift));
    die ("temporary filename already exists") if (-e $name);
    open(F, ">$name");
    print F @_;
    close(F);
    return $name;
}

sub copytmp {
    # returns the filename of a fresh file, which is a copy of the argument (filename)
    my $arg = shift;
    my $name = reltmp($arg);
    die ("source filename does not exist") unless (-e $arg);
    die ("temporary filename already exists") if (-e $name);
    copy($arg, $name);
    return $name;
}

# 0 We are given a (possibly-annotated) source file and a txt-esc file

if ($ARGV[0] eq "-d") {
    print STDERR "Debugging on\n";
    $debug = 1;
    shift @ARGV;
}
my $sourcefile = shift @ARGV;
my $txtescfile = shift @ARGV;

unless ((-f $sourcefile) && (-f $txtescfile) && ($sourcefile =~ /\.java$/)) {
    print STDERR "Usage: $0 source.java annotations.txt-esc\n";
    exit(1);
}

# 1 Read the txt file into memory, adding a nonce to the end of each invariant: ";//nonce-gensym"

my @txtesc = slurpfile($txtescfile);
#@txtesc = 
grep {
    my $nonce = ";//nonce-" . gensym();
    # Skip over things we shouldn't touch; add a gensym to the rest
    m/===========================================================================/
	|| m/\:\:\:(:ENTER|EXIT|OBJECT|CLASS)/
	    || m/variables\:/
		|| s|$|$nonce|;
} @txtesc;

# Iterative ("Houdini") looping
while (1) {
    # 2 Write the txt file (from memory) to a temp file
    $txtesctmp = writetmp($txtescfile, @txtesc);

    # 3 Copy the source file to a temp file
    $sourcetmp = copytmp($sourcefile);

    # 4 Merge the temp text file into the temp source file
    print STDERR `cd $tmpdir && merge-esc.pl $txtesctmp`;
    unlink($sourcetmp);
    rename("$sourcetmp-escannotated", $sourcetmp);

    # 5 Run ESC on the temp source file and slurp the results
    debug("Running ESC");
    my $notdir_sourcetmp = notdir($sourcetmp);
    my @escoutput = `cd $tmpdir && escjava $notdir_sourcetmp`;

    # 6 Remove the two temporary files
    unlink($txtesctmp);
    unlink($sourcetmp);

    # 7 Grep the results for "/*@ ...; //nonce-gensym */"
    my @failures = grep { m|;//nonce-\d+ |; } @escoutput;
    #debug("ESC reported", @failures);
    grep { s|^\s*/\*\@ +\w+ (.*);//nonce-.*|$1|s; } @failures;

    # 8 If any matching lines are found, remove them from the txt file in memory and go to step 2
    my $txtesc_changed = 0;
    for my $failure (@failures) {
	$failure .= ";//nonce-";
	debug("Removing '$failure'");
	my $dec_check = $#txtesc;
	@txtesc = grep { !/^\Q$failure/ } @txtesc;
	if ($#txtesc < $dec_check) {
	    $txtesc_changed = 1;
	} else {
	    print STDERR "Did not find '$failure'\n";
	} 
    }

    # 9 Otherwise, dump the slurped output to stdout and exit
    unless ($txtesc_changed) {
	print @escoutput;
	exit;
    }
}
