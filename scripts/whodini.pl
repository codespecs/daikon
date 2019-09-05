: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# whodini.pl -- Iteratively invoke ESC/Java on a source file until a fixed point is reached
# Jeremy Nimmer <jnimmer@lcs.mit.edu>

# Usage: whodini.pl [-d] annotations.txt-esc source.java [source2.java ...]

# Given a txt-esc file, and some number of (possibly-annotated) source
# files, produces ESC/Java output from processing the original source
# files, except uses as many of the annotations from the txt-esc file
# as possible to aid the verfication.  This script attempts to mimic
# the behavior of the Houdini tool from Compaq SRC.

use Carp;
use File::Copy;
use File::Path;
use POSIX qw(tmpnam);

my $escjava_executable = "/g4/projects/invariants/tools/escjava/current/bin/escjava";

# Algorithm for this script:
# 0 We are given a txt-esc file and some (possibly-annotated) source files
# 1 Read the txt file into memory, adding a nonce to the beginning of each invariant
# 2 Write the txt file (from memory) to a temp file
# 3 Copy the source files to a temp file
# 4 Merge the temp text file into the temp source files
# 5 Run ESC on the temp source files and slurp the results
# 6 Remove the two temporary files
# 7 Grep the results for "/*@ ...; // gensym */"
# 8 If any matching lines are found, remove them from the txt file in memory and go to step 2
# 9 Otherwise, dump the slurped output to stdout (with a little tweaking first) and exit

my $debug = 0;
my $output_file = 0;

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
	print STDERR "whodini.pl: ", shift, "\n";
    }
}

sub slurpfile {
    # returns the contents of the first argument (filename) as a list
    my $name = shift;
    debug("Slurping $name");
    open(F, $name) or die("Cannot open $name");
    my @result = <F>;
    close(F);
    return @result;
}

sub slurpfiles {
    # returns the contents of the arguments (filenames) as files
    my @files = @_;
    my @result = ();
    for my $file (@files) {
	my @slurp = slurpfile($file);
	push @result, \@slurp;
    }
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
    # returns the non-dir part of the filenames
    my @names = @_;
    grep { s|^.*/||; } @names;
    return @names;
}

sub reltmp {
    # returns the non-dir part of the filname, with $tmpdir prepended
    my $name = shift;
    return $tmpdir . "/" . join(' ', notdir($name));
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
    die ("source filename '$arg' does not exist") unless (-e $arg);
    die ("temporary filename '$name' already exists") if (-e $name);
    copy($arg, $name) or die("whodini.pl: $!");
    return $name;
}

# 0 We are given a txt-esc file and some (possibly-annotated) source files

if (($#ARGV >= 0) && ($ARGV[0] eq "-d")) {
    $debug = 1;
    shift @ARGV;
    debug("Debugging is on");
}

if (($#ARGV >= 0) && ($ARGV[0] eq "--lastfile")) {
    debug("Will display final file, not escjava output.");
    # When set, output is the final merged version of the first source
    # file placed on the command line.  This lets users see the final
    # file to find out what was written in by whodini.
    $output_file = 1;
    shift @ARGV;
}

my $txtescfile = shift @ARGV;
my @sourcefiles = @ARGV;

{
    my $ok = defined($txtescfile) && (-f $txtescfile);
    $ok &&= scalar(@sourcefiles);
    for my $sourcefile (@sourcefiles) {
	$ok &&= (-f $sourcefile) && ($sourcefile =~ /\.java$/);
    }
    unless ($ok) {
	print STDERR "Usage: $0 annotations.txt-esc source.java [source2.java ...]\n";
	exit(1);
    }
}

# 1 Read the txt file into memory, adding a nonce to the beginning of each invariant

my @txtesc = slurpfile($txtescfile);
#@txtesc =
grep {
    my $nonce = "/*nonce-" . gensym() . "*/ ";
    # Skip over things we shouldn't touch; add a gensym to the rest
    unless (m/===========================================================================/
	    || m/\:\:\:(ENTER|EXIT|OBJECT|CLASS)/
	    || m/[V|v]ariables\:/)
    {
	s|^|$nonce|;
    }
} @txtesc;

print "Houdini is generating likely invariants; please wait.." unless $output_file;

# Iterative ("Houdini") looping
while (1) {
    # 2 Write the txt file (from memory) to a temp file
    my $txtesctmp = writetmp($txtescfile, @txtesc);
    debug("Wrote invariants to $txtesctmp");

    # 3 Copy the source files to a temp file
    my @sourcetmps;
    for my $sourcefile (@sourcefiles) {
	my $sourcetmp = copytmp($sourcefile);
	push @sourcetmps, $sourcetmp;
	debug("Copied $sourcefile to $sourcetmp");
    }

    # 4 Merge the temp text file into the temp source files
    print STDERR `cd $tmpdir && merge-esc.pl -s $txtesctmp`;
    for my $sourcetmp (@sourcetmps) {
	my $sourceann = "$sourcetmp-escannotated";
	# If there are no invariants for a class, it is not processed
	next unless (-e $sourceann);
	unlink($sourcetmp) or die($!);
	rename($sourceann, $sourcetmp) or die($!);
    }
    my @merge_slurped = slurpfile($sourcetmps[0]);

    # 5 Run ESC on the temp source files and slurp the results
    print "." unless $output_file;
    my $notdir_sourcetmp = join(" ", notdir(@sourcetmps));
    my @checked_sources = slurpfiles(@sourcetmps);
    my @escoutput = `cd $tmpdir && $escjava_executable $notdir_sourcetmp`;

    # 6 Remove the two temporary files
    unlink($txtesctmp);
    for my $sourcetmp (@sourcetmps) {
	unlink($sourcetmp);
    }

    # 7 Grep the results for "//@ ensures /*nonce-DDDD*/ ..." and extract the nonce
    my @failure_list = grep { m|/\*nonce-\d{4}\*/|; } @escoutput;
    debug("ESC reported", @failure_list);
    grep { s|^\s*//\@ \w+ /\*(nonce-\d{4})\*/ .*$|$1|s; } @failure_list;
    my %failures;  # remove duplicates
    for my $failure (@failure_list) {
	$failures{$failure} = 1;
    }

    # 8 If any matching lines are found, remove them from the txt file in memory and go to step 2
    my $txtesc_changed = 0;
    for my $failure (keys %failures) {
	debug("Removing '$failure'");
	my $dec_check = $#txtesc;
	@txtesc = grep { !m/\Q$failure/ } @txtesc;
	if ($#txtesc < $dec_check) {
	    $txtesc_changed = 1;
	} else {
	    print STDERR "Did not find '$failure'\n";
	}
    }

    # 9 Otherwise, dump the slurped output to stdout (with a little tweaking first) and exit
    unless ($txtesc_changed) {
      if ($output_file) {
	# Output is the last whodini file, not the escjava result
	print @merge_slurped;
      } else {
	print "\n";
	# Create a mapping from line number of the checked source to
	# the pre-whodini line number.
	local @map;
	local %class2count;
	my $count = 1;
	for my $checked_source_ref (@checked_sources) {
	    my @checked_source = @{$checked_source_ref};
	    my ($line_no, $orig_no);
	    for my $line (@checked_source) {
		$line_no++;
		$orig_no++ unless ($line =~ m|/\*nonce-\d{4}\*/|);
		$map[$count][$line_no] = $orig_no;
	    }
	    $class2count{$sourcefiles[$count-1]} = $count;
	    $count++;
	}
	# Replace line numbers in ESC output with the correct version
	$count = 0;
	grep {
	    # Find "classname ..." to know when ESC has changed to the next file.
	    if (m|^\w+ \.\.\.$|) {
		$count++;
	    }
	    s|(\w+\.java)(\:)(\d+)(\:)|$1.$2 . &swap_lineno_file($1,$3) . $4|e;
	    s|(Suggestion \[)(\d+)(,\d+\]\:)|$1 . &swap_lineno($count,$2) . $3|e;
	    s|(at )(\d+)(,\d+ in)|$1 . &swap_lineno($count,$2) . $3|e;
	    s|( line )(\d+)(, )|$1 . &swap_lineno($count,$2) . $3|e;
	} @escoutput;
	# Show ESC's output
	print @escoutput;
      }
      exit;
    }
}

sub swap_lineno ( $$ ) {
  my ($count, $reported_line) = @_;
  my $result;
  if (defined($count) && defined($reported_line)) {
    $result = $map[$count][$reported_line];
  }
  if (! defined($result)) {
    $count = "" unless defined($count);
    $reported_line = "" unless defined($reported_line);
    print STDERR "whodini.pl: Internal error: map[$count][$reported_line].  Line numbers may be incorrect.\n";
    $result = $reported_line;
  }
  return $result;
}

sub swap_lineno_file ( $$ ) {
  my ($class, $reported_line) = @_;
  my $count = $class2count{$class};
  return swap_lineno($count, $reported_line);
}

