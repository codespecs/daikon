#!/usr/bin/env perl

# Conveniently run daikon to create a .inv file and start the gui
# $Id$

use English;
use strict;
$WARNING = 1;

use POSIX qw(tmpnam);
use Getopt::Long;

use constant TAR_MANIFEST_TAG => $ENV{'TAR_MANIFEST_TAG'} || '-T'; # change to -I for athena tar
use constant DAIKON_WRAPPER_CLASSPATH => $ENV{'DAIKON_WRAPPER_CLASSPATH'} ||
    $ENV{'CLASSPATH'};
 # || '/g2/users/mernst/java/jdk/jre/lib/rt.jar:/g1/users/mistere/java';


sub usage() {
    print
	"Usage: daikon [OPTION] MAIN_CLASS [MAIN_ARGUMENTS]\n",
	"\n",
	"Options:\n",
        "  -i, --instrument FILE   Only instrument FILE.  Can be specified multiple times.\n",
	"  -o, --output FILE       Save invariants in FILE.inv\n",
	"  -t, --textfile          Save text of invariants in a .txt file\n",
	"  -n, --nogui             Do not start the gui\n",
	"      --daikonarg ARG     Supply argument to Daikon; may occur multiple times.\n",
	"  -v, --verbose           Display progress messages\n",
	"  -c, --cleanup           Remove files left over from an interrupted session before starting\n",
        "      --nocleanup         Do not remove temporary files (for debugging)\n",
	"  -s, --src               Make an archive of the source for later reference\n",
	"  -d, --debug             Print extra debugging info\n",
	"\n",
	"Examples:\n",
	" daikon.pl --output test1 packfoo.MyTestSuite 200\n",
	" daikon.pl -i Gries.java -t -o random -c -nogui \\\n   --daikonarg \"--config_option daikon.derive.unary.SequenceSum.enabled=true\" \\\n   GriesTester\n",
	;
} # usage

sub usagedie() {
    usage();
    exit(1);
} # usagedie

# environment vars

my $cp_lib = DAIKON_WRAPPER_CLASSPATH;
my $cp_dot = $cp_lib . ':.';

# read options from command line

my @instrument = ();
my $output = 0;
my $textfile = 0;
my $verbose = 0;
my $cleanup = 0;
my $nocleanup = 0;
my $nogui = 0;
my $src = 0;
my @daikonarg = ();
my $debug = 0;

GetOptions("instrument=s" => \@instrument,
           "output=s" => \$output,
	   "textfile" => \$textfile,
	   "verbose" => \$verbose,
	   "cleanup" => \$cleanup,
	   "nocleanup" => \$nocleanup,
	   "nogui" => \$nogui,
	   "n" => \$nogui,
	   "src" => \$src,
           "daikonarg=s" => \@daikonarg,
	   "debug" => \$debug,
	   ) or usagedie();

my $nowarn = ($verbose ? "" : "-nowarn"); # for jikes

my $runnable = shift @ARGV or usagedie();
my $runnable_args = join(' ', @ARGV);

# do cleanup first

if ($cleanup) {
    $debug && print "running cleanup...\n";
    for my $fname (find("*.u")) {
	$debug && print "deleting $fname\n";
	unlink($fname);
    }
}

# figure out the configuration

my $mainsrc = $runnable;
if ($runnable =~ s/\.java$//) {
  # nothing to do
} else {
  $mainsrc =~ s/\./\//g;
  $mainsrc .= ".java";
}
die ("Source file $mainsrc does not exist") unless (-f $mainsrc);

if ($output) {
    $output =~ s/\.inv$//;
    die("File $output.inv exists") if (-f "$output.inv");
    die("File $output.src.tar.gz exists") if (-f "$output.src.tar.gz");
} else {
    my $prefix = $runnable;
    $prefix =~ s/\./-/;
    my $sym = 0;
    do {
	$sym = $sym + 1;
	while ((length $sym) < 4) { $sym = '0' . $sym }
	$output = "$prefix-$sym";
    } while (-e "$output.inv" or -e "$output.inv.gz" or -e "$output.src.tar.gz");
}

$debug && print "\n";
print "Output will go in $output...\n" if $verbose;

# check to see that we have jikes avaiable
unless (which('jikes')) {
    die ("You must run the command 'add jikes' before using this tool.\n" .
	 "You may add that line to your ~/.environment file to have jikes\n" .
	 "added automatically every time you log in.\n");
}

# check the program make sure it starts out with no errors

$debug && print "compiling initial source program...\n";
$debug && print "jikes -classpath $cp_dot -depend -nowrite $nowarn $mainsrc\n";
$error = system("jikes -classpath $cp_dot -depend -nowrite $nowarn $mainsrc");
die ("Fix compiler errors before running daikon") if $error;

# come up with a list of files which we need to care about
$debug && print "\n";
print "Building dependency list...\n" if $verbose;

die (".u files already exist; try running with --cleanup option") if (find("*.u"));

my %interesting = (); # keys are interesting java file names
$debug && print "generating makefile dependencies...\n";
$debug && print "jikes -classpath $cp_dot -depend -nowrite $nowarn +M $mainsrc\n";
if (system("jikes -classpath $cp_dot -depend -nowrite $nowarn +M $mainsrc")) {
    # something bad has happened...
    die ("Unexpected jikes error");
}

$debug && print "parsing makefile files...\n";
for my $fname (find("*.u")) {
    open (U, $fname);
    while (my $u = <U>) {
	for my $token (split(/\s+/, $u)) {
	    if ($token =~ /^(.+)\.class$/) {
		my $file = $1 . ".java";
		$debug && print "adding $file to interesting hash\n";
		$interesting{$file} = 1;
	    }
	}
    }
    close(U);
    $debug && print "deleting $fname\n";
    unlink($fname);
}

# create a tarball of the source under inspection
if ($src) {
    $debug && print "\n";
    print "Creating source archive...\n" if $verbose;

    my $manifest = tmpnam();
    die ("tmp filename already exists") if (-e $manifest);
    $debug && print "writing manifest $manifest\n";
    open(MANIFEST, ">$manifest");
    for my $srcfile (sort keys %interesting) {
	print MANIFEST "$srcfile\n";
    }
    close(MANIFEST);

    my $err = system("tar cf - @{[TAR_MANIFEST_TAG]} $manifest | gzip > $output.src.tar.gz");
    unlink($manifest) or die("Could not unlink source manifest");
    die("Could not archive source") if $err;
}

# setup scratch folders
$debug && print "\n";
print "Preparing to instrument files...\n" if $verbose;

my $working = tmpnam();
$debug && print "using temporary working directory $working\n";
die ("tmp filename already exists") if (-e $working);
mkdir($working, 0700) or die("Could not create tmp dir 1");
mkdir("$working/daikon-instrumented", 0700) or die("Could not create tmp dir 2");

# Why in the world was this in a loop?  This seems to be
# obfuscated---as far as I can tell, it is not actually looping over
# anything, but merely using it as a way to use "last" statements to
# skip execution if something goes wrong....  There is a better way to
# do this.

# instrument the source files
print "Instrumenting files...\n" if $verbose;
my $files;
if (@instrument) {
    $files = join(' ', @instrument);
} else {
    $files = join(' ', sort (keys %interesting));
}
my $dfejcommand = "dfej -classpath $cp_dot --instr-dir=$working/daikon-instrumented/ --decls-dir=$working/ --dtrace-file=$working/the_dtrace_file.dtrace $files";
if ($verbose) {
    my($cwd) = `pwd`;
    chomp($cwd);
    print STDERR "dfej: cd $cwd; $dfejcommand\n";
}
my $dfejoutput = `$dfejcommand 2>&1`;
my $dfejerr = $?;
if ($dfejerr) {
    print $dfejoutput;
    cleanupWorking($working);
    die("dfej error");
}

# compile the instrumented source files
$debug && print "\n";
print "Compiling files...\n" if $verbose;
my $cp_work = "$working/daikon-instrumented:$cp_lib:.";

# not sure why this was commented out... I suppose so that the user
# will know if the instrumented files have problems...

# $jikescommand = "jikes -classpath $cp_work -depend -g $nowarn ";
my $jikescommand = "jikes -classpath $cp_work -depend -g ";

my $mainsrc_was_instrumented =
    (! @instrument) || (grep {$_ eq $mainsrc} @instrument);
if ($mainsrc_was_instrumented) {
    $debug && print "main source was instrumented\n";
    $jikescommand .= "$working/daikon-instrumented/$mainsrc";
} else {
    $debug && print "main source was not instrumented\n";
    $jikescommand .= "$mainsrc";
}

$debug && print "$jikescommand\n";
my $jikesoutput = `$jikescommand 2>&1`;
my $jikeserr = $?;
if ($jikeserr) {
    print "jikes command: $jikescommand\n";
    print "jikes output: $jikesoutput\n";
    cleanupWorking($working);
    die("jikes error");
}

# run the test suite / mainline
print "\nRunning your java program...\n" if $verbose;
my $javaTestCmd = "java -classpath $cp_work $runnable $runnable_args 2>&1";
my $javaoutput = `$javaTestCmd`;
$debug && print "$javaTestCmd\n";
my $javaerr = $?;
if ($javaerr) {
    print $javaoutput;
    cleanupWorking($working);
    die("java test suite error");
}

# find the results
my $dtrace = join(' ', find('*.dtrace', $working));
if (! $dtrace) {
    print "No data trace (.dtrace) file found"
	. ($verbose ? " in $working" : "") . ".\n";

    cleanupWorking($working);
    die("missing dtrace files");
}

my $decls = join(' ', find('*.decls', $working));
if (! $decls) {
    print "No declaration (.decls) files found"
	. ($verbose ? " in $working" : "") . ".\n";

    cleanupWorking($working);
    die("missing decls files");
}

#      # run modbit-munge
#      print "Running modbit-munge...\n" if $verbose;
#      $mboutput = `modbit-munge.pl $dtrace 2>&1`;
#      $mberr = $?;
#      last if $mberr;

# run daikon
print "\nRunning invariant detector...\n" if $verbose;
my $output_to = $textfile ? "$output.txt" : '/dev/null';
my $daikonCmd = ("java -classpath $cp_lib daikon.Daikon -o $output.inv " .
                 (scalar(@daikonarg)>0 ? join(' ', @daikonarg) : '') .
		 " $decls $dtrace > $output_to");
$debug && print "$daikonCmd\n";
my $dkerr = system($daikonCmd);
if ($dkerr) {
    cleanupWorking($working);
    die("daikon error");
}

# compress the output
print "\nCompressing output...\n" if $verbose;
my $gzCmd = "gzip $output.inv";
$debug && print "$gzCmd\n";
my $gzoutput = `$gzCmd`;
my $gzerr = $?;
if ($gzerr) {
    print $gzoutput;
    cleanupWorking($working);
    die("gzip error");
}

# do cleanup
cleanupWorking($working);

#  if ($mberr) {
#    print $mboutput;
#    die("modbit error");
#  }

# run the gui
unless ($nogui) {
    print "\nStarting the gui...\n" if $verbose;
    system("java -classpath $cp_lib daikon.gui.treeGUI.InvariantsGUI $output.inv.gz");
}


### subroutines ###

# clean up the working directory
sub cleanupWorking {
    my $workingDir = shift;
    if (! $nocleanup) {
	system("rm -rf $workingDir") && die("Could not remove working dir $workingDir");
    } else {
	print "Leaving $workingDir in place\n";
    }
} # cleanupWorking

sub find {
    my $name = shift || die;
    my $root = shift || '.';
    my @result;
    open(F, "find $root -follow -name '$name' |");
    while (my $line = <F>) {
	chomp $line;
	push @result, $line;
    }
    close(F);
    return @result;
} # find

sub which {
    my $command = shift;
    my @result;
    open(LINES, "which $command |");
    while (my $line = <LINES>) {
	chomp $line;
	next if $line =~ /not found/i;
	next if $line =~ /no $command in/;
	push @result, $line;
    }
    return @result;
} # which
