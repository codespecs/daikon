: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Conveniently run daikon to create a .inv file and start the gui
# $Id$

sub usage() {
    print
	"Usage: $0 [OPTION] <MAIN CLASS> [MAIN ARGUMENTS]\n",
	"\n",
	"Options:\n",
	"  -o, --output FILE   Save invariants in FILE.inv\n",
	"  -t, --textfile      Save text of invariants in a .txt file\n",
	"  -v, --verbose       Display progress messages\n",
	"  -c, --cleanup       Remove files left over from an interrupted session before starting\n",
	"  -n, --nogui         Do not start the gui\n",
	"\n",
	"Example:\n",
	"  $0 --output test1 packfoo.MyTestSuite 200\n",
	;
}

sub usagedie() {
    usage();
    exit(1);
}

use POSIX qw(tmpnam);
use Getopt::Long;

# environment vars

$TAR_MANIFEST_TAG = $ENV{'TAR_MANIFEST_TAG'} || '-T'; # change to -I for athena tar
$DAIKON_WRAPPER_CLASSPATH = $ENV{'DAIKON_WRAPPER_CLASSPATH'} || '/g2/users/mernst/java/jdk/jre/lib/rt.jar:/g1/users/mistere/java';

# read options from command line

GetOptions("output=s" => \$output,
	   "textfile" => \$textfile,
	   "verbose" => \$verbose,
	   "cleanup" => \$cleanup,
	   "nogui" => \$nogui,
	   ) or usagedie();

$runnable = shift @ARGV  or usagedie();
$runnable_args = join(' ', @ARGV);

# subroutines first

sub find {
    my $name = shift || die;
    my $root = shift || '.';
    my @result;
    open(F, "find $root -name '$name' |");
    while (my $line = <F>) {
	chomp $line;
	push @result, $line;
    }
    close(F);
    return @result;
}

# do cleanup first

if ($cleanup) {
    for $fname (find("*.u")) {
	unlink($fname);
    }
}

# figure out the configuration

if ($output) {
    $output =~ s/\.inv$//;
    die("File $output.inv exists") if (-f "$output.inv");
    die("File $output.src.tar.gz exists") if (-f "$output.src.tar.gz");
} else {
    die("gensym not supported yet");
}

$mainsrc = $runnable;
$mainsrc =~ s/\./\//;
$mainsrc .= ".java";
die ("Source file $mainsrc does not exist") unless (-f $mainsrc);

# check the program make sure it starts out with no errors
$error = system("jikes -depend -nowrite -nowarn $mainsrc");
die ("Fix compiler errors before running daikon") if $error;

# come up with a list of files which we need to care about
print "Building dependency list...\n" if $verbose;

die (".u files already exist; try running with --cleanup option") if (find("*.u"));

%interesting = (); # keys are interesting java file names
system("jikes -depend -nowrite -nowarn +M $mainsrc") && die ("Unexpected jikes error");
for $fname (find("*.u")) {
    open (U, $fname);
    while ($u = <U>) {
	for $token (split(/\s+/, $u)) {
	    if ($token =~ /^(.+)\.class$/) {
		$file = $1 . ".java";
		$interesting{$file} = 1;
	    }
	}
    }
    close(U);
    unlink($fname);
}

# create a tarball of the source under inspection
print "Creating source archive...\n" if $verbose;

$manifest = tmpnam();
die ("tmp filename already exists") if (-e $manifest);
open(MANIFEST, ">$manifest");
for $srcfile (sort keys %interesting) {
    print MANIFEST "$srcfile\n";
}
close(MANIFEST);

$err = system("tar cf - $TAR_MANIFEST_TAG $manifest | gzip > $output.src.tar.gz");
unlink($manifest) or die("Could not unlink source manifest");
die("Could not archive source") if $err;

# setup scratch folders
print "Preparing to instrument files...\n" if $verbose;

die("daikon-java already exists") if (-e 'daikon-java');
die("daikon-output already exists") if (-e 'daikon-output');

$working = tmpnam();
die ("tmp filename already exists") if (-e $working);
mkdir($working, 0700) or die("Could not create tmp dir 1");
mkdir("$working/daikon-java", 0700) or die("Could not create tmp dir 2");
symlink($working, "daikon-output") or die("Could not make symlink 1");
symlink("$working/daikon-java", "daikon-java") or die("Could not make symlink 2");

while (1) {
    # instrument the source files
    print "Instrumenting files...\n" if $verbose;
    $dfejerr = system('dfej ' . join(' ', sort (keys %interesting)));
    last if ($dfejerr);

    # compile the instrumented source files
    print "Compiling files...\n" if $verbose;
    $cp = "$DAIKON_WRAPPER_CLASSPATH:$working/daikon-java";
    $jikeserr = system("jikes -classpath $cp -depend -g -nowarn $working/daikon-java/$mainsrc");
    last if $jikeserr;

    # run the test suite / mainline
    print "Running your java program...\n" if $verbose;
    $javaerr = system("java -classpath $cp $runnable $runnable_args");
    last if $javaerr;

    # find the results
    $dtrace = join(' ', find('*.dtrace', 'daikon-output/'));
    $decls = join(' ', find('*.decls', 'daikon-output/'));

    # run modbit-munge
    print "Running modbit-munge...\n" if $verbose;
    $mberr = system("modbit-munge.pl $dtrace");
    last if $mberr;

    # run daikon
    print "Running invariant detector...\n" if $verbose;
    $output_to = $textfile ? "$output.txt" : '/dev/null';
    $dkerr = system("java daikon.Daikon -o $output.inv $decls $dtrace > $output_to");
    last if $dkerr;

    # compress the output
    print "Compressing output...\n" if $verbose;
    $gzerr = system("gzip $output.inv");
    last if $gzerr;

    last;
}

unlink("daikon-java") or die("Could not unlink symlink 2");
unlink("daikon-output") or die("Could not unlink symlink 1");
system("rm -rf $working") && die("Could not remove working dir");

die("dfej error") if $dfejerr;
die("jikes error") if $jikeserr;
die("java test suite error") if $javaerr;
die("modbit error") if $mberr;
die("daikon error") if $dkerr;
die("gzip error") if $gzerr;

# run the gui
unless ($nogui) {
    print "Starting the gui...\n" if $verbose;
    system("java -classpath $cp daikon.gui.InvariantsGUI $output.inv.gz");
}
