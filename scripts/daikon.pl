: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Conveniently run daikon to create a .inv file and start the gui
# $Id$

sub usage() {
    print "Usage: $0 [options] <runnable class> [arguments]\n";
    print "Options: --output file[.inv]\n";
    print "         --verbose\n";
    print "         --killu\n";
}

sub usagedie() {
    usage();
    exit(1);
}

use POSIX qw(tmpnam);
use Getopt::Long;

# global vars

$RTLIB = '/g2/users/mernst/java/jdk/jre/lib/rt.jar:/g1/users/mistere/java';

# read options from command line

GetOptions("output=s" => \$output,
	   "verbose" => \$verbose,
	   "killu" => \$killu,
	   ) or usagedie();

$runnable = shift @ARGV or usagedie();
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

die (".u files already exist") if (!$killu && find("*.u"));

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

  # have to change -T to -I for athena tar (blah!)
$err = system("tar cf - -T $manifest | gzip > $output.src.tar.gz");
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
    $cp = "$RTLIB:$working/daikon-java";
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
    $dkerr = system("java daikon.Daikon -o $output.inv $decls $dtrace > /dev/null");
    last if $dkerr;

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

# run the gui
print "Starting the gui...\n" if $verbose;

system("java -classpath $cp daikon.gui.InvariantsGUI $output.inv");
