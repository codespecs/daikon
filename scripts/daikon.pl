: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Conveniently run daikon to create a .inv file and start the gui
# $Id$

sub usage() {
    print
	"Usage: daikon [OPTION] MAIN_CLASS [MAIN_ARGUMENTS]\n",
	"\n",
	"Options:\n",
        "  -i, --instrument FILE   Only instrument FILE.  Can be specified multiple times.\n",
	"  -o, --output FILE       Save invariants in FILE.inv\n",
	"  -t, --textfile          Save text of invariants in a .txt file\n",
	"  -v, --verbose           Display progress messages\n",
	"  -c, --cleanup           Remove files left over from an interrupted session before starting\n",
        "      --nocleanup         Do not remove temporary files (for debugging)\n",
	"  -n, --nogui             Do not start the gui\n",
	"  -s, --src               Make an archive of the source for later reference\n",
	"  -a, --ajax              Run Ajax analysis\n",
	"\n",
	"Example:\n",
	"  daikon --output test1 packfoo.MyTestSuite 200\n",
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
$DAIKON_WRAPPER_CLASSPATH = $ENV{'DAIKON_WRAPPER_CLASSPATH'} ||
    $ENV{'CLASSPATH'} ||
    '/g2/users/mernst/java/jdk/jre/lib/rt.jar:/g1/users/mistere/java';
$AJAX_DIR = $ENV{'AJAX_DIR'} || "ajax";

$cp_lib = $DAIKON_WRAPPER_CLASSPATH;
$cp_dot = $cp_lib . ':.';

# read options from command line

GetOptions("instrument=s" => \@instrument,
           "output=s" => \$output,
	   "textfile" => \$textfile,
	   "verbose" => \$verbose,
	   "cleanup" => \$cleanup,
	   "nocleanup" => \$nocleanup,
	   "nogui" => \$nogui,
	   "n" => \$nogui,
	   "src" => \$src,
	   "ajax" => \$ajax,
	   ) or usagedie();
my $nowarn = ($verbose ? "-nowarn" : ""); # for jikes

$runnable = shift @ARGV  or usagedie();
$runnable_args = join(' ', @ARGV);

# subroutines

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
}

sub which {
    my $command = shift;
    my @result;
    open(LINES, "which $command |");
    while ($line = <LINES>) {
	chomp $line;
	next if $line =~ /not found/i;
	next if $line =~ /no $command in/;
	push @result, $line;
    }
    return @result;
}

# do cleanup first

if ($cleanup) {
    for $fname (find("*.u")) {
	unlink($fname);
    }
}

# figure out the configuration

$mainsrc = $runnable;
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
    $prefix = $runnable;
    $prefix =~ s/\./-/;
    my $sym = 0;
    do {
	$sym = $sym + 1;
	while ((length $sym) < 4) { $sym = '0' . $sym }
	$output = "$prefix-$sym";
    } while (-x "$output.inv" or -x "$output.inv.gz" or -x "$output.src.tar.gz");
}
print "Output will go in $output...\n" if $verbose;

# check to see that we have jikes avaiable
unless (which('jikes')) {
    die ("You must run the command 'add jikes' before using this tool.\n" .
	 "You may add that line to your ~/.environment file to have jikes\n" .
	 "added automatically every time you log in.\n");
}

# check the program make sure it starts out with no errors
$error = system("jikes -classpath $cp_dot -depend -nowrite $nowarn $mainsrc");
die ("Fix compiler errors before running daikon") if $error;

# come up with a list of files which we need to care about
print "Building dependency list...\n" if $verbose;

die (".u files already exist; try running with --cleanup option") if (find("*.u"));

%interesting = (); # keys are interesting java file names
system("jikes -classpath $cp_dot -depend -nowrite $nowarn +M $mainsrc") && die ("Unexpected jikes error");
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
if ($src) {
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
}

# setup scratch folders
print "Preparing to instrument files...\n" if $verbose;

$working = tmpnam();
die ("tmp filename already exists") if (-e $working);
mkdir($working, 0700) or die("Could not create tmp dir 1");
mkdir("$working/daikon-instrumented", 0700) or die("Could not create tmp dir 2");

while (1) {
    # instrument the source files
    print "Instrumenting files...\n" if $verbose;
    my $files;
    if (@instrument) {
      $files = join(' ', @instrument);
    } else {
      $files = join(' ', sort (keys %interesting));
    }
    $dfejcommand = "dfej -classpath $cp_dot -instrsourcedir=$working/daikon-instrumented/ -declsfiledir=$working/ -tracefilename=$working/the_dtrace_file.dtrace $files";
    if ($verbose) {
      my($cwd) = `pwd`;
      chomp($cwd);
      print STDERR "dfej: cd $cwd; $dfejcommand\n";
    }
    $dfejoutput = `$dfejcommand 2>&1`;
    $dfejerr = $?;
    last if ($dfejerr);

    # run Ajax
    if ($ajax) {
      print "Running Ajax over your java program...\n" if $verbose;
      my($cwd) = `pwd`;
      chomp($cwd);
      chdir($working);
      @decls = find('*.decls', '.');
      if (!@decls) {
	print STDERR "No .decls files found\n";
	chdir($cwd);
	last;
      }
      my $runnable_dots = $runnable;
      $runnable_dots =~ s:/:.:g;
      $ajaxcommand = "java -cp $AJAX_DIR "
	. "ajax.tools.benchmarks.ComparablePairsDescFileReader "
        . "-cp $AJAX_DIR/tweaked-classes.zip -ap $cwd "
	. "$runnable_dots -rewrite " . join(" ", @decls);
      print "Running $ajaxcommand\n" if $verbose;
      $ajaxoutput = `$ajaxcommand 2>&1`;
      $ajaxerr = $?;
      if ($ajaxerr) {
	chdir($cwd);
	last;
      }

      foreach my $d (@decls) {
	rename($d, $d . ".bak") || die "Cannot create backup of $d!\n";
	rename($d . ".ajax", $d) || die "Cannot update $d with $d.ajax!\n";
      }

      chdir($cwd);
    }

    # compile the instrumented source files
    print "Compiling files...\n" if $verbose;
    $cp_work = "$working/daikon-instrumented:$cp_lib:.";
    # $jikescommand = "jikes -classpath $cp_work -depend -g $nowarn ";
    $jikescommand = "jikes -classpath $cp_work -depend -g ";

    $mainsrc_was_instrumented =
        (! @instrument) || (grep {$_ eq $mainsrc} @instrument);
    if ($mainsrc_was_instrumented) {
        $jikescommand .= "$working/daikon-instrumented/$mainsrc";
    } else {
        $jikescommand .= "$mainsrc";
    }

    $jikesoutput = `$jikescommand 2>&1`;
    $jikeserr = $?;
    last if $jikeserr;

    # run the test suite / mainline
    print "Running your java program...\n" if $verbose;
    $javaoutput = `java -classpath $cp_work $runnable $runnable_args 2>&1`;
    $javaerr = $?;
    last if $javaerr;

    # find the results
    $dtrace = join(' ', find('*.dtrace', $working));
    $decls = join(' ', find('*.decls', $working));
    $outerr = !($dtrace && $decls);
    last if $outerr;

    # run modbit-munge
    print "Running modbit-munge...\n" if $verbose;
    $mboutput = `modbit-munge.pl $dtrace 2>&1`;
    $mberr = $?;
    last if $mberr;

    # run daikon
    print "Running invariant detector...\n" if $verbose;
    $output_to = $textfile ? "$output.txt" : '/dev/null';
    $dkerr = system("java -classpath $cp_lib daikon.Daikon -o $output.inv $decls $dtrace > $output_to");
    last if $dkerr;

    # compress the output
    print "Compressing output...\n" if $verbose;
    $gzoutput = `gzip $output.inv`;
    $gzerr = $?;
    last if $gzerr;

    last;
}

if (! $nocleanup) {
  system("rm -rf $working") && die("Could not remove working dir");
}

if ($dfejerr) {
  print $dfejoutput;
  die("dfej error");
}
if ($ajaxerr) {
  print "ajax command: $ajaxcommand\n";
  print "ajax output: $ajaxoutput\n";
  die("ajax error");
}
if ($jikeserr) {
  print "jikes command: $jikescommand\n";
  print "jikes output: $jikesoutput\n";
  die("jikes error");
}
if ($javaerr) {
  print $javaoutput;
  die("java test suite error");
}
if ($outerr) {
  if (! $dtrace) {
    print "No data trace (.dtrace) file found"
      . ($verbose ? " in $working" : "") . ".\n";
  }
  if (! $decls) {
    print "No declaration (.decls) files found"
      . ($verbose ? " in $working" : "") . ".\n";
  }
  die("missing output files");
}
if ($mberr) {
  print $mboutput;
  die("modbit error");
}
if ($dkerr) {
  die("daikon error");
}
if ($gzerr) {
  print $gzoutput;
  die("gzip error");
}

# run the gui
unless ($nogui) {
    print "Starting the gui...\n" if $verbose;
    system("java -classpath $cp_lib daikon.gui.InvariantsGUI $output.inv.gz");
}
