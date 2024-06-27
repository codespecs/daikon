: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# esc-stats.pl -- Typeset a table out of statistics from an ESC-merged file
# Michael Ernst <mernst@lcs.mit.edu>

# The input is a number of files, each produced by esc-stats.pl.
# The output is a .tex file.

# TO DO:
#  produce mini-tables for specific classes, in addition to one big table.

use English;
use Carp;
use POSIX;

my @types = ("invariant","set","requires","modifies","ensures","exsures","also_requires","also_modifies","also_ensures","also_exsures","axiom","assume", "nowarn");

my %types_map = (
		 "invariant" => "Object",
		 "set" => "Unsound",
		 "requires" => "Requires",
		 "modifies" => "Modifies",
		 "ensures" => "Ensures",
		 "exsures" => "Ensures",
		 "also_requires" => "Requires",
		 "also_modifies" => "Modifies",
		 "also_ensures" => "Ensures",
		 "also_exsures" => "Ensures",
		 "axiom" => "Unsound",
		 "assume" => "Unsound",
		 "nowarn" => "Unsound",
		 );
my @printed_types = ("Object", "Requires", "Modifies", "Ensures", "Unsound");
my %printed_types = ("Object" => 1, "Requires" => 1, "Modifies" => 1, "Ensures" => 1, "Unsound" => 1);

my @categories = ("EVU","EVR","ENU","ENR","IU","IR","A");
my %prefixes =
    (EVU => "/*@",   # Expressible Verified Unique
     EVR => "/**@",  # Expressible Verified Redundant
     ENU => "/*#",   # Expressible Unverified Unique
     ENR => "/**#",  # Expressible Unverified Redundant
     IU => "/*!",    # Inexpressible Unique
     IR => "/**!",   # Inexpressible Redundant
     A => "//@",     # Added by hand
     );
# ignore @types, @categories, %prefixes


my $invdir = ($ENV{'INV'} or "/g2/users/mernst/research/invariants");


sub pad_zph ( $$ ) {
  my ($width, $value) = @_;
  my $orig_width = length($value);
  if ($orig_width > $width) {
    die "$value is already wider than $width";
  }
  if ($width > $orig_width) {
    return ("\\zph" x ($width-$orig_width)) . " " . $value;
  } else {
    return $value;
  }
}

sub pad_left ( $$ ) {
  my ($width, $value) = @_;
  return (" " x ($width-length($value))) . $value;
}

sub round ( $ ) {
  my ($x) = @_;
  return floor($x + .5);
}

sub max ( $$ ) {
  my ($a, $b) = @_;
  return ($a > $b) ? $a : $b;
}


my $maxloc = 0;
my $maxncnbloc = 0;
my $maxmethods = 0;
my $maxverified = 0;
my $maxunverified = 0;
my $maxinexpressible = 0;
my $maxredundant = 0;
my $maxreported = 0;
my $maxmissing = 0;

my $single = 0;

if ($ARGV[0] eq '-s') {
  $single = 1;
  shift @ARGV;
}

my %ver;
my %unver;
my %inexp;
my %redun;
my %miss;
my %classdata;


for my $file (@ARGV) {
  # print "# $file\n";
  open(SOURCE, $file) or die("Cannot open $file!");
  my @sources = grep(s|\# merged/(.*)\n|$invdir/tests/sources/$1|, <SOURCE>);
  close(SOURCE);

  my ($loc, $ncnbloc, $methods) = (0, 0, 0);
  for my $one_javafile (@sources) {
    # Lines of code
    my $loc_command = "cat $one_javafile | wc -l";
    $loc += `$loc_command`;
    # Non-comment, non-blank lines of code
    my $ncnb_command = "cpp -P -nostdinc -undef $one_javafile | grep '[^ \\t]' | wc -l";
    $ncnbloc += `$ncnb_command`;
    # Very coarse: only counts number of public methods.  Should be fixed.
    my $methods_command = 'perl -n -e ' .
      "'" .
      'if (/\b(?:public|private|protected)\b.*\b(\w+)\s*(\([^\)]*\))(.*[^;])?$/) { print; }' .
      "'" .
      " $one_javafile | wc -l";
    $methods += `$methods_command`;
  }

  open(SOURCE, $file) or die("Cannot open $file!");
  my $javafile = <SOURCE>;
  chomp($javafile);
  $javafile =~ s/\# merged\///;
  my $class = $javafile;
  $class =~ s/\.java$//;
  $class =~ s|^.+/([^/]+)$|$1|; # Strip directories
  $class =~ s/WeightedNodePath/WeightedPath/; # shorten this name

  my ($verified, $unverified, $inexpressible, $redundant, $missing) = (0, 0, 0, 0, 0);
  for my $typ (@printed_types) {
    $ver{$typ} = 0;
    $unver{$typ} = 0;
    $inexp{$typ} = 0;
    $redun{$typ} = 0;
    $miss{$typ} = 0;
  }

  my $line = <SOURCE>;		# header line
  my $gotlines = 0;
  while (defined($line = <SOURCE>)) {
    # Cope with multiple tables in a single file.
    if (($line =~ /^\# merged/)
	|| ($line =~ /^TYPE/)) {
      next;
    }
    $gotlines = 1;
    # print "line: $line";
    # See esc-stats.pl for definitions of the abbreviations.
    my ($type, $evu, $evr, $enu, $enr, $iu, $ir, $a) = split(/[ \t]+/, $line);
    $type = $types_map{$type};
    if ($single) {
      # Columns:  Verified, Unverified, Inexpressible, Redundant, Missing
      # Rows: Object, Requires, Modifies, Ensures
      $ver{$type} += $evu;
      $unver{$type} += $enu;
      $inexp{$type} += $iu + $ir;
      $redun{$type} += $evr + $enr;
      $miss{$type} += $a;
    } else {
      if (exists($printed_types{$type})) {
	$verified += $evu;
	$unverified += $enu;
	$inexpressible += $iu + $ir;
	$redundant += $evr + $enr;
	$missing += $a;
      }
    }
  }
  close(SOURCE);
  if ($gotlines == 0) {
    die "No lines read from file $file";
  }

  my $reported = ($verified + $unverified + $inexpressible + $redundant);

  $maxloc = max($maxloc, $loc);
  $maxncnbloc = max($maxncnbloc, $ncnbloc);
  $maxmethods = max($maxmethods, $methods);
  $maxverified = max($maxverified, $verified);
  $maxunverified = max($maxunverified, $unverified);
  $maxinexpressible = max($maxinexpressible, $inexpressible);
  $maxredundant = max($maxredundant, $redundant);
  $maxreported = max($maxreported, $reported);
  $maxmissing = max($maxmissing, $missing);

  # print "$class := [$loc, $ncnbloc, $methods, $verified, $unverified, $inexpressible, $redundant, $reported, $missing]\n";

  $classdata{$class} = [$loc, $ncnbloc, $methods, $verified, $unverified, $inexpressible, $redundant, $reported, $missing];
}

# for my $class (sort keys %classdata) {
#   print "$class ${$classdata{$class}}[1]\n";
# }

my $total_loc = 0;
my $total_ncnbloc = 0;
my $total_methods = 0;
my $total_verified = 0;
my $total_unverified = 0;
my $total_inexpressible = 0;
my $total_redundant = 0;
my $total_reported = 0;
my $total_missing = 0;
my $total_precision = 0;
my $total_recall = 0;		# not a movie

my $num_samples = 1.0 * scalar(keys %classdata);
# print "\# $num_samples samples\n";
sub avg ( $ ) {
  my ($x) = @_;
  return round($x / $num_samples);
}

if ($single) {
  # print "% Type ... \n";
  for my $typ (@printed_types) {
    my $ver = $ver{$typ};
    my $unver = $unver{$typ};
    my $inexp = $inexp{$typ};
    my $redun = $redun{$typ};
    my $miss = $miss{$typ};
    my $reported = $ver + $unver + $inexp + $redun;


    printf("%-10s & %s & %s & %s & %s & %s & %s \\\\ \\hline\n",
	   $typ, $ver, $unver, $inexp, $redun, $reported, $miss);
    $total_verified += $ver;
    $total_unverified += $unver;
    $total_inexpressible += $inexp;
    $total_redundant += $redun;
    $total_reported += $reported;
    $total_missing += $miss;
  }
  print "\\hline\n";
  printf("%-10s & %s & %s & %s & %s & %s & %s \\\\ \\hline\n",
	 "Total", $total_verified, $total_unverified, $total_inexpressible,
	 $total_redundant, $total_reported, $total_missing);
} else {
  print "% Class         & LOC     & NCNB    & Methods & Verif.  & Unverif & Inexpr.    & Redund. & TotRept & Missing & Prec & Rec. \\\\\n";
  for my $class (sort {$ {$classdata{$a}}[1] <=> $ {$classdata{$b}}[1]} keys %classdata) {
    ## This doesn't work; not sure why.
    # my ($loc, $ncnbloc, $methods, $verified, $unverified, $inexpressible, $redundant, $reported, $missing) = $ $classdata{$class};
    ## This is a craven admission of defeat:
    my ($loc, $ncnbloc, $methods, $verified, $unverified, $inexpressible, $redundant, $reported, $missing) =
      ($ {$classdata{$class}}[0], $ {$classdata{$class}}[1], $ {$classdata{$class}}[2], $ {$classdata{$class}}[3], $ {$classdata{$class}}[4], $ {$classdata{$class}}[5], $ {$classdata{$class}}[6], $ {$classdata{$class}}[7], $ {$classdata{$class}}[8]);
    # print "$class: ($loc, $ncnbloc, $methods, $verified, $unverified, $inexpressible, $redundant, $reported, $missing)\n";

    my $precision = (1.0 * $verified) / ($verified + $unverified);
    my $recall = (1.0 * $verified) / ($verified + $missing);

    printf("%-15s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %.2f & %.2f \\\\\n",
	   $class,
	   pad_left(7, pad_zph(length($maxloc), $loc)),
	   pad_left(7, pad_zph(length($maxncnbloc), $ncnbloc)),
	   pad_left(7, pad_zph(length($maxmethods), $methods)),
	   pad_left(7, pad_zph(length($maxverified), $verified)),
	   pad_left(7, pad_zph(length($maxunverified), $unverified)),
	   pad_left(10, pad_zph(length($maxinexpressible), $inexpressible)),
	   pad_left(7, pad_zph(length($maxredundant), $redundant)),
	   pad_left(7, pad_zph(length($maxreported), $reported)),
	   pad_left(7, pad_zph(length($maxmissing), $missing)),
	   $precision,
	   $recall);

    $total_loc += $loc;
    $total_ncnbloc += $ncnbloc;
    $total_methods += $methods;
    $total_verified += $verified;
    $total_unverified += $unverified;
    $total_inexpressible += $inexpressible;
    $total_redundant += $redundant;
    $total_reported += $reported;
    $total_missing += $missing;
    $total_precision += $precision;
    $total_recall += $recall;
  }

  print "\\hline\n";
# Copied from above.
  printf("%-15s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %.2f & %.2f \\\\\n",
	 "Average",
	 pad_left(7, pad_zph(length($maxloc), avg($total_loc))),
	 pad_left(7, pad_zph(length($maxncnbloc), avg($total_ncnbloc))),
	 pad_left(7, pad_zph(length($maxmethods), avg($total_methods))),
	 pad_left(7, pad_zph(length($maxverified), avg($total_verified))),
	 pad_left(7, pad_zph(length($maxunverified), avg($total_unverified))),
	 pad_left(10, pad_zph(length($maxinexpressible), avg($total_inexpressible))),
	 pad_left(7, pad_zph(length($maxredundant), avg($total_redundant))),
	 pad_left(7, pad_zph(length($maxreported), avg($total_reported))),
	 pad_left(7, pad_zph(length($maxmissing), avg($total_missing))),
	 $total_precision/$num_samples,
	 $total_recall/$num_samples);
#	 (1.0 * $total_verified) / ($total_verified + $total_unverified),
#	 (1.0 * $total_verified) / ($total_verified + $total_missing));
  print "\\hline\n";
}
