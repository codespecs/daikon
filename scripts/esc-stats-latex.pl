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

my @types = ("invariant","set","requires","modifies","ensures","exsures","also_requires","also_modifies","also_ensures","also_exsures","axiom");
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
my $maxverified = 0;
my $maxunverified = 0;
my $maxinexpressible = 0;
my $maxredundant = 0;
my $maxreported = 0;
my $maxmissing = 0;


for my $file (@ARGV) {
  # print "# $file\n";
  open(SOURCE, $file) or die("Cannot open $file!");
  my $javafile = <SOURCE>;
  chomp($javafile);
  $javafile =~ s/\# merged\///;
  my $class = $javafile;
  $class =~ s/\.java$//;
  $class =~ s|^.+/([^/]+)$|$1|; # Strip directories
  $class =~ s/WeightedNodePath/WeightedPath/; # shorten this name

  # Line of code
  my $loc_command = "wc -l < $invdir/tests/sources/$javafile";
  my $loc = `$loc_command`;
  chomp($loc);
  $loc =~ s/^ *//;
  # Non-comment, non-blank lines of code
  my $ncnb_command = "cpp -P -nostdinc -undef $invdir/tests/sources/$javafile | grep '[^ \\t]' | wc -l";
  # print "command = $ncnb_command\n";
  my $ncnbloc = `$ncnb_command`;
  chomp($ncnbloc);
  $ncnbloc =~ s/^ *//;

  my ($verified, $unverified, $inexpressible, $redundant, $missing) = (0, 0, 0, 0, 0);
  my $line = <SOURCE>;		# header line
  my $gotlines = 0;
  while (defined($line = <SOURCE>)) {
    # Cope with multiple tables in a single file.
    if (($line ~= /^\# merged/)
	|| ($line ~= /^TYPE/)) {
      next;
    }
    # print "line: $line";
    # See esc-stats.pl for definitions of the abbreviations.
    my ($category, $evu, $evr, $enu, $enr, $iu, $ir, $a) = split(/[ \t]+/, $line);
    # ignore $category
    $verified += $evu;
    $unverified += $enu;
    $inexpressible += $iu;
    $redundant += $evr + $enr + $ir;
    $missing += $a;
    $gotlines = 1;
  }
  close(SOURCE);
  if ($gotlines == 0) {
    die "No lines read from file $file";
  }

  my $reported = ($verified + $unverified + $inexpressible + $redundant);

  $maxloc = max($maxloc, $loc);
  $maxncnbloc = max($maxncnbloc, $ncnbloc);
  $maxverified = max($maxverified, $verified);
  $maxunverified = max($maxunverified, $unverified);
  $maxinexpressible = max($maxinexpressible, $inexpressible);
  $maxredundant = max($maxredundant, $redundant);
  $maxreported = max($maxreported, $reported);
  $maxmissing = max($maxmissing, $missing);

  $classdata{$class} = [$loc, $ncnbloc, $verified, $unverified, $inexpressible, $redundant, $reported, $missing];
}

# for my $class (sort keys %classdata) {
#   print "$class ${$classdata{$class}}[1]\n";
# }

my $total_loc = 0;
my $total_ncnbloc = 0;
my $total_verified = 0;
my $total_unverified = 0;
my $total_inexpressible = 0;
my $total_redundant = 0;
my $total_reported = 0;
my $total_missing = 0;
my $total_precision = 0;
my $total_recall = 0;

print "% Class         & LOC     & NCNB    & Verif.  & Unverif & Inexpr.    & Redund. & TotRept & Missing & Prec & Rec. \\\\\n";
for my $class (sort {$ {$classdata{$a}}[1] <=> $ {$classdata{$b}}[1]} keys %classdata) {
  ## This doesn't work; not sure why.
  # my ($loc, $ncnbloc, $verified, $unverified, $inexpressible, $redundant, $reported, $missing) = $ $classdata{$class};
  # print "($loc, $ncnbloc, $verified, $unverified, $inexpressible, $redundant, $reported, $missing)\n";
  ## This is a craven admission of defeat:
  my ($loc, $ncnbloc, $verified, $unverified, $inexpressible, $redundant, $reported, $missing) =
    ($ {$classdata{$class}}[0], $ {$classdata{$class}}[1], $ {$classdata{$class}}[2], $ {$classdata{$class}}[3], $ {$classdata{$class}}[4], $ {$classdata{$class}}[5], $ {$classdata{$class}}[6], $ {$classdata{$class}}[7]);
  # print "($loc, $ncnbloc, $verified, $unverified, $inexpressible, $redundant, $reported, $missing)\n";

  my $precision = (1.0 * $verified) / ($verified + $unverified);
  my $recall = (1.0 * $verified) / ($verified + $missing);

  printf("%-15s & %s & %s & %s & %s & %s & %s & %s & %s & %.2f & %.2f \\\\\n",
	 $class,
	 pad_left(7, pad_zph(length($maxloc), $loc)),
	 pad_left(7, pad_zph(length($maxncnbloc), $ncnbloc)),
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
$num_samples = 1.0 * scalar(keys %classdata);
# print "\# $num_samples samples\n";

sub avg ( $ ) {
  my ($x) = @_;
  return round($x / $num_samples);
}


# Copied from above.
printf("%-15s & %s & %s & %s & %s & %s & %s & %s & %s & %.2f & %.2f \\\\\n",
	 "Average",
	 pad_left(7, pad_zph(length($maxloc), avg($total_loc))),
	 pad_left(7, pad_zph(length($maxncnbloc), avg($total_ncnbloc))),
	 pad_left(7, pad_zph(length($maxverified), avg($total_verified))),
	 pad_left(7, pad_zph(length($maxunverified), avg($total_unverified))),
	 pad_left(10, pad_zph(length($maxinexpressible), avg($total_inexpressible))),
	 pad_left(7, pad_zph(length($maxredundant), avg($total_redundant))),
	 pad_left(7, pad_zph(length($maxreported), avg($total_reported))),
	 pad_left(7, pad_zph(length($maxmissing), avg($total_missing))),
	 $total_precision/$num_samples,
	 $total_recall/$num_samples);
print "\\hline\n";
