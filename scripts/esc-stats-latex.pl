: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# esc-stats.pl -- Typeset a table out of statistics from an ESC-merged file
# Michael Ernst <mernst@lcs.mit.edu>

# The input is a number of files, each produced by esc-stats.pl.
# The output is a .tex file.

use English;
use Carp;

my @types = ("invariant","set","requires","modifies","ensures","exsures","also_req","also_mods","also_ens","also_exs");
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


my $invdir = "/g2/users/mernst/research/invariants";

print "Class   & NCNB LOC & Verified & Unverified & Inexpressible & Redundant & Total reported & Missing & Precision & Recall \\\\ \\hline\n";

for my $file (@ARGV) {
  # print "# $file\n";
  open(SOURCE, $file) or die("Cannot open $file!");
  my $javafile = <SOURCE>;
  chomp($javafile);
  $javafile =~ s/\# merged\///;
  my $class = $javafile;
  $class =~ s/\.java$//;
  $class =~ s|^.+/([^/]+)$|$1|; # Strip directories
  # Non-comment, non-blank lines of code
  my $ncnb_command = "cpp -P -nostdinc -undef $invdir/tests/sources/$javafile | grep '[^ \\t]' | wc";
  # print "command = $ncnb_command\n";
  my $ncnbloc = `$ncnb_command`;
  chomp($ncnbloc);
  $ncnbloc =~ s/^ *([0-9]+) .*$/$1/;

  my ($verified, $unverified, $inexpressible, $redundant, $missing) = (0, 0, 0, 0, 0);
  my $line = <SOURCE>;		# header line
  while (defined($line = <SOURCE>)) {
    # See esc-stats.pl for definitions of the abbreviations.
    # print "line: $line";
    my ($category, $evu, $evr, $enu, $enr, $iu, $ir, $a) = split(/[ \t]+/, $line);
    # ignore $category
    $verified += $evu;
    $unverified += $enu;
    $inexpressible += $iu;
    $redundant += $evr + $enr + $ir;
    $missing += $a;
  }
  close(SOURCE);
  print "$class\t& $ncnbloc\t& $verified\t& $unverified\t& $inexpressible\t& $redundant\t& ", ($verified + $unverified + $inexpressible + $redundant), "\t& $missing\t& ";
  printf("%.2f", (1.0 * $verified) / ($verified + $unverified));
  print "\t& ";
  printf("%.2f", (1.0 * $verified) / ($verified + $missing));
  print "\t\\\\\n";
}
