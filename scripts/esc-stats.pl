: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# esc-stats.pl -- Collect statistics over a ESC-merged file
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>

# The input is one or more source files produced by merge-esc.pl, and
# possibly edited by hand.  Output is a table of statictics with
# invariant counts.

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

for my $file (@ARGV) {
    print "# $file\n";
    open(SOURCE, $file) or die("Cannot open $file!");
    my @lines = <SOURCE>;
    close(SOURCE);
    print "TYPE           \t", join("\t", @categories), "\n";
    for my $type (@types) {
	printf "%-15s", $type;
	print "\t";
	for my $category (@categories) {
	    my $prefix = $prefixes{$category} || die($category);
	    my $literal = $prefix . " " . $type;
	    my $count = grep(/\Q$literal/, @lines);
	    print $count;
	    print "\t" unless ($category eq "A");
	}
	print "\n";
    }
}
