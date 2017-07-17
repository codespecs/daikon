: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# esc-stats.pl -- Collect statistics over a ESC-merged file
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>

# The input is one or more source files produced by merge-esc.pl, and
# possibly edited by hand.  Output is a table of statictics with
# invariant counts.

use Carp;

my @types = ("invariant","set","requires","modifies","ensures","exsures","also_requires","also_modifies","also_ensures","also_exsures","axiom","assume", "nowarn");
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
	    my @matches = grep(/\Q$literal/, @lines);
	    if ($category eq ("EVU")) {
		if ($type eq "invariant") {
		    # remove invariants added by a heuristic
		    @matches = grep(!/\Q.owner == this/, @matches);
		} elsif ($type eq "set") {
		    # remove invariants added by a heuristic
		    @matches = grep(!/\Q.owner = this/, @matches);
		}
	    }
	    # remove "dumb" things
	    @matches = grep(!m|// dumb|, @matches);
	    my $count = scalar(@matches);
	    print STDERR "$file: " . (join("$file: ", @matches)) if ($count);
	    print $count;
	    print "\t" unless ($category eq "A");
	}
	print "\n";
    }
}
