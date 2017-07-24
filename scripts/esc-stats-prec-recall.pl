: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# esc-stats-prec-recall.pl -- Collect statistics over a ESC-merged file.
# Calculate the precision and recall. (Adapted from esc-stats.pl)
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>

# The input is one or more source files produced by merge-esc.pl, and
# possibly edited by hand.  Output is a table of statictics with
# invariant counts, precision and recall

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
    my $evu = 0; my $enu = 0; my $a = 0; #the number of invariants in each category

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

	    if ($category eq "A") {
		$a = $a + $count;
	    } elsif ($category eq "EVU") {
		$evu = $evu + $count;
	    } elsif ($category eq "ENU") {
		$enu = $enu + $count;
	    }
	}
	print "\n";
    }

    my $t1 = $enu + $evu;
    my $t2 = $evu + $a;
    my $precision = 0; my $recall = 0;

    if ($t1 != 0) {
	$precision = $evu/$t1;
	$precision = sprintf("%.2f", $precision);
    }

    if ($t2 != 0) {
	$recall = $evu/$t2;
	$recall = sprintf("%.2f", $recall);
    }

    print "==========================================\n";
    print "$file\n";
    print "EVU = $evu, ENU = $enu, A = $a\n";
    print "(EVU + ENU) = $t1, (EVU + A) = $t2\n";
    print "precision = $precision, recall = $recall\n";
    print "==========================================\n\n";

}
