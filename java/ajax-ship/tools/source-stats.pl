#!perl -w

my($threshold) = ($#ARGV > 0 && $ARGV[0] =~ /^[0-9]/) ? shift(@ARGV) : 0;
my($visited_threshold) = ($#ARGV > 0 && $ARGV[0] =~ /^[0-9]/) ? shift(@ARGV) : 0;

my($total_sources) = 0;
my($total_visited) = 0;
my($total_invalidations) = 0;
my($total_incomplete_invalidations) = 0;

while (<ARGV>) {
    if (/^\[ .* S=([0-9]+),([0-9]+)/) {
        my($sources, $visited) = ($1, $2);
        
        if ($sources >= $threshold && $visited >= $visited_threshold) {
            $total_sources += $sources;
            $total_visited += $visited;
            $total_invalidations++;
            
            if ($visited < $sources) {
                $total_incomplete_invalidations++;
            }
        }
    }
}

print "Visited sources = $total_visited/$total_sources (%", 100*($total_visited/$total_sources), ")\n";
print "Incomplete invalidations = $total_incomplete_invalidations/$total_invalidations (%", 100*($total_incomplete_invalidations/$total_invalidations), ")\n";

