#! perl -w

my($in_sites) = 0;
my(%class_data);

while (<>) {
    if (/^SITES BEGIN/) {
        $in_sites = 1;
    } elsif (/^SITES END/) {
        $in_sites = 0;
        
        print 'SITES BEGIN (ordered by live bytes)
          percent           live            alloc\'ed     stack class
 rank   self  accum      bytes objs        bytes objs    trace name
';
        my(@data) = sort { -(${$class_data{$a}}[0] <=> ${$class_data{$b}}[0]) } keys(%class_data);
        my($rank) = 1;
        my($accum) = 0;
        
        foreach my $class (@data) {
            my($c_percent_self, $c_live_bytes, $c_live_objs, $c_alloc_bytes, $c_alloc_objs)
                = @{$class_data{$class}};
            
            $accum += $c_percent_self;
            printf(" %4d %5.2f%% %5.2f%%  %9d %7d %9d %7d %5d %s\n",
                $rank++, $c_percent_self, $accum,
                $c_live_bytes, $c_live_objs, $c_alloc_bytes, $c_alloc_objs,
                -1, $class);
        }
        
        print 'SITES END
';
    } elsif ($in_sites
        && m!^\s*([0-9]+)\s+([0-9.]+)%\s+([0-9.]+)%\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)\s+([a-zA-Z0-9_/[<>;()\$]+)!) {
        my($rank, $percent_self, $percent_accum, $live_bytes, $live_objs, $alloc_bytes, $alloc_objs, $trace, $class)
            = ($1, $2, $3, $4, $5, $6, $7, $8, $9);
        
        my($c_percent_self, $c_live_bytes, $c_live_objs, $c_alloc_bytes, $c_alloc_objs)
            = defined($class_data{$class})
                ? @{$class_data{$class}}
                : (0, 0, 0, 0, 0);
        
        $c_percent_self += $percent_self;
        $c_live_bytes += $live_bytes;
        $c_live_objs += $live_objs;
        $c_alloc_bytes += $alloc_bytes;
        $c_alloc_objs += $alloc_objs;
        
        my(@vals) = ($c_percent_self, $c_live_bytes, $c_live_objs, $c_alloc_bytes, $c_alloc_objs);
        $class_data{$class} = \@vals;
    }
}
