#!perl -w

# This script traces out part of the constraint graph.

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($filename, $from) = @ARGV;

if (!defined($to)) {
    die "Usage: <filename> <from>\n";
}

&build_data($filename);

my(%varclusters) = &get_varclusters();
my(%varinstances) = &get_varinstances();

my(%clustervars);
foreach my $v (keys(%varclusters)) {
    push(@{$clustervars{$varclusters{$k}}}, $v);
}

my(%clusterinstances);
my(%clustersources);
foreach my $v (keys(%varclusters)) {
    my($c) = $varclusters{$v};
    
    foreach my $t (@{$varinstances{$v}}) {
        my($c2) = $varclusters{$t};
        
        push(@{$clusterinstances{$c}}, $c2);
        push(@{$clustersources{$c2}}, $c);
    }
}

foreach my $v (@to_vars) {
    $to_hash{$v} = 1;
}

foreach my $v (@from_vars) {
}

sub traceclusters {
    my($start, $mode, @ignorelist) = @_;

    my($tracesources) = $mode =~ /s/;
    my($traceparents) = $mode =~ /p/;
    my($traceinstances) = $mode =~ /i/;
    my($tracecomponents) = $mode =~ /c/;
    my($tracethroughglobals) = $mode =~ /g/;

    my(%ignore) = ();
    foreach my $i (@ignorelist) {
        $ignore{$i} = 1;
    }

    my(@newfound) = ( $start );
    my(%found) = ( $start => 1 );

    my($traceelems) = sub {
        foreach my $elem (@_) {
            if (!$found{$elem} && !$ignore{$elem}
                && ($tracethroughglobals || defined($varclusters{$elem}))) {
                $found{$elem} = 1;
                push(@newfound, $elem);
            }
        }
    };

    while (@newfound) {
        my($v) = pop(@newfound);

        if ($tracecomponents) {
            &$traceelems(@{$varcomponents{$v}});
        }
        if ($traceinstances) {
            &$traceelems(@{$varinstances{$v}});
        }
        if ($traceparents) {
            &$traceelems(@{$varparents{$v}});
        }
        if ($tracesources) {
            &$traceelems(@{$varsources{$v}});
        }
    }

    %found;
}
