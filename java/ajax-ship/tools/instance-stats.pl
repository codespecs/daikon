#!perl -w

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($filename) = @ARGV;

if (!defined($filename)) {
    die "Usage: <filename>\n";
}

&build_data($filename, "il");

my(%varclusters) = &get_varclusters();
my(%varinstances) = &get_varinstances();
my(%clusterlevels) = &get_clusterlevels();
my(%vars) = &get_vars();

my($numinstances) = 0;
my($samelevelinstances) = 0;
my($selfloops) = 0;
my(%fanin) = ();
my(%fanout) = ();

foreach $v (keys(%vars)) {
    $fanin{$v} = 0;
    $fanout{$v} = 0;
}
foreach $v (keys(%varinstances)) {
    foreach $i (@{$varinstances{$v}}) {
        $numinstances++;
        if (defined($varclusters{$i}) && $clusterlevels{$varclusters{$v}} eq $clusterlevels{$varclusters{$i}}) {
            $samelevelinstances++;
        }
        if ($v eq $i) {
            $selfloops++;
        }
        $fanin{$i}++;
        $fanout{$v}++;
    }
}

my(%faninhist) = ();
my(%fanouthist) = ();
my(%faninhistexample) = ();
my(%fanouthistexample) = ();
foreach $v (keys(%vars)) {
    if (defined($fanin{$v})) {
        $faninhist{$fanin{$v}}++;
        $faninhistexample{$fanin{$v}} = $v;
    }
    if (defined($fanout{$v})) {
        $fanouthist{$fanout{$v}}++;
        $fanouthistexample{$fanout{$v}} = $v;
    }
}

print "num instances = $numinstances, same levels = $samelevelinstances, self loops = $selfloops\n";
print "Fan-in:\n";
foreach $n (sort {$a <=> $b} keys(%faninhist)) {
    print "$n:\t$faninhist{$n}\t$faninhistexample{$n}\n";
}
print "Fan-out:\n";
foreach $n (sort {$a <=> $b} keys(%fanouthist)) {
    print "$n:\t$fanouthist{$n}\t$fanouthistexample{$n}\n";
}
