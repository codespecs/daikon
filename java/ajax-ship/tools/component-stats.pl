#!perl -w

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($filename) = @ARGV;

if (!defined($filename)) {
    die "Usage: <filename>\n";
}

&build_data($filename, "c");

my(%varcomponents) = &get_varcomponents();
my(%vars) = &get_vars();

my($numcomponents) = 0;
my(%fanin) = ();
my(%fanout) = ();

foreach $v (keys(%vars)) {
    $fanin{$v} = 0;
    $fanout{$v} = 0;
}
foreach $v (keys(%varcomponents)) {
    foreach $i (@{$varcomponents{$v}}) {
        $numcomponents++;
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

print "num components = $numcomponents, self loops = $selfloops\n";
print "Fan-in:\n";
foreach $n (sort {$a <=> $b} keys(%faninhist)) {
    print "$n:\t$faninhist{$n}\t$faninhistexample{$n}\n";
}
print "Fan-out:\n";
foreach $n (sort {$a <=> $b} keys(%fanouthist)) {
    print "$n:\t$fanouthist{$n}\t$fanouthistexample{$n}\n";
}
