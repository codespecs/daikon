#!perl -w

use FindBin;
require "$FindBin::Bin/toollib.pm";

my($from, $to, $mode) = @ARGV;
$mode = $mode . ",";

my(%varlinks) = ();

open(F, "gzcat vardump0.gz |");
while (<F>) {
    if (/^([0-9]+) -> ([0-9]+)/) {
        my($src, $dest) = ($1, $2);

        if (/\[style=dashed/) {
            push(@{$varlinks{$src}}, $dest);
        } else {
            if (/MODES=(CMODE,)?(DMODE,)?/ &&
                ($1 eq $mode || $2 eq $mode)) {
                push(@{$varlinks{$dest}}, $src);
            }
        }
    }
}

my(%reachable) = ($from => "root");
my(@check) = ($from);
while (!defined($reachable{$to}) && @check) {
    my($node) = pop(@check);

    foreach $n (@{$varlinks{$node}}) {
        if (!defined($reachable{$n})) {
            $reachable{$n} = $node;
            push(@check, $n);
        }
    }
}

if (defined($reachable{$to})) {
    my(@path) = ();

    while ($to ne "root") {
        push(@path, $to);
        $to = $reachable{$to};
    }

    print "Path: ", join(" ", reverse(@path)), "\n";
} else {
    print "No path\n";
    print join(" ", keys(%reachable)), "\n";
}
