my(@amounts) = ();

while (<STDIN>) {
    if (/^\s*(\[[A-Z])\s+[0-9]+\s+([0-9]+)/) {
        my(@pair) = ($2, $1);
        push(@amounts, \@pair);
    } elsif (m!^\s*([A-Za-z0-9_/[]+);\s+[0-9]+\s+([0-9]+)$!) {
        my(@pair) = ($2, $1);
        push(@amounts, \@pair);
    }
}

@amounts = sort {${$a}[0] <=> ${$b}[0]} @amounts;

foreach $a (@amounts) {
    my($name) = ${$a}[1];

    print ${$a}[0], "\t", $name, "\n";
}

my($accum) = 0; grep { $accum += ${$_}[0]; } @amounts;
print $accum, "\n";
