#!perl -w

my($frompkg) = shift(@ARGV);
my($topkg) = shift(@ARGV);

while (<STDIN>) {
    my($f) = $_;

    open(FILE, "<$f");

    my(@lines) = <FILE>;
    my($anychange) = 0;

    foreach $line (@lines) {
        if ($line =~ /\w(class|interface)\w/) {
            last;
        }

        $line =~ s/^package $frompkg/package $topkg/o;
        $line =~ s/^import $frompkg./import $topkg./o;
        $anychange = 1;
    }

    if ($anychange) {
        open(FILE, ">$f");
        print FILE @lines;
    }
}
