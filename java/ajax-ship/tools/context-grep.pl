my(@linebuf) = ();
my($printcount) = 0;
my($lineslost) = 0;

my($pattern) = $ARGV[0];

if (!defined($pattern)) {
    die "Need a pattern as first argument";
}

shift(@ARGV);

while (<ARGV>) {
    if (@linebuf >= 5) {
        shift(@linebuf);
        if (!$lineslost) {
            print "...\n";
            $lineslost = 1;
        }
    }
    push(@linebuf, $_);

    if (m!$pattern!o) {
        print @linebuf;
        @linebuf = ();
        $printcount = 5;
        $lineslost = 0;
    } elsif ($printcount > 0) {
        print $_;
        @linebuf = ();
        $printcount--;
        $lineslost = 0;
    }
}
