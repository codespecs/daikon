open(IN, "<opcodelist");
open(OUT, ">opcodeinfo");

my(@lengths) = ();
my(@exns) = ();
my(@opdecls) = ();
my(@casestmt) = ();
my(@stackdeltas) = ();
my(@stackpushes) = ();
my($index) = 0;

select OUT;
while (<IN>) {
    if (! /^#/) {
        my(@fields) = split;
        my(@exnlist) = ();

        $opdecls[$index] = "    public static final byte OP_" . $fields[0] . " = " . $index . ";\n";
        $casestmt[$index] = "       case OP_" . $fields[0] . ":\n";
        $lengths[$index] = 1;
        $stackdeltas[$index] = -99;
        $stackpushes[$index] = -99;
        foreach $f (@fields) {
            if ($f =~ /([a-z_]+)=([-a-zA-Z0-9_]*)/) {
                if ($1 eq "len") {
                    $lengths[$index] = $2;
                } elsif ($1 eq "throws") {
                    @exnlist = (@exnlist, "\"java.lang." . $2 . "\"");
                } elsif ($1 eq "stackdelta") {
                    $stackdeltas[$index] = $2;
                } elsif ($1 eq "stackpush") {
                    $stackpushes[$index] = $2;
                }
            }
        }
        @exns[$index] = \@exnlist;

        $index++;
    }
}

print "\n/* declarations */\n", @opdecls, "\n\n";

print "\n/* case statement */\n", @casestmt, "\n\n";

print "\n/* len */\n", join(", ", @lengths), "\n\n";

print "\n/* stackdelta */\n", join(", ", @stackdeltas), "\n\n";

print "\n/* stackpush */\n", join(", ", @stackpushes), "\n\n";

print "\n/* throws */\n";
my(%lines) = ();
my($count) = 0;
my(@k) = ();
foreach $e (@exns) {
    my($line) = "{ " . join(", ", @{$e}) . " }";

    if (!defined($lines{$line})) {
        $lines{$line} = $count;
        @k = (@k, $line);
        $count++;
    }
    print $lines{$line}, ", ";
}
print "\n\n";
foreach $e (@k) {
    print "        ", $e, ",\n";
}
