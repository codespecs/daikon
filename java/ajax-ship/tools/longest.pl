#!perl5 -w

my($longest) = "";
while (<ARGV>) {
    if (length($_) > length($longest)) {
        $longest = $_;
    }
}

print $longest;
