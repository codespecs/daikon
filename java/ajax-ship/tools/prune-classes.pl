#!perl -w

%classes = ();

while (<ARGV>) {
    if (/in class ([^:]+):/) {
        $classes{$1} = 1;
    }
}

while (<STDIN>) {
    chop;
    
    if (/\.([^.]+)$/) {
        my($fullname) = $_;
        
        if (defined($classes{$fullname})) {
            print $fullname, "\n";
        }
    }
}
