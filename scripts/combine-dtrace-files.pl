#!/usr/bin/env perl

# combine-dtrace-files.pl
# arguments:  dtrace1 dtrace2 ...
# Outputs combined dtrace file to stdout
# usage:  combine-dtrace-files dtrace1 dtrace2 dtrace3 > dtraces

use English;

my @headers;
my %decls;                  # key is ppt name, value is array of lines
my @values;
my $in_decl = 0;
my $skipping_decl = 0;
my $cur_decl_name = "";

while (<>) {
    if (/^decl-version/) {
        push @headers, $_;
        next;}

    if (/^var-comparability/) {
        push @headers, $_;
        next;}

    if ($in_decl) {             # are we in a ppt declaration?
        if (! $skipping_decl) {
             my $arr = $decls{$cur_decl_name};
             push @$arr, $_;
        }
        if (/^\s*$/) {
            $in_decl = 0;       # a blank line ends a declaration
            $skipping_decl = 0;
        }
        next;
    }
                                # if get here, not in a declaration

    if (/^ppt (.*):::BB/) {     # starting a new declaration
        $in_decl = 1;
        $cur_decl_name = $1;
        if ($decls{$1}) {       # look for duplicate
            print STDERR "duplicate declaration of $1\n";
            $skipping_decl = 1;
        } else {
            $decls{$1} = [$_];  # initialize decl's array of lines
            $skipping_decl = 0;
        }
        next;
    }

    # if get here, must be a value
    push @values, $_;
}

# now print collected info to stdout

foreach my $header (@headers) {
    print "$header";
}

foreach my $decl_name (keys(%decls)) {
    my $arr = $decls{$decl_name};
    foreach my $decl_line (@$arr) {
        print "$decl_line";
    }
}

foreach my $value (@values) {
    print "$value";
}



# eof
