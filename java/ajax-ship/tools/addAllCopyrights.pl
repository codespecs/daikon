#!/usr/local/bin/perl -w

use File::Find;

my($copyright) =
"/*
Copyright (c) Robert O'Callahan <roc\@cs.cmu.edu> and Carnegie Mellon University
*/

";

sub addCopyright {
    my($f) = @_;
    
    open(FILE, "<$f");
    my(@lines) = <FILE>;
    my($start) = 0;
    my($preamble) = "";
    while (defined($lines[$start]) && !($lines[$start] =~ /^package/)) {
        $preamble = $preamble . $lines[$start];
        $start++;
    }

    if (!defined($lines[$start])) {
        print "File $f is not a valid Java file\n";
    } elsif ($preamble ne $copyright) {
        print "Updating copyright for $f...\n";
        open(FILE, ">$f");
        print FILE $copyright;

        do {
            print FILE $lines[$start];
            $start++;
        } while (defined($lines[$start]));
    }
}

sub wanted {
    if (/\.java$/) {
        my($filename) = $_;
        
        addCopyright($filename);
        $_ = $filename;
    }
}

find(\&wanted, 'ajax');
