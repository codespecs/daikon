#!/uns/bin/perl

open(OUTFILE, ">$ARGV[0].conds") || die "Cannot open output file\n";

$brace = 0;

while (<>) {
    chop;
    if ($brace == 0) {
	if (/(class\s+[^\{\s]+)/) {
	    print OUTFILE "\nIn $1:\n";
	} elsif (/(import.*\;)/) {
	    print OUTFILE "\n$1\n";
	}
    }
    if ($brace == 1) {
	if (/\s+(\S+)\s*\(/) {
	    $method = $1;
	    print OUTFILE "\nIn function $method:\n";
	    if (/(^|\s)boolean\s/) {
		$boolean_method = 1;
	    } else {
		$boolean_method = 0;
	    }
	}
    }
    if (/\{/) {
	$brace++;
    }
    if (/\}/) {
	$brace--;
    }
    if (/return\s*(\S.*);$/) {
	$cond = $1;
	if ($cond =~ /^(.*)\s*\?/) {
	    $cond = $1;
	    print OUTFILE $cond;
	    if ($boolean_method) {
		$replace{$method} = $cond;
	    }
	} elsif ($cond =~ /(==|\!=|\<=|=\>|=\<|\>=|\<|\>)/) {
	    print OUTFILE $cond;
	    if ($boolean_method) {
		$replace{$method} = $cond;
	    }
	}
    } elsif (/for\s*\([^\;]*\;\s*([^\;]*)\;/) {
	print OUTFILE "$1\n";
	$in_for = 0;
    } elsif (/for\s*\([^\;]*\;/) {
	$in_for = 1;
    } elsif ($in_for && /\s*([^\;]*)\;/) {
	print OUTFILE "$1\n";
	$in_for = 0;
    } elsif (/(if|while)\s*\((.*)$/) {
	$paren = 0;
	$cond = $2;
	for ($line = $_;
	     $line =~ /\(/;
	     $paren++) {
	    $line =~ s/\(//;
	}
	for ($line = $_;
	     $line =~ /\)/;
	     $paren--) {
	    $line =~ s/\)//;
	}
	if ($paren == 0) {
	    $cond =~ s/^(.*)\)[^\)]*$/$1/;
	}
	print OUTFILE $cond;
	if ($paren == 0) {
	    print OUTFILE "\n";
	}
    } elsif ($paren > 0) {
	if (/\s*(\S[^\)]*)(.*)/) {
	    print OUTFILE " $1";
	    $hoge = $1;
	    $hage = $2;
	    for ($i = 0;
		 $i < $paren - 1;
		 $i++) {
		$hoge = "\($hoge";
	    }
	    $paren = 0;
	    while ($hoge =~ /\(/) {
		$paren++;
		if ($hage =~ /(\)[^\)]*)(.*)/) {
		    $paren--;
		    print OUTFILE "$1";
		    $hage = $2;
		    $hoge = "$hoge$1";
		    $hoge =~ s/\(//;
		}
	    }
	    if ($paren == 0) {
		print OUTFILE "\n";
	    }
	}
    }
}

print OUTFILE "\nreplace:\n";
foreach $method (keys %replace) {
    print OUTFILE "$method : $replace{$method}\n";
}

close(OUTFILE);
