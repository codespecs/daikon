#!/uns/bin/perl

$listclass = $ARGV[0];
$package = $ARGV[1];
$depfile = $ARGV[2];

if ($listclass eq "") {
    $listclass = "SplitterListForSomething";
}
if ($package eq "") {
    $package = "daikon.split";
}
if ($depfile eq "") {
    $depfile = "depends.lst";
}
$outfile = "$listclass.java";

open(DEPFILE, "$depfile") || die "$depfile: Cannot open file:$!\n!";

while (<DEPFILE>) {
    if (/(\S+)\s*\:\s*(\S.*)$/) {
	$base = $1;
	$_ = $2;
    }
    while (/^\s*((\S+)Splitter\S*)($|\s+\S.*)/) {
	$splitter = $1;
	if ($class eq $2) {
	    $i++;
	    $depends{$class}[$i] = $splitter;
	} else {
	    $class = $2;
	    $i = 0;
	    $depends{$class}[$i] = $splitter;
	}
	$_ = $3;
    }
}

close(DEPFILE);

#  foreach $base (keys %depends) {
#      for ($i = 0;
#  	 $depends{$base}[$i];
#  	 $i++) {
#  	print "$base: $depends{$base}[$i]\n";
#      }
#  }

open(OUTFILE, ">$outfile") || die "$outfile: Cannot open file: $!\n";

print OUTFILE "package $package;\n\n";

print OUTFILE "import daikon.*;\n";
print OUTFILE "import daikon.split.*;\n\n";

print OUTFILE "public class $listclass {\n\n";

print OUTFILE "\tstatic {\n\n";

foreach $base (keys %depends) {
    @splitterlist = ();
    %registered = ();
    foreach $class (keys %depends) {
	@splitterlist = ();
	for ($i = 0;
	     $splitter = $depends{$class}[$i];
	     $i++) {
	    $file = "$splitter\.java";
	    open(SPLITTER, "$file") || die "$file: Cannot open file: $!\n";
	    %registered = ();
	    while (<SPLITTER>) {
		if (/class\s+(\S+)/) {
		    $current_class = $1;
		} elsif (/public String condition/) {
		    $_ = <SPLITTER>;
		    if (/return \"([^\"]+)\"/) {
			if (!$registered{$1}) {
			    push @splitterlist, $current_class;
			    $registered{$1} = 1;
			}
		    }
		}
	    }
	    close(SPLITTER);
	}
	&output_instantiator;
    }
}

print OUTFILE "\t}\n";
print OUTFILE "}\n";

close(OUTFILE);

sub output_instantiator {
    if (!$generated{$class}) {
	if ($splitterlist[0]) {
	    $generated{$class} = 1;
	    print OUTFILE "\t\tSplitterList.put(\"$class\", new Splitter[] {\n";
	    for ($j = 0;
		 $splitterlist[$j];
		 $j++) {
		print OUTFILE "\t\t\tnew $splitterlist[$j]()";
		if ($splitterlist[$j+1]) {
		    print OUTFILE ",\n";
		} else {
		    print OUTFILE "});\n\n";
		}
	    }
	}
    }
}
