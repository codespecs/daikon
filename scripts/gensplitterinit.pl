#!/usr/bin/perl

use Getopt::Std;

main: {
getopt('cpl');

$listclass = $opt_c;
$package = $opt_p;
$splitterlist = $opt_l;

$listclass = "SplitterListForSomething" if ($listclass eq "");
$package = "daikon.split" if ($package eq "");
$splitterlist = "splitter.lst" if ($splitterlist eq "");
$outfile = "$listclass.java";

&read_splitterlist($splitterlist);

open(OUTFILE, ">$outfile") || die "$outfile: Cannot open file: $!\n";

&generate_target;

print OUTFILE <<EOF;
        }
}
EOF

close(OUTFILE);
}

sub read_splitterlist {
    my ($list) = @_;

    open(LIST, "$list") || die "$list: Cannot open file:$!\n!";

    while (<LIST>) {
	$class = "";
	$index = 0;
	chomp;
	$splitter = $_;
	if (/^(\S+)_(\d+)\.java$/) {
	    $class = $1;
	    $index = $2;
	} elsif (/^(\S+)\.java$/) {
	    $class = $1;
	    $index = 0;
	}
	$class =~ s/Splitter_/\./;
	$splitters{$class}[$index] = $splitter;
    }

# The following awkward routine resolves duplication, which should be
# managed in gensplitter.pl.

    foreach $class (keys %splitters) {
	for ($i = 1;
	     $splitters{"$class$i"} != ();
	     $i++) {
	    $j = $#{$splitters{$class}};
	    foreach $splitter (@{$splitters{"$class$i"}}) {
		$j++;
		$splitters{$class}[$j] = $splitter;
	    }
	    delete $splitters{"$class$i"};
	}
    }

    close(LIST);
}

sub generate_target {

print OUTFILE <<EOF;
package $package;

import daikon.*;
import daikon.split.*;

public class $listclass {

        static {

EOF

    foreach $class (keys %splitters) {
	@splitterlist = ();
	for ($i = 0;
	     $file = $splitters{$class}[$i];
	     $i++) {
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
	&output_instantiator($class, @splitterlist);
    }
}

sub output_instantiator {
    my ($class, @list) = @_;
    if ($list[0]) {
	print OUTFILE "\t\tSplitterList.put(\"$class\", new Splitter[] {\n";
	for ($j = 0;
	     $list[$j];
	     $j++) {
	    print OUTFILE "\t\t\tnew $list[$j]()";
	    if ($list[$j+1]) {
		print OUTFILE ",\n";
	    } else {
		print OUTFILE "});\n\n";
	    }
	}
    }
}
