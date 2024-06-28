#!/usr/bin/env perl
# createspinfo.pl

# Usage:
#   createspinfo.pl file.java
# This creates "file.java.spinfo", a splitter info file that can be
# provided to Daikon on the command line in order to enable detection of
# implications (conditional invariants).

# This script creates the splitter info file in two stages.  First, it
# creates a .conds file, and then it creates the splitter info file from
# the .conds file.

die("Do not use createspinfo.pl; instead, use CreateSpinfo.java");

use English;
# use strict;
$WARNING = 1;			# "-w" flag


#creation of .conds file
foreach my $filename (@ARGV) {
    open(INPUTFILE, $filename) || die "Could not open input file $filename";
    open(CONDFILE, ">$filename.conds") || die "Cannot open output file\n";
    my $brace = 0;
    my $in_if = 0;

    my $method;
    my $args;
    my $cond;
    my $replace_method;
    my %replace;
    my %arguments;

    # Parsing state
    my $in_for; # a boolean
    my $paren; # a numeric depth

    while (<INPUTFILE>) {

        # start: skip commented lines
	my $line = $_;
	chop $line;
	if ($line =~ /^\s*\/\//) {
	    next;
	}
	if ($line =~ /^\s*\/\*/) {
	    if ($line =~ /\*\//) {
		next;
	    } else {
		while (($line = <INPUTFILE>) !~ /\*\//) {
		    next;
		}
	    }
	}
        # end: skip commented lines

	if ($brace == 0) {
	    if ($line =~ /(class\s+[^\{\s]+)/) {
		print CONDFILE "\nIn $1:\n";
	    }
	}
	if ($brace == 1) {
	    if ($line =~ /\s+(\S+)\s*(\(.*\))/) {
		#we've matched a function name.
		$method = $1;
		$args = $2;
		print CONDFILE "\nIn function $method:\n";
		$replace_method = 1;
	    }
	}
	if ($line =~ /\{/) {
	    $brace++;
	}
	if ($line =~ /\}/) {
	    $brace--;
	}
	if ($line =~ /return\s*(\S.*);$/) {
	    $cond = $1;
	    my $repl = $1;
	    if ($cond =~ /^\((.*)\s*\?/) {
		$cond = $1;
		print CONDFILE $cond."\n";
		if ($replace_method) {
		    if ($cond =~/^\(([^\(\)]*)\)$/ ) {
			#strip off any extra brackets around the expression
			$cond = $1;
		    }
		    $replace{$method} = $repl;
		    $arguments{$method} = $args;
		}
	    }elsif ($cond =~ /(==|\!=|\<=|=\>|=\<|\>=|\<|\>|&)/) {
		print CONDFILE $cond."\n";
		if ($replace_method) {
		    if ($cond =~/^\(([^\(\)]*)\)$/ ) {
			#strip off any extra brackets around the expression
			$cond = $1;
		    }
		    $replace{$method} = $cond;
		    $arguments{$method} = $args;
		}
	    }
	}
	#//// trying to find declaration of boolean variables
	if ($line =~ /\s+boolean\s+([^\(\)\{\}]*)$/) {
	    #found boolean variable(s). Check for declaration of multiple booleans
	    my @boolean_declarations = split /;/,$1;
	    foreach my $declaration (@boolean_declarations) {
		my @booleans = split /,/,$declaration;
		foreach my $boo (@booleans) {
		    if ($boo =~ /\s*(.*)\s*=\s*(.*)/) {
			$boo = $1;
		    }
		    if ($boo !~ /^\s*$/) {
			print CONDFILE $boo." == true \n";
			$replace_method = 0;
		    }
		}
	    }
	}elsif ($line =~ /\((.*)boolean\s*([^,\)]*)/) {
	    #looking for a boolean argument to a function
	    my $remainder = $1;
	    print CONDFILE $2." == true \n";
	    while ($remainder =~ /(.*)boolean\s*([^,\)]*)/) {
		print CONDFILE $2." == true \n";
		$remainder = $1;
	    }
	}
	#/// end boolean variables

	if ($line =~ /for\s*\([^\;]*\;\s*([^\;]*)\;/) {
	    print CONDFILE "$1\n";
	    $replace_method = 0;
	    $in_for = 0;
	} elsif ($line =~ /for\s*\([^\;]*\;/) {
	    $in_for = 1;
	} elsif ($in_for && ($line =~ /\s*([^\;]*)\;/)) {
	    print CONDFILE "$1\n";
	    $replace_method = 0;
	    $in_for = 0;
	} elsif (($line =~ /(if|while)\s*\((.*)$/) || $in_if) {
	    #multiline expression in the if condition => $in_if = 1
	    if ( $in_if == 0) {
		$paren = 0;
		$cond = $2;
	    }
	    for (my $arg = $line;
		 $arg =~ /\(/;
		 $paren++) {
		$arg =~ s/\(//;
	    }
	    for (my $arg = $line;
		 $arg =~ /\)/;
		 $paren--) {
		$arg =~ s/\)//;
	    }
	    if ($paren == 0 &&  $in_if == 0) {
		$cond =~ s/^(.*)\)[^\)]*$/$1/;
		print CONDFILE $cond."\n";
		#Do the replacements only on simple one-line methods which return booleans.
		#If it's a complicated function ie. more then one line, discard.
		$replace_method = 0;
		$in_if = 0;
	    }elsif ($paren == 0 && $in_if == 1) {
		$in_if = 0;
		$line =~ s/\s*(.*)\s*\{/$1/;
		$cond = $cond.$line;
		print CONDFILE $cond."\n";
		$replace_method = 0;
	    } else {
		$in_if = 1;
		if ($line !~ /(if|while)/) {
		    $line =~ s/\s*(.*)\s*/$1/;
		    $cond = $cond.$line;
		}
	    }
	} elsif ($paren > 0) {
	    if ($line =~ /\s*(\S[^\)]*)(.*)/) {
		print CONDFILE " $1";
		$replace_method = 0;
		my $hoge = $1;
		my $hage = $2;
		for (my $i = 0;
		     $i < $paren - 1;
		     $i++) {
		    $hoge = "\($hoge";
		}
		$paren = 0;
		while ($hoge =~ /\(/) {
		    $paren++;
		    if ($hage =~ /(\)[^\)]*)(.*)/) {
			$paren--;
			print CONDFILE "$1";
			$hage = $2;
			$hoge = "$hoge$1";
			$hoge =~ s/\(//;
		    }
		}
		if ($paren == 0) {
		    print CONDFILE "\n";
		}
	    }
	}
    }

    print CONDFILE "\nreplace:\n";
    foreach my $method2 (keys %replace) {
	print CONDFILE "$method$arguments{$method2} : $replace{$method2}\n";
    }

    close(CONDFILE);
#.conds file created

    open(CONDFILE, "$filename.conds") || die "could not open conditions file";
    $filename =~ s/\.java//;
    open(SPINFOFILE, ">$filename.spinfo") || die "could not write to file $filename.spinfo";
#structure: (Hash of Arrays) of the form:
# %HashOfConds = [
#                 Class1.Function1 => (cond, cond, ...)
#                 ...
#                ]

    my @subs = ();
    my %HashOfConds = ();
    # %HashOfSubs = ();

    my $function = "OBJECT";

    my $class;

    while (my $line = <CONDFILE>) {
	if ($line =~ /In class\s+(\S*):/) {
	    #matches the class name
	    $class = $1;
	}
	if ($line =~ /In function\s+(\S*):/) {
	    #matches the function name
	    $function = $1;
	}
	if ($line =~ /replace:/) {
	    # A "replace:" line. Since these occur at the end
	    # of the file, read till the end.
	    while ($line = <CONDFILE>) {
		if ($line =~ /(\S.*) : (\S*.*)/) {
		    push (@subs, $1);
		    push (@subs, $2);
		}
	    }
	}
	if ($line !~ /In |replace:|^[ \t]*$|import/) {
	    #not a blank line, and doesn't contain "replace:" or "In "
	    # or "import" so it must be a condition
	    my $key = $class.".".$function;
	    chomp($line);

	    push @{ $HashOfConds{$key}}, $line;
	}
    }

# have to deal with inner classes in here

    # @functions = (keys %HashOfConds);
    # $numfuncs = scalar(@functions);

    {
	my $replace = "REPLACE";
	for (my $i = 0; $i < scalar(@subs); $i = $i + 2) {
	    my $sub = $subs[$i];
	    $sub =~ s/(\(|\?|\))/$1/;
	    my $replace_expr = $subs[$i+1];
	    $replace_expr =~ s/\s*return\s*(.*)\s*;\s*$/ $1/;
	    $replace = $replace."\n".$sub."\n".$replace_expr;
	}
	if (scalar(@subs)> 0) {
	    print SPINFOFILE $replace."\n\n";
	}
    }

    foreach my $function (keys %HashOfConds) {
	my $conditions = "";
	print SPINFOFILE "PPT_NAME  $function";
	# my $replace = "REPLACE";
	my @conds = @{$HashOfConds{$function}};
	$function =~ /^(\S*)\./;
	$class = $1;
	foreach my $cond2 (@conds) {
	    if ($cond2 !~ /^\s*true\s*$/) {
		$cond2 =~ s/^\s*(.*)\s*$/$1/; #strip whitespace from ends
		$cond2 =~ s/^\s*(\S*\s*)\!=(\s*\S*)\s*$/$1==$2/;
		$conditions = $conditions."\n".$cond2;
	    } else { next; }
	}

	print SPINFOFILE $conditions."\n";
	print SPINFOFILE "\n";
	#unlink <$filename.conds>;
    }

    print "output written to $filename.spinfo \n";
    close CONDFILE;
    close SPINFOFILE;
    unlink("$filename.conds");
    }
