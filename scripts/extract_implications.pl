#!/usr/bin/perl

# This script extracts implications obtained from clustering and writes them
# into a splitter info file. The input file is obtained from printing out 
# Daikon output in the Java output format. All consequents in implications
# of the form
#            (cluster == <num>) ==> <consequent>  or
#            (cluster == <num>) == <consequent>
# are extracted and written into the splitter info file.

# usage: extract_implications.pl <input_file> <derive_conditions>
#        if you want extra conditions to be derived from the conditions in the
#        input file, set <derived_conditions> to 1 (or any number > 1)

$DERIVE_CONDITIONS = 0;
$WARNING = 0;

$inv_file = $ARGV[0];
shift @ARGV;
if ($ARGV[0] == 1) {
    $DERIVE_CONDITIONS = 1;
}

$out = "cluster.spinfo";

open (IN, $inv_file) || die "couldn't open $inv_file\n";
open (OUT, ">$out") || die "couldn't open $out for output\n";

%pptname_to_conds;

while (<IN>) {
    $line = $_;
    chomp($line);
    if ($line =~ /:::/) {
	$pptname = &cleanup_pptname($line);
	chomp($pptname);
	next;
    }
    
    #eliminate unwanted invariants
    if ($line =~ /subsequence/ || $line =~ /elements/ 
	|| $line =~ /in this/ || $line =~ /orig\(.*?\[.*?\].*?\)/
	|| $line =~ /contains no duplicates/ || $line =~ /Format/
	|| $line =~ /\[.*cluster.*\]/ || $line =~ /reverse/ 
	|| $line =~ /implemented/) {
	next;
    }
    
    
    #remove all invariants which involve the cluster variable as the 
    #index of an array eg. someArray[cluster..] etc
    if ($line =~ /\[.*?cluster.*?\]/) {
	next;
    }
    #remove all invariants in which we have an array sequence
    #eg. myArray[0..i]
    if ($line =~ /\[.*\.\..*\]/) {
	next;
    }
    
    #remove predicates depending on cluster. Eg. in the java output,
    #(cluster == 1) ==> (condition) is written "(condition) || !(cluster == 1).
    #In this case, remove "|| !(cluster == 1) and extract only the condition.
    if ($line =~ /\(cluster\s*==/) {
	$line =~ s/\!?\(cluster.*?\)//;
	$line =~ s/^\s*\|\|\s*//; # remove trailing and leading ||
	$line =~ s/\s*\|\|\s*$//;
	if ( $line =~ /\((.*)\)/ ) { #strip off the parentheses at the end.
	    $line = $1;
	}
    } else {
	next;
    }
    
    if ($line =~ /(cluster|===+|class )/) {
	next;
    }
    
    @temparray = @{$pptname_to_conds{$pptname}};
    #remove all duplicates at a program point. However we want
    #to preserve duplicates across program points (or do we?)
    # use hashing!! more efficient
    $duplicate = 0;
    for($i = 0; $i < scalar(@temparray); $i++) {
	if( $line eq $temparray[$i] ) {
	    $duplicate = 1;
	    next;
	}
    }
    if ($duplicate == 0) {
	push @{$pptname_to_conds{$pptname}}, $line;
    }  
}

undef @allconds;

foreach $pptname (keys %pptname_to_conds) {
    
    @conds = @{$pptname_to_conds{$pptname}};
    undef @pptconds_toprint;
    
    foreach $cond (@conds) {
	undef @derived;
	
	if ($DERIVE_CONDITIONS) {
	    @derived = &derive_conditions($cond);
	} else {
	    push @derived, $cond;
	}
	
	#look through all the conditions you've already obtained to see
	#if this already exists. If we are using indiscriminate splitting
	#then we need only one of each condition for all the program 
	#points. However if we are not using indiscriminate splitting,
	#we have to remove this block altogether.
	#======================================
      allconds:       #look through all the conditions we have, to see if we have a duplicate
	foreach $possible (@derived) {
	    $duplicate = 0;
	    $tmp = $possible;
	    $tmp =~ s/\s//g;
	    foreach $exists (@allconds) {
		if($exists eq $tmp) {
		    $duplicate = 1;
		    next allconds;
		}
	    }
	    if ($duplicate == 0) {
		push @pptconds_toprint, $possible;
		#we haven't seen it before. save it
		push @allconds, $tmp;
	    }
	}
	#======================================
    }
    
    if (scalar(@pptconds_toprint) > 0) {
	print OUT "PPT_NAME $pptname\n";
	foreach $cond (@pptconds_toprint) {
	    
	    # remove conditions of the form "int i ....", which 
	    # are extracted from range implications
	    if ($cond =~ /\s*int\s*/) {
		next;
	    }
	    
	    #for now don't use orig variables. can comment
	    #this out if you want it.
	    if ($cond =~ /orig(_|\()/ ) {
		next;
	    }
	    
	    #we made this substitution in &derive_implications
	    while ($cond =~ /orig_.*?_/) {
		$cond =~ s/orig_(.*?)_/orig($1)/;
	    }
	    
	    # we don't want splitting conditions comparing a variable against a 
	    # number, except if the number is -2, -1 , 0, 1, 2, 
	    if ($cond =~ /[=><!]=?.*?(-?\d+)\s*/) { #extract the number
		if ( $1 =~ /^\s*-?(0|1|2)\s*$/) {  #throw away if it's not (-)0,1,2
		    print OUT "$cond\n";
		}
		next;
	    }
	    
	    print OUT "$cond\n";
	}
	print OUT "\n";
    }
}

close IN;
close OUT;

sub cleanup_pptname {
    #clean out the pptname and leave only the stem. Therefore
    #DataStructures.StackAr.<init>(I)V:::ENTER would be cleaned
    #up to DataStructures.StackAr
    $pptname = $_[0];
    $pptname =~ s/<.*>//;
    $pptname =~ s/\(.*\)//;
    $pptname =~ s/EXIT.*//;
    $pptname =~ s/ENTER.*//;
    $pptname =~ s/[VZ]?:::/./;
    $pptname =~ s/Ljava\/lang\/Object;//;
    $pptname =~ s/\.$//;
    return $pptname;
}

# for each possible implication, derive other good implications from it.
# for example, if you have "x > 5", we might be interested in "x < 5" and
# "x == 5" also.
sub derive_conditions {
    $kraw = $_[0];
    undef @result;
    
    #substitute "orig(x) with "orig_x_"
    while($kraw =~ /orig\s*\(.*?\)/) {
	$kraw =~ s/orig\s*\((.*?)\)/orig_$1_/;
    }
    #now, I'm just checking for the simple case of 
    #var1 op var2. eg: a > b
    #ignore more complex ones like (a > b) || (a < c) ...
    #because there are too many derived conditions
    if ($kraw =~ /([^\(\)]*)(==|!=|>=|<=|&&|>|<|\|)([^\(\)]*)/) {
	$left = $1;
	$right = $3;
	$operator = $2;
	
	#keep the signs consistent so that duplicates can be detected.
	$right =~ s/false/true/;
	if ( $operator =~ /[!=]=/ ) {
	    if ($right !~ /(null|true|false)/) {
		push @result, "$left == $right";
		push @result, "$left > $right";
		push @result, "$left < $right";
	    } else {
		push @result, "$left $operator $right";
	    }
	    
	    #fix the problem where daikon is substituting
	    #null with 0.
	    if ($left =~ /return/ && $right =~ /\s*\[0|1]\s*/) {
		push @result , "return == false";
	    }
	    
	} elsif ($operator =~ /[!<>=]=/) {
	    push @result, "$left > $right";
	    push @result, "$left == $right";
	    push @result, "$left < $right";
	} elsif ($operator =~ /[<>]/) {
	    push @result, "$left > $right";
	    push @result, "$left == $right";
	    push @result, "$left < $right";
	} elsif ($operator =~ /(\|\||&&)/) {
	    push @result, "$left && $right";
	    push @result, "$left || $right";
	}
    } else {
	push @result, $kraw;
    }
    return @result;
}
