#!/usr/bin/perl

# This script extracts implications obtained from clustering and writes them
# into a splitter info file. The input file is obtained from printing out 
# Daikon output in the Java output format. All consequents in implications
# of the form
#            (cluster == <num>) ==> <consequent>  or
#            (cluster == <num>) == <consequent>
# are extracted and written into the splitter info file.

my $usage = "extract_implications.pl [--derive-conditions] [-o <output-file>] <input_file>";
# if you want extra conditions to be derived from the conditions in the
# input file, call with the flag --derived_conditions. The default output file is 
# cluster.spinfo

use English;
use strict;
$WARNING = 0;

my $DERIVE_CONDITIONS = 0; # by default, don't derive conditions
my $out;
my $inv_file;
my $cond_cluster_num; # the cluster number of the current condition

while (scalar(@ARGV) > 0) {
    if ($ARGV[0] eq "--derive-conditions") {
	$DERIVE_CONDITIONS = 1;
	shift @ARGV;
    } elsif ($ARGV[0] eq "-o") {
	$out = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } else {
	$inv_file = $ARGV[0];
	@ARGV = ();
    }
}

if ($inv_file =~ /^\s*$/) {
    die "$usage\n";
}


open (IN, $inv_file) || die "couldn't open $inv_file\n";
if (defined($out)) {
    open (OUT, ">$out") || die "couldn't open $out for output\n";
} else {
    open (OUT, ">&1") || die "couldn't open standard output\n";
}

my %pptname_to_conds = ();
my %allconds = ();

my $pptname;
while (<IN>) {
    
    my $line = $_;
    chomp($line);
    if ($line =~ /:::/) {
	$pptname = &cleanup_pptname($line);
	chomp($pptname);
	next;
    } 
    
    #eliminate unwanted invariants and lines
    if ($line =~ /subsequence/ || $line =~ /elements/ 
	|| $line =~ /in this/ || $line =~ /orig/
	|| $line =~ /contains no duplicates/ || $line =~ /Format/
	|| $line =~ /\[.*cluster.*\]/ || $line =~ /reverse/ 
	|| $line =~ /implemented/ || $line =~ /has only one value/
	|| $line =~ /===+/ 
	|| $line =~ /class/) {
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
    
    # remove conditions of the form "int i ....", which 
    # are extracted from range implications
    if ($line =~ /\s*int\s*/) {
	next;
    }
    
    #remove predicates depending on the variable 'cluster'. Eg. in the java output,
    #(cluster == 1) ==> (condition) is written "(condition) || !(cluster == 1).
    #In this case, remove "|| !(cluster == 1) and extract only the condition.
    if ($line =~ /\(cluster\s*==\s*(\d*)/) {
        $cond_cluster_num = $1;
	$line =~ s/\!?\(cluster.*?\)//;
	$line =~ s/^\s*\|\|\s*//; # remove leading ||
	$line =~ s/\s*\|\|\s*$//; # remove trailing ||
	if ( $line =~ /\((.*)\)/ ) { #strip off the parentheses at the end.
	    $line = $1;
	}
	
	
	
    } else {
	#this is an invariant which doesn't involve the cluster implication.
	#discard and read the next line
	next;
    }
    
    #If the line still has cluster in it, then this must be the cluster variable in an
    #invariant, so discard the invariant.
    if ($line =~ /(cluster)/) {
	next;
    }
    
    push @{$pptname_to_conds{$pptname}{$cond_cluster_num}}, $line;
}

my %printed_conds = (); # this stores all the conditions already printed, so that
                        # a condition is not printed more than once
    
foreach my $pptname (keys %pptname_to_conds) {
    my @pptconds_toprint = (); #the conditions to print @ each program point
    
    # get the hashset (cluster_num => @conditions)
    my %cluster_hash = %{$pptname_to_conds{$pptname}};
    
    
    foreach my $cluster_num (keys %cluster_hash) {
	my @conds = @{$cluster_hash{$cluster_num}};
	my %conjunction = (); # a conjunction of all the conditions at the ppt.
	
	foreach my $cond (@conds) {
	    # we don't want splitting conditions comparing a variable against a 
	    # number, except if the number is -2, -1 , 0, 1, 2, 
	    if ($cond =~ /[=><!]=?\D*-?(\d+)/) { #extract the number
		if ($1 !~ /^\s*(0|1|2)\s*$/) {
		    next;
		}
	    }
	    
	    if ($cond !~ /return/) {
		$conjunction{$cond} = 1;
	    }
	    if ($DERIVE_CONDITIONS) {
		my @derived = &derive_conditions($cond);
		@pptconds_toprint = (@pptconds_toprint, @derived);
	    } else {
		push @pptconds_toprint, $cond;
	    }
	}
	
	push @pptconds_toprint, join ( ' && ', keys(%conjunction));
    }
    
    if (scalar(@pptconds_toprint) > 0) {
	print OUT "PPT_NAME $pptname\n";
	foreach my $cond (@pptconds_toprint) {
	    
	    # this is the last line of filtering. If you've already printed
	    # it, don't. (This is the case for indiscriminate splitting: we
	    # need just one of each splitting condition in the entire file.

	    my $hash = $cond; #store it in the global list of conditions
	    $hash =~ s/\s//;
	    if (!exists $printed_conds{$hash}) {
		$printed_conds{$hash} = 1;
		if ($cond !~ /^\s*$/) {
		    print OUT "$cond\n";
		} 
	    }
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

    my $pptname = $_[0];
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
    my ($kraw, @result, $left, $right, $operator);
    $kraw = $_[0];
    @result = ();
    
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
