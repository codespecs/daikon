#!/usr/bin/env perl

# extract_vars.pl - extract variable values from a dtrace file. Writes them
#                  out in one of various input formats for different clustering
#                  algoritm implementations.

# todo: document the daikon_cluster thing for clustering tempfiles.
my $usage = "extract_vars.pl  <dtracefile>.dtrace <decls1>.decls <decls2>.decls ...";

use English;
use strict;
$WARNING = 0;			# "-w" flag

srand(1);
my $decls_file; #holds the name of the .decls file
my $dtrace_file; #holds the name of the .dtrace file
my %nvars_to_maxsamples = ();
my $algorithm = "km";

# in extracting the variables, I want only scalar data. The variables I throw
# away are
# (1) Hashcode of Objects (2) Arrays (3) Strings
my %pptname_to_fhandles = (); #stores a filehandle for each program point.
my %pptname_to_nvars = (); #stores the number of variables at each program point
my %pptname_to_vararrays = (); #stores the variable values at each program point.
# need to keep track of this in case we want to sample it later.
my %pptname_to_objectvars = (); #stores the variable name of all Object variables at
# each program point. This is because we want to throw away this data from
# the clustering.
my %pptname_to_nonces = (); #keep track of invocation nonces for ppts which don't have them.
my %pptname_to_varnames = (); # keep track of the variable names to be clustered.

#tie %pptname_to_varnames, "Tie::IxHash";

#define the output format. Important: Only one of these should be set to 1. Others set to zero
my $output_xmeans = 1;
my $output_seq = 0;
my $output_column = 0;

while ( scalar(@ARGV) > 0) {
    if ($ARGV[0] eq '-algorithm') {
	$algorithm = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ($ARGV[0] =~ /\.decls/){
	$decls_file = $ARGV[0];
	&read_decls_file($decls_file);
	shift @ARGV;
    } elsif ( $ARGV[0] =~/\.dtrace/ ) {
	if (defined($dtrace_file)) {
	    die("extract_vars.pl: multiple dtrace files supplied: $dtrace_file\n$usage");
	}
	$dtrace_file = $ARGV[0];
	shift @ARGV;
    } else {
	die $usage;
    }
}

#for hierarchical clustering, it doesn't really number what the number of 
#variables is, because the difference table is used to do the clustering.

if ($algorithm eq 'hierarchical') {
    #use this for hierarchical because hierarchical can handle fewer data points
    %nvars_to_maxsamples = (1, 1500, 2, 1500, 3, 1500 , 4, 1200, 5, 1000, 6, 800);
} else  {
    #use this for kmeans
    %nvars_to_maxsamples = (1, 6000, 2, 5000, 3, 4000 , 4, 4000, 5, 4000, 6, 4000);
}


sub read_decls_file {
    my ($decls_file, $pptname);
    $decls_file = $_[0];
    open(DECL, $decls_file) || die "decls file not found";
    while(<DECL>) {
	my $line = $_;
	if($line =~ /DECLARE/){
	    $pptname = &read_decl_ppt();
	    #extract the variables out of only the EXIT program points.
	    if ($pptname !~ /ENTER/) { 
		my $pptfilename = &cleanup_pptname($pptname);
		$pptfilename = $pptfilename.".daikon_temp";
		if ($output_seq) {
		    &open_file_for_output_seq($pptname, $pptfilename);
		} elsif ($output_xmeans) {
		    &open_file_for_output_xmeans($pptname, $pptfilename);
		} else {
		    die "No output format defined. Set one of the \$output_xxx variables\n";
		}
	    }
	}
    }
}

open (DTRACE, $dtrace_file) || die "dtrace file not found \n";

while (<DTRACE>) {
    my ($pptname);

    my $line = $_;
    if ($line =~ /:::/) {
	$pptname = $line;
	chomp ($pptname);
	#extract the variables out of only the EXIT program points.
	#(take this out if you want everything). Also if the filehandle
	#doesn't exist, then we didn't come across this program point in
	#the decls file, so ignore
	if( $pptname =~ /:::ENTER/ || !(exists $pptname_to_fhandles{$pptname})) {
	    &skip_till_next(*DTRACE);
	} else {	
	    my @variables = &read_execution($pptname);
	    push @{$pptname_to_vararrays{$pptname}}, @variables;
	    if ($output_seq) {
		&output_seq(@variables);
	    } elsif ($output_xmeans) {
		&output_xmeans(@variables);
	    }
	}
    }
}

#close all open filehandles
foreach my $pptname (keys %pptname_to_fhandles) {
    *FH = $pptname_to_fhandles{$pptname};
    close FH;
}

&sample_large_ppts;

sub sample_large_ppts {
    #can reduce the space constraints by not saving the stuff in
    #memory, but rather reading the trace file again and printing
    #only the sampled invocations
    my ($fhandle, $nsamples, $maxsamples);
    foreach my $pptname (keys %pptname_to_vararrays) {
	
	my $num = $pptname_to_nvars{$pptname};
	if ($algorithm eq 'hierarchical') {
	    if($num > 6 && $num < 10) {
		$nvars_to_maxsamples{$num} = 600;
	    } elsif ($num > 10) {
		$nvars_to_maxsamples{$num} = 550;
	    } 
	} elsif ($algorithm eq 'km') {
	    $nvars_to_maxsamples{$num} = 4000;
	}
    	
	$maxsamples = $nvars_to_maxsamples{$num};
	
	#find the number of samples for this program point
	my $temp = scalar( @{$pptname_to_vararrays{$pptname}} );
	$nsamples = $temp/($num + 2);
	
	#if the number of samples is too large, sample.
	if ($nsamples > $maxsamples) {
	    my @samples = &get_random_numbers($maxsamples, $nsamples);
	    #open a file and print, but first clobber the original one
	    my $pptfilename = &cleanup_pptname($pptname);
	    my $old = "$pptfilename.daikon_temp";
	    unlink $old;
	    my $new_filename = $pptfilename.".daikon_temp.samp";
	    if ($output_seq) {
		&open_file_for_output_seq($pptname, $new_filename);
	    } elsif ($output_xmeans) {
		&open_file_for_output_xmeans($pptname, $new_filename);
	    } else {
		die "no  output format chosen!!\n";
	    }
	    
	    #vararray contains the pptname and the invoc, in addition to
	    # the variable values
	    
	    my $block_size = $pptname_to_nvars{$pptname} + 2;
	    
	    for (my $i = 0; $i < $maxsamples; $i++) {
		my $start = $samples[$i] * $block_size;
		my $end = $start + $block_size - 1;
		
		my @invocation_slice = ();
		
		for (my $j = $start; $j <= $end; $j++) {
		    push @invocation_slice, $pptname_to_vararrays{$pptname}[$j];
		}
		if ($output_seq) {
		    &output_seq(@invocation_slice);
		} elsif ($output_xmeans) {
		    &output_xmeans(@invocation_slice);
		} else {
		    die " no output format chosen!!\n";
		}
	    }
	    close SAMP;
	}
    }
}

sub read_execution {
    my ($varname, $value, $mod);
    
    my @vararray = ();
    my $pptname = $_[0];
    #the pptname is the first element in the array. The variables follow it
    push @vararray, $pptname;
    
    #find out what the Object variables are for this ppt
    my @objarray = $pptname_to_objectvars{$pptname};
    
    $varname = <DTRACE>;
    if ($varname !~ /this.invocation.nonce/) {
	$pptname_to_nonces{$pptname}++;
	push @vararray, $pptname_to_nonces{$pptname};
    } else {
	$value = <DTRACE>;
	chomp($value);
	push @vararray, $value;
	$varname = <DTRACE>;
    }
    
    # get the variables at this ppt that we want to cluster
    
    while ($varname !~ /^$/) {
	chomp( $varname );
	$value = <DTRACE>;
	chomp ($value);
	$value =~ s/false/-10/;
	$value =~ s/true/10/;
	$value =~ s/null/0/;
	$value =~ s/missing/-11111/;
	$mod = <DTRACE>;
	chomp ($mod);
	# see if the variable is an Object variable
	my $object = 0;
	# to fix: use an associative array
	foreach my $variablename (@objarray){
	    if($variablename eq $varname){
		$object = 1;
	    }
	}
	
	# extract variables to be clustered.
	# Omit Object variables, .class, array[] or a string
	if ( $varname !~ /\.class/ && $varname !~ /\[\]/ && $varname !~ /\.toString/) {
	    if ( exists $pptname_to_varnames{$pptname}{$varname}) {
		push @vararray, $value;
	    }
	}
	
	$varname = <DTRACE>;
    }
    return @vararray;
}

sub read_decl_ppt {
    #read the decls file and open up a file for writing the variable values.
    #also count the number of variables we want to cluster at each prog. point
    my (%vararray, $pptname, $line, $varname, $rep_type, $declared_type, $nvars);

    #the pptname is the first element in the array. The variable names follow    
    $line = <DECL>;
    chomp ($line);
    $pptname = $line;


    #now read the variable names and types
    $varname = <DECL>;
    while( $varname !~ /^$/ ) {
	chomp ($varname);
	$declared_type = <DECL>; 
	$rep_type = <DECL>;
	
	#If the variable is an Object, keep note of that. Will be ignored (not be clustered)
	# later because its value is a hashcode
	if ( $varname !~ /\.class/ && $varname !~ /\[\]/ && $varname !~ /\.toString/) {
	    if($rep_type =~ /hashcode/){
		push @{$pptname_to_objectvars{$pptname}}, $varname;
	    } elsif ( $rep_type =~ /=/) {
		#definition. do nothing
	    } else {
		$nvars++;
		$pptname_to_varnames{$pptname}{$varname} = 1;
	    }
	}
	$line = <DECL>; #this is that 22 number thing
	$varname = <DECL>; #this is the next variable name, or a blank line
    }
    #store the number of variables at this program point. Remember that @vararray[1] 
    #stores the program point name. The invocation nonce is included in @vararray, 
    #but is not counted as a variable
    $pptname_to_nvars{$pptname} = $nvars;
    return $pptname;
}

sub open_file_for_output_seq {
    # prep the file for writing the variable values. Now it just prints the number of variables
    # at this program point
    my ($pptname, $pptfilename, @vararray, $fhandle);
    $pptname = @_[0];
    $pptfilename = @_[1];
    #create a filehandle for this program point
    
    my $nvars = $pptname_to_nvars{$pptname};
    if ($nvars > 0) {
	local *FNAME;
	open(FNAME, ">$pptfilename") || die "couldn't open $pptfilename for writing\n";
	$fhandle = *FNAME;
	$pptname_to_fhandles{$pptname} = $fhandle;
	print $fhandle "$nvars\n";
    }
}
    
sub open_file_for_output_xmeans {
    # ** customized for output to the xmeans algorithm
    
    #prep the file for writing the variable values
    my ($pptname, $pptfilename, @vararray, $fhandle);
    
    $pptname = @_[0];
    $pptfilename = @_[1];
    
    #create a filehandle for this program point
    my $nvars = $pptname_to_nvars{$pptname};
    if ($nvars > 0) {
	local *FNAME;
	open(FNAME, ">$pptfilename") || die "couldn't open $pptfilename for writing\n";
	$fhandle = *FNAME;
	$pptname_to_fhandles{$pptname} = $fhandle;
	
	print $fhandle "#Generated by extract_vars.pl\n\n";
	for (my $i = 0; $i < $nvars; $i++) {
	    print $fhandle "x$i ";
	}
	print $fhandle "\n";
    }
}

sub output_xmeans {
    #prints out the variable values in column format, one invocation per line
    # point1 var1 var2 ... varN
    # point2 var1 var2 ... varN
    
    my @output_vararray = @_;
    my $output_pptname = $output_vararray[0];
    my $output_nvars = $pptname_to_nvars{$output_pptname};
    my $output_string;
    local *FH = $pptname_to_fhandles{$output_pptname};
    
    ### ignore the invocation nonce!! (xmeans)
    for (my $k = 1; $k < $output_nvars+1; $k++) {
	$output_string = $output_string.$output_vararray[$k+1]." ";
    }
    chop $output_string;
    print FH "$output_string\n";
}


sub output_seq {
    #prints out the variable values in the following form:
    # vector length (N)
    #point 1 
    #var1
    #var2
    # .
    # .
    # .
    # varN
    #point 2
    #var1
    #var2
    # .
    # .
    # .
    # varN
    #point 3
    # .
    # .
    # .

    my @output_vararray = @_;
    my $output_pptname = $output_vararray[0];
    my $output_nvars = $pptname_to_nvars{$output_pptname};
    local *FH = $pptname_to_fhandles{$output_pptname};
    #print the invoc nonce in addition to the variables.
    for (my $k = 0; $k < $output_nvars+1; $k++) {
	print FH "$output_vararray[$k+1]\n";
    }
}

BEGIN {
    my %cache = ();
    my @unwanted = ("<",">","\\\\","\\/",";","\\(", "\\)");
    sub cleanup_pptname {
	# this subroutine is the same as the one used in both write_dtrace.pl and 
	# and extract_vars.pl. Changing one might affect the other, so change both 
	# if you want them to work together. 
	my ($pptfilename, $pptname, $ret);
	#clean up the pptname, replacing all colons, periods and non-filename
	#characters with underscores
	$pptname = $_[0];
	$ret = $cache{$pptname};
	
	if( $ret eq "" ) {
	    $pptfilename = $pptname;
	    $pptfilename =~ s/:::/./;
	    $pptfilename =~ s/Ljava.lang././;
	    foreach my $token (@unwanted) {
		while ( $pptfilename =~ /$token/) {
		    $pptfilename =~ s/$token//;
		}
	    }
	    $pptfilename =~ s/</./;
	    $pptfilename =~ s/>/./;
	    $pptfilename =~ s/\(\s*(\S+)\s*\)/_$1_/;
	    
	    #replace two or more dots in a row with just one dot
	    while ($pptfilename =~ /\.\.+/) {
		$pptfilename =~ s/\.\.+/\./;
	    }
	    #store it for future reference.
	    $cache{$pptname} = $pptfilename;
	    return $pptfilename;
	} else {
	    return $ret;
	}
    }
}

sub skip_till_next {
#read an opened file till you reach a blank line, then return    
    my ($line);
    local *FHANDLE = $_[0];
    while ($line = <FHANDLE>) {
	if ($line =~ /^\s*$/) {
	    return; 
	}
    }
}

sub get_random_numbers {
    #selects a given number (target) of random numbers between 0 and max.
    # target and max are the zeroth and first arguments of this subroutine
    my $target = $_[0];
    my $max = $_[1];
    my @selected; #keeps track of whether a number has been selected
    my @numbers;  #this array holds the random numbers to be returned.
    my $num_selected = 0;
    my $num_clashes = 0;
    
    while($num_selected < $target){
	my $rand = rand ($max);
	$rand =~ s/\..*$//;  #chop off the decimal
	if($selected[$rand] != 1) {
	    $num_clashes = $num_clashes / 1.5;
	    $num_selected ++;
	    $selected[$rand] = 1;
	    push @numbers, $rand;
	} else {
	    #we've got a clash here. To avoid the program continuing for a very
	    #long time (eg when we want to sample 2000 random numbers out of 2001),
	    #keep track of how many consecutive clashes. If it's more than 500, then
	    #just pick deterministically from 0
	    $num_clashes++;
	    if ($num_clashes > 500) {
		for (my $determ = 0; $num_selected < $target; $determ++) {
		    if ($selected[$determ] != 1) {
			$num_selected++;
			push @numbers, $determ;
		    }
		}
	    }
	}
    }
    return @numbers;
}
