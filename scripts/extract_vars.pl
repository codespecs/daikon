#!/usr/local/bin/perl

$decls_file; #holds the name of the .decls file
$dtrace_file; #holds the name of the .dtrace file

# in extracting the variables, I want only scalar data. The variables I throw
# away are
# (1) Hashcode of Objects (2) Arrays (3) Strings  
%pptname_to_fhandles; #stores a filehandle for each program point.
%pptname_to_nvars; #stores the number of variables at each program point
%pptname_to_vararrays; #stores the variable values at each program point.
#need to keep track of this in case we want to sample it later.
%pptname_to_objectvars; #stores the variable name of all Object variables at 
# each program point. This is because we want to throw away this data from 
# the clustering.

#for hierarchical clustering, it doesn't really number what the number of 
#variables is, because the difference table is used to do the clustering.
%nvars_to_maxsamples = (2, 5000, 3, 4000 , 4, 4000, 5, 4000, 6, 4000);
  
if(@ARGV[0] =~ /\.decls/){
    $decls_file = @ARGV[0];
    $dtrace_file = @ARGV[1];
}elsif( @ARGV[0] =~/\.dtrace/ ){
    $dtrace_file = @ARGV[0];
    $decls_file = @ARGV[1];
}

$dtrace_file =~ /\/*(.*)\.dtrace/;

open(DECL, $decls_file) || die "decls file not found";
local $pptname;
while(<DECL>) {
    $line = $_;
    if($line =~ /DECLARE/){
	@variables = &read_decl_ppt;
	#extract the variables out of only the EXIT program points.
	#(take this out if you want everything)
	$pptname = @variables[0];
	if ($pptname !~ /ENTER/) {
	    &open_file_for_output_seq(@variables);
	    #&open_file_for_output_column(@variables);
	}
    }
}

open (DTRACE, $dtrace_file) || die "dtrace file not found \n";
while (<DTRACE>) {
    local $pptname, @variables;
    
    $line = $_;
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
	    @variables = &read_dtrace($pptname);
	    push @{ $pptname_to_vararrays{$pptname}}, @variables;
	    &output_seq(@variables);
	    #&output_column(@variables);
	}
    }
}

#close all open filehandles
foreach $fhandle (keys %pptname_to_fhandles) {
    *FH = $pptname_to_fhandles{$pptname};
    close FH;
}

&sample_large_ppts;

sub sample_large_ppts {
    #can reduce the space constraints by not saving the stuff in
    #memory, but rather reading the trace file again and printing
    #only the sampled invocations

    local $pptname, $i, $j, $pptfilename;
    
    
    foreach $pptname (keys %pptname_to_vararrays) {
	
	local $nvars = $pptname_to_nvars{$pptname};
	if($nvars > 6 && $nvars < 10) {
	    $nvars_to_maxsamples{$nvars} = 1000;
	} elsif ($nvars > 10) {
	    $nvars_to_maxsample{$nvars} = 800;
	}
	
	$maxsamples = $nvars_to_maxsamples{$nvars};
	
	#find the number of samples for this program point
	$temp = scalar( @{$pptname_to_vararrays{$pptname}} );
	$nsamples = $temp/($nvars + 2);
			
	#if the number of samples is too large, sample.
	if ($nsamples > $maxsamples) {
	    print "sampling ... $pptname\n";
	    $sampling = 1;
	    @samples = &get_random_numbers($maxsamples, $nsamples);
	    
	    #open a file and print, but first clobber the original one
	    $pptfilename = &cleanup_pptname($pptname);
	    #unlink $pptfilename;
	    $new_filename = $pptfilename.".samp";
	    open (SAMP, ">$new_filename") 
		|| die "couldn't open $new_filename to print sampled output\n";
	    $pptname_to_fhandles{$pptname} = *SAMP;
	    
	    #print out the selected invocations from the vararray
	    #print SAMP "#number of variables is $nvars\n";
	    print SAMP "$nvars\n";
	    
	    #vararray contains the pptname and the invoc, in addition to
	    # the variable values
	    $block_size = $pptname_to_nvars{$pptname} + 2;
	    	    
	    for ($i = 0; $i < $maxsamples; $i++) {
		$start = $samples[$i] * $block_size;
		$end = $start + $block_size - 1; 
		$invocation_slice = "";
		$count = 0;
		for ($j = $start; $j <= $end; $j++) {
		    $invocation_slice[$count] = $pptname_to_vararrays{$pptname}[$j];
		    $count ++;
		}
		&output_seq(@invocation_slice);
		#&output_column(@invocation_slice);
	    }
	}
	close SAMP;
    }
}

#the object program point doesn't have invocation nonces.
$object_invoc = 0; #keep track of OBJECT invoc. nonces
$class_invoc = 0; #keep track of CLASS invoc nonces
sub read_dtrace {
    local @vararray;
    $pptname = $_[0];
    #the pptname is the first element in the array. The variables follow it
    push @vararray, $pptname;
    
    #create an invocation nonce for the object program point invocation
    if($pptname =~ /OBJECT/) {
	$object_invoc ++;
	push @vararray, $object_invoc;
    } elsif ($pptname =~ /CLASS/) {
	$class_invoc ++;
	push @vararray, $class_invoc;
    }
    
    #find out what the Object variables are for this ppt
    @objarray = @{$pptname_to_objectvars{$pptname}};
    
    $varname = <DTRACE>;
    while ($varname !~ /^$/) {
	chomp( $varname );
	if ($varname =~ /this.invocation.nonce/) {
	    #the nonce should be stored as an extra variable
	    $value = <DTRACE>;
	    chomp ($value);
	    push @vararray, $value;
	} else {
	    $value = <DTRACE>;
	    chomp ($value);
	    $value =~ s/false/-10/;
	    $value =~ s/true/10/;
	    $value =~ s/null/0/;
	    $value =~ s/missing/-11111/;
	    $mod = <DTRACE>;
	    chomp ($mod);
	    # see if the variable is an Object variable
	    $object = 0;
	    foreach $variablename (@objarray){
		if($variablename eq $varname){
		    $object = 1;
		}
	    }
	    # extract variables to be clustered.
	    # Omit Object variables, .class, array[] or a string
	    if($varname !~ /\.class/ && $varname !~ /\[\]/ && $varname !~ /\.toString/ && $object == 0) {
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
    local $line;
    local @vararray, $pptname;
    
    $line = <DECL>;
    chomp ($line);
    $pptname = $line;
    #the pptname is the first element in the array. The variable names follow
    push @vararray, $pptname;
    push @vararray, "this_invocation_nonce";
    
    #now read the variable names and types
    $varname = <DECL>;
    while( $varname !~ /^$/ ) {
	chomp ($varname);
	$declared_type = <DECL>; 
	$rep_type = <DECL>;
	
	#If the variable is an Object, keep note of that. Will be ignored (not be clustered)
	# later because its value is a hashcode
	if ( $varname !~ /\.class/ && $varname !~ /\[\]/ && $varname !~ /\.toString/) {
	    if($declared_type =~ /Object/ && $rep_type =~ /hashcode/){
		push @{$pptname_to_objectvars{$pptname}}, $varname;
	    } elsif ( $rep_type =~ /=/) {
		#definition. do nothing
	    } else {
		push @vararray, $varname;
	    }
	}
	$line = <DECL>; #this is that 22 number thing
	$varname = <DECL>; #this is the next variable name, or a blank line
    }
    #store the number of variables at this program point. Remember that @vararray[1] 
    #stores the program point name. The invocation nonce is included in @vararray, 
    #and is counted as a variable
    $nvars = scalar(@vararray) - 2;
    $pptname_to_nvars{$pptname} = $nvars;
    return @vararray;
}

sub open_file_for_output_seq {
    #prep the file for writing the variable values
    local $pptfilename, $m, @vararray;
    
    @vararray = @_;
    $pptfilename = $vararray[0];
    $pptfilename = &cleanup_pptname($pptfilename);
    
    #create a filehandle for this program point
    local *FNAME;
    open(FNAME, ">$pptfilename") || die "couldn't open $pptfilename for writing\n";
    $fhandle = *FNAME;
    
    $pptname = $vararray[0];
    $pptname_to_fhandles{$pptname} = $fhandle;
    $nvars = $pptname_to_nvars{$pptname};
    
    for($m = 0, $tempstring = ""; $m < $nvars; $m++) {
	$varname = $vararray[$m+2]; #0 and 1 hold pptname and invoc.
	$tempstring = $tempstring."$varname\t";
    }
    #print $fhandle "#$tempstring\n";
    #print $fhandle "#number of variables is $nvars\n";
    print $fhandle "$nvars\n";
}

sub open_file_for_output_column {
    #prep the file for writing the variable values
    local $pptfilename, $m, @vararray;
    
    @vararray = @_;
    $pptfilename = $vararray[0];
    $pptfilename = &cleanup_pptname($pptfilename);
    
    #create a filehandle for this program point
    local *FNAME;
    open(FNAME, ">$pptfilename") || die "couldn't open $pptfilename for writing\n";
    $fhandle = *FNAME;
    
    $pptname = $vararray[0];
    $pptname_to_fhandles{$pptname} = $fhandle;
}

sub output_column {
    #prints out the variable values in column format, one invocation per line
    # point1 var1 var2 ... varN
    # point2 var1 var2 ... varN
    
    local $pptname, $k, $fhandle, @vararray;
    @vararray = @_;
    $pptname = $vararray[0];
    $nvars = $pptname_to_nvars{$pptname};
    $fhandle = $pptname_to_fhandles{$pptname};
    
    #print the invoc nonce in addition to the variables.
    for ($k = 0; $k < $nvars+1; $k++) {
	print $fhandle "$vararray[$k+1]\t";
    }
    print $fhandle "\n";
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
    
    
    local $pptname, $k, $fhandle, @vararray;
    @vararray = @_;
    $pptname = $vararray[0];
    $nvars = $pptname_to_nvars{$pptname};
    $fhandle = $pptname_to_fhandles{$pptname};
    
    #if ($sampling) {
    #print "pptname $pptname, fhandle $fhandle, $vararray[1], $vararray[2], k = $k\n";
    #}
    
    #print the invoc nonce in addition to the variables.
    for ($k = 0; $k < $nvars+1; $k++) {
	print $fhandle "$vararray[$k+1]\n";
    }
}

sub cleanup_pptname {
# this subroutine is the same as the one used in both write_dtrace.pl and 
# and extract_vars.pl. Changing one might affect the other, so change both 
# if you want them to work together. 

    %cache;
    #clean up the pptname, replacing all colons, periods and non-filename
    #characters with underscores
    $pptname = $_[0];
    $ret = $cache{$pptname};
    
    if( $ret eq "" ) {
	$pptfilename = $pptname;
	$pptfilename =~ s/:::/./;
	$pptfilename =~ s/Ljava.lang././;
	@unwanted = ("<",">","\\\\","\\/",";","\\(", "\\)");
	foreach $token (@unwanted) {
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

sub skip_till_next {
#read an opened file till you reach a blank line, then return    
    local *FHANDLE = $_[0];
    do {
	$line = <FHANDLE>;
    } until ($line =~ /^\s*$/);
    return; 
}

sub get_random_numbers {
    #selects a given number (target) of random numbers between 0 and max.
    # target and max are the zeroth and first arguments of this subroutine
    $target = $_[0];
    $max = $_[1];
    @selected = ""; #keeps track of whether a number has been selected
    @numbers = "";  #this array holds the random numbers to be returned.
    $num_selected = 0;
    $num_clashes = 0;
    while($num_selected < $target){
	$rand = rand ($max);
	$invoc++;
	$rand =~ s/\..*$//;  #chop of the decimal
	if($selected[$rand] != 1) {
	    $num_clashes = $num_clashes / 2;
	    $num_selected ++;
	    $selected[$rand] = 1;
	    push @numbers, $rand;
	} else {
	    #we've got a clash here. To avoid the program continuing for a very
	    #long time (eg when we want to sample 2000 random numbers out of 2001),
	    #keep track of how many consecutive clashes. If it's more than 500, then
	    #just pick deterministically from 0
	    $num_clashes++;
	    if ($num_clashes > 400) {
		for ($determ = 0; $num_selected < $target; $determ++) {
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
