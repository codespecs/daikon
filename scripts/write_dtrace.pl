#!/usr/local/bin/perl
# This perl script is used to insert cluster information into a dtrace file.
# The subroutine "read_cluster_info_xxx" reads one or more files (or takes in 
# some input) and returns an associative array. This associative array has
# as key the program point name, and as value an array whose index is the
# invocation nonce and the value cluster number of that point.

%pptname_to_cluster;
$object_invoc = 0; #assigns invocation number of the OBJECT program point

#substitute this with your own read_cluster_info procedure.
$pptname_to_cluster = &read_cluster_info_seq;

if( @ARGV[0] =~/(.*)\.dtrace/ ){
    $stem = $1;
    $dtrace_file = @ARGV[0];
} else {
    print "First argument must be a dtrace file. Must end in .dtrace";
    exit(0);
}

open (DTRACE_IN, $dtrace_file) || die "dtrace file not found \n";
$newfile = $stem."_new.dtrace";
open (DTRACE_OUT, ">$newfile") 
    || die "couldn't open $newfile for output\n";

while (<DTRACE_IN>) {
    $line = $_;
    if ($line =~ /:::/) {
	$pptname = $line;
	chomp ($pptname);
	&insert_cluster_info($pptname);
    }
}

sub insert_cluster_info {
#assumes that the invocation nonce is always the first "variable" at the
#program point (apart from the OBJECT program point, for which the invocation
#nonce is just the number of times the program point has appeard in this dtrace
#file. Uses the invocation nonce to match the program points and insert the 
#cluster information 
    $pptname = $_[0];
    if ($pptname =~ /OBJECT/) {
	$object_invoc ++;
	$invoc = $object_invoc;
    } else {
	$line = <DTRACE_IN>;
	$invoc = <DTRACE_IN>;
	chomp($invoc);
	if ($line !~ /this.invocation.nonce/) {
	    &skip_till_next(*DTRACE_IN);
	    return;
	}
    }
    # find out if this program point was clustered. If it was, retrieve the 
    # cluster information. Otherwise skip it.
    
    # use only the stem program point name to store and access the cluster information,
    # because the entry and exit with the same invocation number must have the same 
    # cluster number 
    
    $pptstem = $pptname;
    $pptstem = &cleanup_pptname($pptstem);
    $pptstem =~ s/ENTER.*//;
    $pptstem =~ s/EXIT.*//;
    
    $cluster_number = $pptname_to_cluster{$pptstem}[$invoc];
    if($cluster_number == 0){
	&skip_till_next(*DTRACE_IN);
    } else {
	$output = "$pptname\nthis_invocation_nonce\n$invoc\n";
	$output = $output."cluster\n$cluster_number\n1\n";
	print DTRACE_OUT $output;
	&copy_till_next(*DTRACE_IN, *DTRACE_OUT);
    }
    return;
}
	        
sub skip_till_next {
#read an opened file till you reach a blank line, then return    
    local *FHANDLE = $_[0];
    do {
	$line = <FHANDLE>;
    } until ($line =~ /^\s*$/);
    return; 
}

sub copy_till_next {
#copy one file into another, until a blank line is reached.
    *INHANDLE = $_[0];
    *OUTHANDLE = $_[1];
    do {
	$line = <INHANDLE>;
	print OUTHANDLE $line;
    } until ($line =~ /^\s*$/);
    return; 
}

########################## read_cluster_info_xxx ##########
# the return is an associative array. The keys are the program point stems 
# (ie. without :::ENTER or :::EXIT. The value is an array for which the ith 
# index contains the cluster number of the ith invocation of the program point.
# Therefore if QueueAr.isEmpty()Z:::ENTER and QueueAr.isEmpty()Z:::EXIT44 
# invocation nonce 1000 belongs to cluster 3, then the returned associative
# array should have a key "QueueAr.isEmpty()Z" whose value is an array. The
# 1000th element of that array should be 3
##########################################################

sub read_cluster_info_seq {
# @ARGV[1....] are the files with the cluster information
# the file format is just a list of invocation nonces (one per line), grouped
# by cluster. there is a blank line separating clusters.

    @temparray; #holds the cluster information
    $numfiles = scalar(@ARGV) - 1;
    for ($i = 1; $i < $numfiles+1; $i++) {
	$filename = $ARGV[$i];
	open (FILE, $filename) || die "can't open $filename to read cluster info\n";
	$cluster = 1;
	#read the file with the cluster information
	while( $line = <FILE>) {
	    if($line =~ /^\s*$/) {
		$cluster++;
	    } else {
		chomp($line);
		$temparray[$line] = $cluster;
	    }
	}
	$filename =~ s/ENTER.*//;
	$filename =~ s/EXIT.*//;
	$filename =~ s/.cluster//;
	$filename =~ s/.samp//;
	
	$pptname_to_cluster{$filename} = [@temparray];
    }
    return $pptname_to_cluster;
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
