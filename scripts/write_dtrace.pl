#!/usr/bin/env perl

# This perl script is used to insert cluster information into a dtrace file.
# The subroutine "read_cluster_info_xxx" reads one or more files (or takes in
# some input) and returns an associative array. This associative array has
# as key the program point name, and as value an array whose index is the
# invocation nonce and the value cluster number of that point.


# todo: document translation for xm (sequence in dtrace file -> invocation nonce)
use English;
use strict;
$WARNING = 0;			# "-w" flag

my $usage = "write_dtrace.pl [-output <seq|xm>] [-log] <dtrace file> \@decls_files\n";

my %pptname_to_cluster = ();
my ($dtrace_file, $ppt_stem);
my %pptname_to_nonces = (); #used to keep track of an invocation nonce for ppts 
                      #which don't have them.

my $maxcluster = 0; # the highest cluster number. This is needed when we are using
                    # xmeans so that we can know how many clusters to split the dtrace
                    # file into.

my @cluster_files = ();
my $output = "km";
my $loghandle;
my $logging = 1;

if ($logging) {
    my $logfile = "$ENV{HOME}/daikon_logfile";
    my $now_string = localtime(time);
    local *LOG;
    open (LOG, ">>$logfile") || die "couldn't open $logfile\n";
    print LOG "\n==================================== $now_string =====================\n";
    $loghandle = *LOG;
    print "logging to file $logfile\n";
}

while ( scalar(@ARGV) > 0) {
    if ($ARGV[0] eq '-output') {
	$output = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ( $ARGV[0] =~/(.*)\.dtrace/ ){
	if (defined($dtrace_file)) {
	    die("extract_vars.pl: multiple dtrace files supplied: $dtrace_file\n$usage");
	}
	$dtrace_file = $ARGV[0];
	$ppt_stem = $1;
	shift @ARGV;
    } else {
	push @cluster_files, $ARGV[0];
	shift @ARGV;
    }
}

if (!defined($dtrace_file)) {
    die "write_dtrace.pl: no dtrace file supplied \n";
}

#substitute this with your own read_cluster_info procedure.
if ($output eq 'km' || $output eq 'hierarchical') {
    %pptname_to_cluster = &read_cluster_info_seq(@cluster_files);
} elsif ($output eq 'xm') {
    %pptname_to_cluster = &read_cluster_info_xm(@cluster_files);
}

open (DTRACE_IN, $dtrace_file) || die "dtrace file not found \n";
my $newfile = $ppt_stem."_daikon_temp.dtrace";
# print "writing $newfile\n";
open (DTRACE_OUT, ">$newfile")
    || die "couldn't open $newfile for output\n";

while (<DTRACE_IN>) {
    my $line = $_;
    if ($line =~ /:::/) {
	my $pptname = $line;
	chomp ($pptname);
	&insert_cluster_info($pptname);
    } 
}


if ($output eq 'xm') {
    open (MAX, ">daikon_temp.maxcluster") || die "couldn't open file to output max cluster\n";
    print MAX "$maxcluster";
}

sub insert_cluster_info {
#assumes that the invocation nonce is always the first "variable" at
#the program point. It it's not there, then this program point does
#not have an invocation nonce, create a nonce for it. Uses the
#invocation nonce to match the program points and insert the cluster
#information
    my ($pptname, $invoc, $line, $pptstem, $cluster_number);
    $pptname = $_[0];
    
    $line = <DTRACE_IN>;
    if ($line !~ /this.invocation.nonce/) {
	$pptname_to_nonces{$pptname}++;
	$invoc = $pptname_to_nonces{$pptname};
    } else {
	$invoc = <DTRACE_IN>;
	chomp($invoc);
	$line = <DTRACE_IN>;
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
	my $output = "$pptname\nthis_invocation_nonce\n$invoc\n";
	$output = $output."cluster\n$cluster_number\n1\n";
	print DTRACE_OUT $output;
	print DTRACE_OUT $line;
	if ($line =~ /^\s*$/) {
	    # this ppt has no variables.
	    return;
	}
	&copy_till_next(*DTRACE_IN, *DTRACE_OUT);
    }
    return;
}

sub skip_till_next {
#read an opened file till you reach a blank line, then return
    local *FHANDLE = $_[0];
    my ($line);
    do {
	$line = <FHANDLE>;
    } until ($line =~ /^\s*$/);
    return;
}

sub copy_till_next {
#copy one file into another, until a blank line is reached.
    my ($line);
    local *INHANDLE = $_[0];
    local *OUTHANDLE = $_[1];

    while ($line = <INHANDLE>) {
	print OUTHANDLE $line;
	if ($line =~ /^\s*$/) {
	    return;
	}
    }
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
    my ($filename, $line, $cluster, $blank_line);
    my @temparray = (); #holds the cluster information
    $cluster = 0;
    my @filenames = @_; # cluster files
    foreach $filename (@filenames) {	
	open (FILE, $filename) || die "can't open $filename to read cluster info\n";
	$cluster = 1;
	#read the file with the cluster information
	while( $line = <FILE>) {
	    if($line =~ /^\s*$/) {
		if ($blank_line) { #previous line was a blank, so don't increase the cluster number. 
		    next;
		}
		$blank_line = 1;
		#if you hit a blank like, then we've come to the end of one cluster.
		#increase the cluster number by 1
		$cluster++;
	    } else {
		$blank_line = 0;
		chomp($line);
		$temparray[$line] = $cluster;
	    }
	}
	$filename =~ s/ENTER.*//;
	$filename =~ s/EXIT.*//;
	$filename =~ s/.cluster//;
	$filename =~ s/.samp//;
	$filename =~ s/.daikon_temp.*//;
	
	$pptname_to_cluster{$filename} = [@temparray];
    }
    return %pptname_to_cluster;
}

my %pptname_to_nonce_translation = ();

sub read_cluster_info_xm {
# @ARGV[1....] are the files with the cluster information
# the file format is just a list of invocation nonces (one per line), grouped
# by cluster. there is a blank line separating clusters.

    my ($filename, $line, $cluster, $blank_line);
    my @translation_array = (); #holds the translation information
    my @nonce_to_cluster = ();
    $cluster = 0;
    my @filenames = @_; # cluster files
    foreach $filename (@filenames) {
	my $trans_filename = $filename;
	$trans_filename =~ s/.cluster//;
	open (TRANS, "$trans_filename.trans") || die "write_dtrace: couldn't open translation file ($filename.trans) for $filename\n";
	my $sequence_number = 0;
	while (<TRANS>) {
	    if ($_ =~ /^\s*(\d*)\s*$/) {
		my $trans = $1;
		$translation_array[$sequence_number] = $trans;
		$sequence_number++;
	    } else {
		next;
	    }
	}

	open (FILE, $filename) || die "can't open $filename to read cluster info\n";
	$cluster = 1;
	#read the file with the cluster information
	while( $line = <FILE>) {
	    if($line =~ /^\s*$/) {
		if ($blank_line) { #previous line was a blank, so don't increase the cluster number. 
		    next;
		}
		$blank_line = 1;
		#if you hit a blank like, then we've come to the end of one cluster.
		#increase the cluster number by 1
		$cluster++;
	    } elsif ($line =~ /^\s*(\d*)\s*$/) {
		$blank_line = 0;
		chomp($line);
		my $nonce = $translation_array[$line];
		$nonce_to_cluster[$nonce] = $cluster;
		#print "$nonce -> $cluster\n";
		if ($cluster > $maxcluster) {
		    $maxcluster = $cluster;
		}
	    } else {
		next;
	    }
	}
	$filename =~ s/ENTER.*//;
	$filename =~ s/EXIT.*//;
	$filename =~ s/.cluster//;
	$filename =~ s/.samp//;
	$filename =~ s/.daikon_temp.*//;
	
	$pptname_to_cluster{$filename} = [@nonce_to_cluster];
	if ($logging) {
	    &log ("$filename  ==> $cluster clusters");
	}
    }
    return %pptname_to_cluster;
}

BEGIN {
    my %cache = (); #stores the cleaned up program point names
    my @unwanted = ("<",">","\\\\","\\/",";","\\(", "\\)");
    sub cleanup_pptname {
	# this subroutine is the same as the one used in both write_dtrace.pl and
	# and extract_vars.pl. Changing one might affect the other, so change both
	# if you want them to work together.
	
	#clean up the pptname, replacing all colons, periods and non-filename
	#characters with underscores
	my ($pptfilename, $ret, $pptname);
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


sub log {
    my $message = $_[0];
    print $loghandle "$message \n";
}
