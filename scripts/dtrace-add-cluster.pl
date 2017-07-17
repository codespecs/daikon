#!/usr/bin/env perl

# Insert cluster variables into a dtrace file, as the first variable of
# each program point.
# The arguments are:
#  * filenames containing clustering information about each program point.
#    In particular, each file contains information relating invocations at a
#    particular program point to their cluster number, as produced by a
#    clustering tool.
#  * data trace files (possibly compressed with gzip).
# New (uncompressed) data trace files are written with filenames
# BASENAME_runcluster_temp.dtrace.

use English;
use strict;
$WARNING = 1;			# "-w" flag

use util_daikon;

sub usage() {
  print STDERR
    "Usage: dtrace-add-cluster.pl [OPTIONS] CLUSTER_FILES DTRACE_FILES",
    "\n",
    "Options:\n",
    " -a, --algorithm ALG      ALG specifies a clustering algorithm:\n",
    "                          'km' (for kmeans), 'hierarchical',\n",
    "                          or 'xm' (for xmeans).  The default is xmeans.\n",
    " -log  FILE               write log messages to the file FILE\n",
    ;
}				# usage

# An invocation nonce is globally unique.
# A per-ppt invocation order number runs from 1 (or is it 0?) to the total
#   number of times that particular ppt was encountered.

# PPTNAME -> ARRAY[CLUSTER_NUMBER]
# The invocation nonce is the index into the array.
my %pptname_to_cluster = ();

# Used to keep track of an invocation nonce for ppts that don't have them.
my %pptname_to_nonces = ();

# The highest cluster number.  This is needed when we are using xmeans so
# that we can know how many clusters to split the dtrace file into.
my $maxcluster = 0;

my $algorithm = "xm";
my $logging = 0;
my $logfile;

my @dtrace_files = (); # the data trace files.
# These files contain the cluster information, associating each
# invocation of a program point with a cluster number.  They are
# produced by clustering algorithm implementations.
my @cluster_files = ();

while (scalar(@ARGV) > 0) {
  my $arg = shift @ARGV;
  if ($arg eq '--algorithm' || $arg eq '-a') {
    $algorithm = shift @ARGV;
  } elsif ($arg eq '-log') {
    $logging = 1;
    $logfile = shift @ARGV;
  } elsif ( $arg =~/(.*)\.dtrace/ ) {
    push @dtrace_files, $arg;
  } else {
    push @cluster_files, $arg;
  }
}

my $loghandle; # filehandle to print logging information
if ($logging) {
  my $now_string = localtime(time);
  local *LOG;
  open (LOG, ">>$logfile") || &dieusage("couldn't open $logfile");
  print LOG "\n================== $now_string =====================\n";
  $loghandle = *LOG;
  print "logging to file $logfile\n";
}

if (scalar(@dtrace_files) == 0) {
  die "no dtrace file supplied";
}

# substitute this with your own read_cluster_info procedure.
# read_cluster_info produces an associative array
# PPTNAME -> ARRAY[CLUSTER_NUMBER]
if ($algorithm eq 'hierarchical' || $algorithm eq 'km') {
  %pptname_to_cluster = &read_cluster_info_seq(@cluster_files);
} elsif ($algorithm eq 'xm') {
  %pptname_to_cluster = &read_cluster_info_xm(@cluster_files);
} else {
  &dieusage("unrecognized algorithm ($algorithm) specified");
}

foreach my $dtrace_file (@dtrace_files) {

  # need to run the DtraceNonceDoctor in order in order for
  # xmeans and possibly other clustering methods to work
  system_or_die ("java daikon.tools.DtraceNonceFixer $dtrace_file");
  if (-e ("$dtrace_file" . "_all_fixed")) {
    system_or_die ("mv $dtrace_file" . "_all_fixed $dtrace_file");
  }
  if (-e "dtrace_file" . "_all_fixed.gz") {
    system_or_die ("mv $dtrace_file" . "_all_fixed.gz $dtrace_file");
  }


 if ($dtrace_file =~ /\.gz$/) {
    open (DTRACE_IN, "zcat $dtrace_file |") || &dieusage("couldn't open dtrace file $dtrace_file with zcat");
  } else {
    open (DTRACE_IN, $dtrace_file) || &dieusage("couldn't open dtrace file $dtrace_file");
  }

  $dtrace_file =~ /(.*)\.dtrace/;
  my $newfile = "$1_runcluster_temp.dtrace";

  open (DTRACE_OUT, ">$newfile")
    || die "couldn't open $newfile for output";

  print "Reading from $dtrace_file\n";
  while (<DTRACE_IN>) {
    my $line = $_;
#    print ("$line");
    if ($line =~ /:::/) {
      my $pptname = $line;
      chomp ($pptname);
      &insert_cluster_info($pptname);
    }
  }
}


if ($algorithm eq 'xm') {
  open (MAX, ">runcluster_temp.maxcluster") || die "couldn't open file to output max cluster";
  print MAX "$maxcluster";
  close(MAX);
}

exit();

########################### subroutines ######################

# Read a record from a dtrace file and insert a cluster number if
# appropriate.  Discard the record if that ppt was not clustered.
sub insert_cluster_info ( $ ) {
  my $pptname = $_[0];

  # If the first 'variable' at this ppt execution is not an
  # invocation nonce, then this program point does not have an
  # invocation nonce, so create a nonce for it. The invocation nonce
  # is used to match the entry and exit program points to cluster
  # information
  my $invoc;			# the invocation nonce for this execution.
  my $line = <DTRACE_IN>;
#  print "$line\n";
  if ($line !~ /this.invocation.nonce/) {
    die "No nonces present, and this program adds them incorrectly.";
    $pptname_to_nonces{$pptname}++;
    $invoc = $pptname_to_nonces{$pptname};
  } else {
    $invoc = <DTRACE_IN>;
    chomp($invoc);
    $line = <DTRACE_IN>;

  }

  # Find out if this program point was clustered.  If it was, retrieve the
  # cluster information.  Otherwise skip it.

  # Use only the stem program point name to store and access the cluster
  # information, because the entry and exit with the same invocation number
  # must have the same cluster number.

  my $pptstem = $pptname;
  $pptstem =~ s/:::(ENTER|EXIT).*//;
  $pptstem = &cleanup_pptname($pptstem);

  my $cluster_number = $pptname_to_cluster{$pptstem}[$invoc];
  if ((! defined($cluster_number)) || ($cluster_number == 0)) {
      chomp ($line);
      if (! ($line eq "")) {
	  &skip_till_next(*DTRACE_IN);
      }
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
}				# insert_cluster_info


# read an opened file till you reach a blank line, then return
sub skip_till_next(*) {
     local *FHANDLE = $_[0];
    my $line;
    do {
	$line = <FHANDLE>;
    } until ($line =~ /^\s*$/);
    return;
}				# skip_till_next


# copy one file into another, until a blank line is reached.
sub copy_till_next(**) {
  my $line;
  local (*INHANDLE, *OUTHANDLE) = @_;

  while ($line = <INHANDLE>) {
    print OUTHANDLE $line;
    if ($line =~ /^\s*$/) {
      return;
    }
  }
}				# copy_till_next

########################## read_cluster_info_xxx ##########
# the return is an associative array. The keys are the program point stems
# (i.e., without :::ENTER or :::EXIT). The value is an array for which the ith
# index contains the cluster number of the ith invocation of the program point.
# Therefore if QueueAr.isEmpty()Z:::ENTER and QueueAr.isEmpty()Z:::EXIT44
# invocation nonce 1000 belongs to cluster 3, then the returned associative
# array should have a key "QueueAr.isEmpty()Z" whose value is an array. The
# 1000th element of that array should be 3
##########################################################

sub read_cluster_info_seq ( @ ) {
# @ARGV[1....] are the files with the cluster information.  The file
# format is just a list of invocation nonces (one per line), grouped
# by cluster. There is a blank line separating clusters.
  my @temparray = ();		#holds the cluster information
  my $cluster = 0;
  my @filenames = @_;		# cluster files
  foreach my $filename (@filenames) {
    open (FILE, $filename) || die "can't open $filename to read cluster info";
    $cluster = 1;
    my $blank_line = 0;
    # read the file with the cluster information
    while (my $line = <FILE>) {
      if ($line =~ /^\s*$/) {
	if ($blank_line) {
	  # previous line was a blank, so don't increase the cluster number.
	  next;
	}
	$blank_line = 1;
	# if you hit a blank like, then we've come to the end of one cluster.
	# increase the cluster number by 1
	$cluster++;
      } else {
	$blank_line = 0;
	chomp($line);
	$temparray[$line] = $cluster;
      }
    }
    $filename =~ s/\.ENTER.*//;
    $filename =~ s/\.EXIT.*//;
    $filename =~ s/.cluster//;
    $filename =~ s/.samp//;
    $filename =~ s/.runcluster_temp.*//;

    print "filename=$filename\n";
    $pptname_to_cluster{$filename} = [@temparray];
  }
  return %pptname_to_cluster;
}


sub read_cluster_info_xm(@) {
# @ARGV[1....] are the files with the cluster information. The file
# format is a list of sequence numbers (one per line), grouped by
# cluster. There is a blank line separating clusters. Each invocation
# of a program point is associated with a sequence number, which
# indicates the order in which that invocation appeared in the data
# trace file. A "translation file" (file ending with .trans) contains
# a mapping from sequence number to invocation number. This
# translation is necessary because the xmeans algorithm outputs the
# sequence numbers of the data points it reads, not the invocation
# numbers.

  my @translation_array = ();	# holds the translation information
  my @nonce_to_cluster = ();
  my $cluster = 0;
  my @filenames = @_;		# cluster files
  foreach my $filename (@filenames) {
    my $trans_filename = $filename;
    $trans_filename =~ s/\.cluster/.trans/;
    open (TRANS, "$trans_filename")
      || die "couldn't open translation file ($trans_filename) for $filename";
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

    open (FILE, $filename) || die "can't open $filename to read cluster info";
    $cluster = 1;
    my $blank_line = 0;
    # read the file with the cluster information
    while (my $line = <FILE>) {
      if ($line =~ /^\s*$/) {
	if ($blank_line) {
	  # previous line was a blank, so don't increase the cluster number.
	  next;
	}
	$blank_line = 1;
	# if you hit a blank like, then we've come to the end of one cluster.
	# increase the cluster number by 1
	$cluster++;
      } elsif ($line =~ /^\s*(\d*)\s*$/) {
	$blank_line = 0;
	chomp($line);
	my $nonce = $translation_array[$line];
	$nonce_to_cluster[$nonce] = $cluster;
	# print "$nonce -> $cluster\n";
	if ($cluster > $maxcluster) {
	  $maxcluster = $cluster;
	}
      } else {
	next;
      }
    }
    $filename =~ s/\.ENTER.*//;
    $filename =~ s/\.EXIT.*//;
    $filename =~ s/.cluster//;
    $filename =~ s/.samp//;
    $filename =~ s/.runcluster_temp.*//;

    $pptname_to_cluster{$filename} = [@nonce_to_cluster];
    if ($logging) {
      &log ("$filename  ==> $cluster clusters");
    }
  }
  return %pptname_to_cluster;
}


sub log ( $ ) {
  print $loghandle "$_[0]\n";
}

# die gracefully while printing the usage.
sub dieusage($) {
  if ($_[0] !~ /^\s*$/) {
    print STDERR "$_[0]\n";
  }
  &usage();
  die;
}				# dieusage
