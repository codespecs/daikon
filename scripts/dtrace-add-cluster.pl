#!/usr/bin/env perl

# Insert cluster variables into a dtrace file, as the first variable of
# each program point.
# The arguments are:
#  * filenames containing clustering information about each program point.
#    In particular, each file contains information relating invocations at a
#    particular program point to their cluster number, as produced by a
#    clustering tool.
#  * data trace files (possibly compressed with gzip).  Every sample must
#    have a consistent invocation nonce; daikon.tools.DtraceNonceFixer
#    produces such a file.  These must be the same data trace files that
#    were given to extract_vars.pl.
# New (uncompressed) data trace files are written with filenames
# BASENAME_runcluster_temp.dtrace.

use English;
use strict;
$WARNING = 1;			# "-w" flag

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);

# The file `util_daikon.pm` appears in the same directory as this script.
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

# PPTNAME -> HASH[INVOCATION_NONCE -> CLUSTER_NUMBER]
# A hash, not an array, because the nonces of all the trace files are
# distinct, so the nonces of one program point can be large and sparse.
# The key is a program point stem (without :::ENTER or :::EXIT...).  Several
# cluster files can have the same stem, such as the files for
# Foo.bar():::EXIT12 and Foo.bar():::EXIT15; their entries are merged by
# add_cluster_info.
my %pptname_to_cluster = ();

# PPTNAME -> NUM:  the highest cluster number used so far for the stem.
my %pptname_to_maxcluster = ();

# The highest cluster number, over all program points.  It tells
# runcluster.pl how many clusters to split each program point into.
my $maxcluster = 0;

# The names of the program points that have at least one clustered sample.
# They are written to file runcluster_temp.clustered_ppts, one per line.
my %clustered_ppts = ();

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
    if ($line =~ /^ppt /) {
      # A program point declaration; the decls file provides the declarations.
      &skip_till_next(*DTRACE_IN);
    } elsif ($line =~ /:::/) {
      my $pptname = $line;
      chomp ($pptname);
      &insert_cluster_info($pptname);
    }
  }
}


open (PPTS, ">runcluster_temp.clustered_ppts") || die "couldn't open file to output clustered ppts";
foreach my $ppt (sort keys %clustered_ppts) {
  print PPTS "$ppt\n";
}
close(PPTS);

# Cluster numbers can exceed the number of clusters requested of a
# clustering tool (see add_cluster_info), so write the maximum for every
# algorithm.
open (MAX, ">runcluster_temp.maxcluster") || die "couldn't open file to output max cluster";
print MAX "$maxcluster";
close(MAX);

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
    die "No invocation nonce for a sample at $pptname; run daikon.tools.DtraceNonceFixer first";
  }
  $invoc = <DTRACE_IN>;
  chomp($invoc);
  $line = <DTRACE_IN>;

  # Find out if this program point was clustered.  If it was, retrieve the
  # cluster information.  Otherwise skip it.

  # Use only the stem program point name to store and access the cluster
  # information, because the entry and exit with the same invocation number
  # must have the same cluster number.

  my $pptstem = $pptname;
  $pptstem =~ s/:::(ENTER|EXIT).*//;
  $pptstem = &cleanup_pptname($pptstem);

  my $cluster_number = $pptname_to_cluster{$pptstem}{$invoc};
  if ((! defined($cluster_number)) || ($cluster_number == 0)) {
      chomp ($line);
      if (! ($line eq "")) {
	  &skip_till_next(*DTRACE_IN);
      }
  } else {
    $clustered_ppts{$pptname} = 1;
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
    while (my $line = <FHANDLE>) {
	if ($line =~ /^\s*$/) {
	    return;
	}
    }
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
# (i.e., without :::ENTER or :::EXIT). The value is a reference to a hash that
# maps each invocation nonce of the program point to its cluster number.
# Therefore if QueueAr.isEmpty()Z:::ENTER and QueueAr.isEmpty()Z:::EXIT44
# invocation nonce 1000 belongs to cluster 3, then the returned associative
# array should have a key "QueueAr.isEmpty()Z" whose value maps 1000 to 3.
##########################################################

sub read_cluster_info_seq ( @ ) {
# @ARGV[1....] are the files with the cluster information.  The file
# format is just a list of invocation nonces (one per line), grouped
# by cluster. There is a blank line separating clusters.
  my $cluster = 0;
  my @filenames = @_;		# cluster files
  foreach my $filename (@filenames) {
    my %nonce_to_cluster = ();	# the cluster information for this file
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
	$nonce_to_cluster{$line} = $cluster;
      }
    }
    $filename =~ s/\.ENTER.*//;
    $filename =~ s/\.EXIT.*//;
    $filename =~ s/\.cluster$//;
    $filename =~ s/\.samp$//;
    $filename =~ s/\.runcluster_temp.*//;

    print "filename=$filename\n";
    &add_cluster_info($filename, \%nonce_to_cluster);
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

  my $cluster = 0;
  my @filenames = @_;		# cluster files
  foreach my $filename (@filenames) {
    my @translation_array = ();	# the translation information for this file
    my %nonce_to_cluster = ();	# the cluster information for this file
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
	$nonce_to_cluster{$nonce} = $cluster;
	# print "$nonce -> $cluster\n";
      } else {
	next;
      }
    }
    $filename =~ s/\.ENTER.*//;
    $filename =~ s/\.EXIT.*//;
    $filename =~ s/\.cluster$//;
    $filename =~ s/\.samp$//;
    $filename =~ s/\.runcluster_temp.*//;

    my $nclusters = &add_cluster_info($filename, \%nonce_to_cluster);
    if ($logging) {
      &log ("$filename  ==> $nclusters clusters");
    }
  }
  return %pptname_to_cluster;
}


# Merges the cluster information read from one cluster file into
# %pptname_to_cluster.  The arguments are the program point stem and a
# reference to a hash from invocation nonce to file-local cluster number.
# Returns the number of clusters in the file.
#
# Every cluster file for a stem (for instance, one per numbered exit point)
# numbers its clusters from 1, but its clusters are unrelated to those of
# the stem's other files.  The ENTER program point receives the samples of
# all of them, so the clusters must stay distinct:  a file's clusters are
# renumbered to follow those of the stem's earlier files.
#
# Each invocation leaves through one exit point, so normally a nonce
# appears in only one file for a stem.  If a nonce appears in more than one
# (for instance, if both a combined :::EXIT and a numbered :::EXITnn point
# were clustered), its first cluster number is kept, so that all the
# records of the invocation have the same cluster number.
sub add_cluster_info ( $$ ) {
  my ($stem, $file_nonce_to_cluster) = @_;
  if (! exists $pptname_to_cluster{$stem}) {
    $pptname_to_cluster{$stem} = {};
    $pptname_to_maxcluster{$stem} = 0;
  }
  my $stem_nonce_to_cluster = $pptname_to_cluster{$stem};
  my $offset = $pptname_to_maxcluster{$stem};
  my $file_maxcluster = 0;
  my $nconflicts = 0;
  foreach my $nonce (keys %{$file_nonce_to_cluster}) {
    my $file_cluster = $file_nonce_to_cluster->{$nonce};
    if ($file_cluster > $file_maxcluster) {
      $file_maxcluster = $file_cluster;
    }
    if (exists $stem_nonce_to_cluster->{$nonce}) {
      $nconflicts++;
    } else {
      $stem_nonce_to_cluster->{$nonce} = $file_cluster + $offset;
    }
  }
  if ($nconflicts > 0) {
    print STDERR "Warning: $nconflicts invocations of $stem appear in more than one cluster file; using the first cluster number for each\n";
  }
  $pptname_to_maxcluster{$stem} = $offset + $file_maxcluster;
  if ($pptname_to_maxcluster{$stem} > $maxcluster) {
    $maxcluster = $pptname_to_maxcluster{$stem};
  }
  return $file_maxcluster;
}				# add_cluster_info

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
