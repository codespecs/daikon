#!/usr/bin/env perl

# Performs clustering on a Daikon data trace.
# Input is a dtrace and a decls file.
# Output is "cluster.spinfo" (actually "cluster-$algorithm-$ncluster.spinfo").

use English;
use strict;
$WARNING = 1;                    # -w flag

use util_daikon;

sub usage() {
  print STDERR
    "Usage: runcluster.pl [OPTIONS] DTRACE_FILES DECLS_FILES",
    "\n",
    "Options:\n",
    " -a, --algorithm ALG\n",
    "       ALG specifies an implementation of a clustering algorithm.\n",
    "       Current options are 'km' (for kmeans), 'hierarchical',\n",
    "       and 'xm' (for xmeans). Default is xmeans.\n",
    " -k,   The number of clusters to use (for algorithms which require\n",
    "       this input.) The default is 4\n",
    " --keep\n",
    "       Don't delete the temporary files created by the clustering\n",
    "       process\n",
    " --verbose\n",
    "       Show progress, subcommands executed, etc.\n",
    ;
} #usage

###########################################################################
### Variables
###

my $ncluster = 4; # the number of clusters
my $algorithm = "xm";
my $keep_tempfiles = 0;
my $verbose = 0;
my @trace_files; # the dtrace files to be clustered
my @decls_files ; # the decls files

###########################################################################
### Process command-line arguments
###

while (scalar(@ARGV) > 0) {
  my $arg = shift @ARGV;
  if ($arg eq '-k') {
    $ncluster = shift @ARGV;
  } elsif ($arg eq '-a' || $arg eq '--algorithm') {
    $algorithm = shift @ARGV;
  } elsif ($arg eq '--keep') {
    $keep_tempfiles = 1;
  } elsif ($arg eq '--verbose') {
    $verbose = 1;
  } elsif ($arg =~ /\.decls/) {
    push @decls_files, $arg;
  } elsif ($arg =~/\.dtrace/ ) {
    push @trace_files, $arg;
  } else {
    &dieusage("Unrecognized argument \"$arg\"");
  }
}
if (scalar(@trace_files) == 0) {
  &dieusage("No trace files specified");
}
if (scalar(@decls_files) == 0) {
  &dieusage("No decls files specified");
}
if ($algorithm eq "xm") {
  if (system("xmeans 2>&1 > /dev/null") != 0) {
    die "Could not run the 'xmeans' binary.\n"
      . "Download it from http://www.cs.cmu.edu/~dpelleg/kmeans.html\n"
      . "or choose a different clustering algorithm.\n";
  }
}

my $dtrace_files = join(' ', @trace_files);
my $decls_files = join(' ', @decls_files);

###########################################################################
### Processing
###

#remove files from a previous run that might have aborted...
&remove_temporary_files();

#extract the variables from the dtrace file
if ($verbose) { print "\n# Extracting variables from dtrace file ...\n"; }
my $command = "extract_vars.pl --algorithm $algorithm $decls_files $dtrace_files";
system_or_die($command, $verbose);

###
### Perform clustering
###

if ($verbose) { print "\n# Performing clustering ....\n"; }
my @to_cluster = glob("*\\.runcluster_temp *.runcluster_temp.samp");
if (scalar(@to_cluster) == 0) {
  die "Nothing to cluster found";
}
foreach my $filename (@to_cluster) {
  my $outfile = "$filename.cluster";
  my $command;
  if ($algorithm eq "km") {
    # kmeans clustering
    $command = "kmeans $filename $ncluster > $outfile";
    system_or_die($command, $verbose);
  } elsif ($algorithm eq "hierarchical") {
    # hierarchical clustering
    $command = "difftbl $filename | cluster -w | clgroup -n $ncluster > $outfile";
    system_or_die($command, $verbose);
  } elsif ($algorithm eq "xm") {
    # xmeans clustering

    # filter out data that isn't a number
    open (FILE ,  "$filename");
    open (OUT, ">$filename.new");
    my @lines = <FILE>;

    foreach my $line (@lines) {

        $line =~ s/uninit/0/g;
        $line =~ s/nan/10000/g;
	print OUT "$line";
    }
    close OUT;
    close FILE;
    system_or_die ("mv $filename.new $filename", $verbose);
    #end filter


    $command = "xmeans makeuni in $filename > xmeans-output-runcluster_temp-$filename-makeuni";
    system_or_die($command, $verbose);
    $command = "xmeans kmeans -k 1 -method blacklist -max_leaf_size 40 -min_box_width 0.03 -cutoff_factor 0.5 -max_iter 200 -num_splits 6 -max_ctrs 15 -in $filename -printclusters out.clust > xmeans-output-runcluster_temp-$filename-kmeans";
    system_or_die($command, $verbose);
    $command = "xmeans membership in out.clust > $outfile";
    if (! $verbose) {
      $command .= " 2>xmeans-output-runcluster_temp-$filename-membership";
    }
    system_or_die($command, $verbose);
    unlink("out.clust");
  } else {
    &dieusage("unknown algorithm $algorithm");
  }
}

###
### Rewrite decls and dtrace files
###

# Rewrite the dtrace file to include the cluster information.
if ($verbose) { print "\n# Rewriting dtrace file ...\n"; }
my @clustered_files = glob("*\\.cluster");
$command = "dtrace-add-cluster.pl --algorithm $algorithm -log dtrace-add-cluster.log $dtrace_files " . join(' ', @clustered_files);
system_or_die($command, $verbose);

# Rewrite the decls file to include the cluster information.
$command = "decls-add-cluster.pl $decls_files";
if ($verbose) { print "\n# Rewriting .decls files to include cluster variable...\n"; }
my $decls_new = backticks_or_die("$command 2> output-decls-add-cluster-runcluster_temp", $verbose);
if ($decls_new eq "") {
  die "No decls output by decls-add-cluster.pl";
}

# Since the number of clusters for xmeans varies, we have to find the max
# number of clusters it found for all the program points, so we can create
# a .spinfo file to split on all the clusters.

if ($algorithm eq 'xm') {
  open (MAX, "runcluster_temp.maxcluster") || die "file with max clusters (xmeans) not found\n";
  $ncluster = <MAX>;
  close MAX;
}

###
### Write temporary intermediate cluster spinfo file
###

my $spinfo_file = "runcluster_temp.spinfo";
if ($verbose) { print "\n# Writing spinfo file $spinfo_file ...\n"; }
open (SPINFO, ">$spinfo_file") || die "couldn't write cluster spinfo file runcluster_temp.spinfo\n";

my $spinfostring = "PPT_NAME OBJECT\n";
for (my $i = 1; $i <= $ncluster; $i++) {
  $spinfostring  = $spinfostring."cluster == $i\n";
}
print SPINFO $spinfostring;
close SPINFO;

###
### Run daikon with cluster spinfo file and new dtrace and decls files.
###

if ($verbose) { print "\n# Running daikon with cluster spinfo file ...\n"; }

my @new_dtraces = ();
foreach my $dtrace_file (@trace_files) {
  $dtrace_file =~ /(.*)\.dtrace/;
  push @new_dtraces , "$1_runcluster_temp.dtrace";
}

my $invfile = "runcluster_temp_$algorithm-$ncluster.inv";
$command = "java -Xmx3600m daikon.Daikon -o $invfile --config_option daikon.PptTopLevel.pairwise_implications=true --var_omit_pattern=\"class\" --no_text_output --no_show_progress $spinfo_file $decls_new " . join(' ', @new_dtraces) . " 2>&1 > runcluster_temp_Daikon_output.txt";
system_or_die($command, $verbose);

$invfile =~ /(.*)\.inv/;

####################
##print out the invariants
#my $textout = $1;
#$command = "java daikon.PrintInvariants --suppress_redundant --java_output $invfile > $textout";
#print "\n$command\n";
#system_or_die($command);
####################

###
### Create final .spinfo file
###

if ($verbose) { print "\n# Creating final .spinfo file\n"; }
my $outfile;
if ($algorithm eq 'xm') {
  $outfile = "cluster-$algorithm.spinfo";
} else {
  $outfile = "cluster-$algorithm-$ncluster.spinfo";
}
$command = "java daikon.tools.ExtractConsequent $invfile > $outfile";
system_or_die($command, $verbose);

#remove all temporary files
if (! $keep_tempfiles ) {
  &remove_temporary_files();
}

exit();

################# Subroutines ####################################

sub unlink_glob ( $ ) {
  my ($glob) = @_;
  my @list = glob($glob);
  foreach my $f (@list) {
    # print "removing $f\n";
    unlink $f;
  }
} #unlink_glob

sub remove_temporary_files () {
  unlink_glob("*cluster_temp*");
} #remove_temporary_files

sub dieusage ( $ ) {
  my ($msg) = @_;
  if ($msg !~ /^\s*$/) {
    $msg =~ s/([^\n])\z/$1\n/;	# add newline if not present
    print STDERR "$msg\n";
  }
  &usage();
  die;
} #dieusage
