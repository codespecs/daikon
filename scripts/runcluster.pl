#!/usr/bin/env perl

# Performs clustering on a Daikon data trace.
# Input is a dtrace and a decls file.
# Output is "cluster.spinfo" (actually "cluster-$algorithm-$ncluster.spinfo").

use English;
use strict;
$WARNING = 1;                    # -w flag

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);

# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

sub usage() {
  print STDERR
    "Usage: runcluster.pl [OPTIONS] DTRACE_FILES [DECLS_FILES]",
    "\n",
    "If no DECLS_FILES are given, the declarations in DTRACE_FILES are used.\n",
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

my $SCRIPTDIR = dirname (__FILE__);

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
if ($algorithm eq "xm") {
  if (system("xmeans 2>&1 > /dev/null") != 0) {
    die "Could not run the 'xmeans' binary.\n"
      . "Download it from http://www.cs.cmu.edu/~dpelleg/kmeans.html\n"
      . "or choose a different clustering algorithm.\n";
  }
}

###########################################################################
### Processing
###

#remove files from a previous run that might have aborted...
&remove_temporary_files();

# Make the invocation nonces consistent, and give every sample a nonce.
# The input files are left unchanged; the fixed copies are temporary files
# in the current directory, which are read by both extract_vars.pl and
# dtrace-add-cluster.pl so that both see the same nonces.
if ($verbose) { print "\n# Fixing invocation nonces ...\n"; }
my @fixed_trace_files = ();
foreach my $trace_file (@trace_files) {
  my $gz = ($trace_file =~ /\.gz$/) ? ".gz" : "";
  my $fixed = basename($trace_file);
  $fixed =~ s/\.dtrace(\.gz)?$//;
  $fixed .= "_runcluster_temp_nonces.dtrace$gz";
  system_or_die("java -cp $SCRIPTDIR/../daikon.jar daikon.tools.DtraceNonceFixer $trace_file $fixed", $verbose);
  push @fixed_trace_files, $fixed;
}
my $dtrace_files = join(' ', @fixed_trace_files);
# If no decls files were given, the declarations are in the dtrace files.
my $decls_files = (scalar(@decls_files) == 0) ? $dtrace_files : join(' ', @decls_files);

#extract the variables from the dtrace file
if ($verbose) { print "\n# Extracting variables from dtrace file ...\n"; }
my $command = "$SCRIPTDIR/extract_vars.pl --algorithm $algorithm $decls_files $dtrace_files";
if ($verbose) { print "$command\n"; }
if (system($command) != 0) {
  # extract_vars.pl's error message names a temporary file, not an input file.
  my $copies = "";
  for (my $i = 0; $i < scalar(@trace_files); $i++) {
    $copies .= "  $fixed_trace_files[$i] is a copy of $trace_files[$i]\n";
  }
  die "Failed executing $command\n"
    . "Its dtrace files are copies of the input files, with fixed invocation nonces:\n$copies";
}

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
$command = "$SCRIPTDIR/dtrace-add-cluster.pl --algorithm $algorithm -log dtrace-add-cluster.log $dtrace_files " . join(' ', @clustered_files);
system_or_die($command, $verbose);

# Rewrite the decls file to include the cluster information.
$command = "$SCRIPTDIR/decls-add-cluster.pl $decls_files";
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

my $conditions = "";
for (my $i = 1; $i <= $ncluster; $i++) {
  $conditions .= "cluster == $i\n";
}
# Split each program point that was clustered.
my @spinfo_ppts = ();
open (PPTS, "runcluster_temp.clustered_ppts") || die "file with clustered ppts not found\n";
while (my $ppt = <PPTS>) {
  chomp($ppt);
  push @spinfo_ppts, $ppt;
}
close PPTS;
foreach my $ppt (@spinfo_ppts) {
  print SPINFO "PPT_NAME $ppt\n$conditions\n";
}
close SPINFO;

###
### Run daikon with cluster spinfo file and new dtrace and decls files.
###

if ($verbose) { print "\n# Running daikon with cluster spinfo file ...\n"; }

my @new_dtraces = ();
foreach my $dtrace_file (@fixed_trace_files) {
  $dtrace_file =~ /(.*)\.dtrace/;
  push @new_dtraces , "$1_runcluster_temp.dtrace";
}

my $invfile = "runcluster_temp_$algorithm-$ncluster.inv";
$command = "java -cp $SCRIPTDIR/../daikon.jar -Xmx7g daikon.Daikon -o $invfile --config_option daikon.PptTopLevel.pairwise_implications=true --var-omit-pattern=\"class\" --no_text_output --no_show_progress $spinfo_file $decls_new " . join(' ', @new_dtraces) . " 2>&1 > runcluster_temp_Daikon_output.txt";
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
$command = "java -cp $SCRIPTDIR/../daikon.jar daikon.tools.ExtractConsequent $invfile > $outfile";
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
