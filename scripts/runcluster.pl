#!/usr/bin/env perl

# performs a clustering of a Daikon data trace to produce a cluster info file out
# of the useful clusters. Input is a dtrace and a decls file. Output is "cluster.spinfo"

use English;
use strict;

$WARNING = 0;                    # -w flag


my $ncluster = 4; # the number of clusters
my $algorithm = "km";
my $usage = "Usage: runcluster.pl [-k <num_clusters>] [-algorithm (hierarchical | km)] <dtrace_files> <decls_files>";

my $options;
my $command;


my ($dtrace_file, $decls_file);   # the dtrace and decls files

while (scalar(@ARGV) > 0) {
    if ($ARGV[0] eq '-k') {
	$ncluster = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ($ARGV[0] =~ /-alg/) {
	$algorithm = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ($ARGV[0] =~ /\.decls/){
	$decls_file = $decls_file." $ARGV[0]";
	shift @ARGV;
    } elsif ( $ARGV[0] =~/\.dtrace/ ) {
	$dtrace_file = $dtrace_file." $ARGV[0]";
	shift @ARGV;
    } else {
	die $usage;
    }
}

print "runcluster.pl: -k $ncluster -algorithm $algorithm $decls_file $dtrace_file\n";

#remove files from a previous run that might have aborted...
&remove_temporary_files();

#extract the variables from the dtrace file
print "\nextracting variables from dtrace file ...\n";

if ($algorithm eq 'xm') {
    $options = " -output xm ";
}

$command = "perl $ENV{INV}/scripts/extract_vars.pl $options $decls_file $dtrace_file";
print "$command\n";
system($command);

$command = "";
my @to_cluster = glob("*\\.daikon_temp *.daikon_temp.samp");

foreach my $filename (@to_cluster) {
    my $outfile = "$filename.cluster";
    #this is for kmeans clustering
    if ($algorithm eq "km") {
	$command = $command . " $ENV{INV}/tools/kmeans/kmeans $filename $ncluster > $outfile; ";
    } elsif ($algorithm eq "hierarchical") {
	#this is for hierarchical clustering
	$command = $command . " difftbl $filename | cluster -w | clgroup -n $ncluster > $outfile; ";
    } elsif ($algorithm eq "xm") {
	my $exec = "$ENV{INV}/tools/xmeans/kmeans/kmeans ";
	$command = $command. " $exec makeuni in  $filename; ";
	$command = $command. " $exec kmeans -k 1 -method blacklist -max_leaf_size 45 -min_box_width 0.04 -cutoff_factor 0.5 -max_iter 180 -num_splits 6 -max_ctrs 15 -in $filename -printclusters out.clust; ";
	$command = $command. " $exec membership in out.clust > $outfile ; rm out.clust ;";
    }
}

#perform the clustering
print "\nperforming clustering ....\n";
print "$command\n";
system($command);

#rewrite dtrace file
print "\nrewriting dtrace file ...\n";
my @clustered_files = glob("*\\.cluster");
$command = "perl $ENV{INV}/scripts/write_dtrace.pl $options $dtrace_file " . join(' ', @clustered_files);
print "$command\n";
system($command);

$command = "perl $ENV{INV}/scripts/decls-add-cluster.pl $decls_file";
print "\nrewriting .decls files to include cluster variable...\n";
print "$command\n";
my @new_decls = `$command 2>/dev/null`;  
my $decls_new = join (' ', @new_decls);

# Since the number of clusters for xmeans varies, we have to find the max number
# of clusters it found for all the program points, so we can create a .spinfo
# file to split on all the clusters.
if ($algorithm eq 'xm') {
    open (MAX, "daikon_temp.maxcluster") || die "file with max clusters (xmeans) not found\n";
    $ncluster = <MAX>;
    close MAX;
}

#write spinfo file
my $spinfo_file = "daikon_temp.spinfo";
print "\nwriting spinfo file $spinfo_file ...\n";
open (SPINFO, ">$spinfo_file") || die "couldn't write cluster spinfo file daikon_temp.spinfo\n";

my $spinfostring = "PPT_NAME OBJECT\n";
for (my $i = 1; $i <= $ncluster; $i++) {
    $spinfostring  = $spinfostring."cluster == $i\n";
}
print SPINFO $spinfostring;
close SPINFO;

#run daikon with cluster spinfo file and cluster dtrace file.
$dtrace_file =~ /(.*)\.dtrace/;
my $new_dtrace = "$1_daikon_temp.dtrace";
my $invfile = "$algorithm-$ncluster.inv";
$command = "java -Xmx1024m daikon.Daikon -o $invfile --no_text_output --suppress_redundant --suppress_post $spinfo_file $new_dtrace $decls_new";
print "$command\n";
system($command);

#print out the invariants
$invfile =~ /(.*)\.inv/;
my $textout = $1;
$command = "java daikon.PrintInvariants --java_output $invfile > $textout";
print "\n$command\n";
system($command);

#clean up results
$command = "$ENV{INV}/scripts/extract_implications.pl -o cluster-$algorithm-$ncluster.spinfo $textout";
print "\n$command\n";
system($command);
unlink($textout);

#remove all temporary files
&remove_temporary_files();

###########################################################################
### Subroutines
###

sub unlink_glob ( $ ) {
  my ($glob) = @_;
  my @list = glob($glob);
  foreach my $f (@list) {
    # print "removing $f\n";
    unlink $f;
  }
}

sub remove_temporary_files () {
  unlink_glob("*daikon_temp*");
}
