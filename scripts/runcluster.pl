#!/usr/bin/env perl

# performs a clustering of a Daikon data trace to produce a cluster info file out
# of the useful clusters. Input is a dtrace and a decls file. Output is "cluster.spinfo"

use English;
use strict;

$WARNING = 0;                    # -w flag


my $ncluster = 4; # the number of clusters
my $algorithm = "km";
my $usage = "Usage: runcluster.pl [-k <num_clusters>] [-algorithm (hierarchical | km)] <dtrace_files> <decls_files>";

my $command;


my ($dtrace_file, $decls_file);   # the dtrace and decls files

while (scalar(@ARGV) > 0) {
    print scalar(@ARGV);
    if ($ARGV[0] eq '-k') {
	$ncluster = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ($ARGV[0] eq '-algorithm') {
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
$command = "perl $ENV{INV}/scripts/extract_vars.pl $decls_file $dtrace_file";
print "$command\n";
system($command);

$command = "";
#this is a hack. careful!
my @exit_ppts = glob("*\\.EXIT*");
my @object_ppts = glob("*\\.OBJECT*");
my @to_cluster = (@exit_ppts, @object_ppts);

foreach my $filename (@to_cluster) {
    my $outfile = "$filename.cluster";
    #this is for kmeans clustering
    if ($algorithm eq "km") {
	$command = $command . " $ENV{INV}/tools/kmeans/kmeans $filename $ncluster > $outfile; ";
    } else {
	#this is for hierarchical clustering
	$command = $command . " difftbl $filename | cluster -w | clgroup -n $ncluster > $outfile; ";
    }
}

#perform the clustering
print "\nperforming clustering ....\n";
print "$command\n";
system($command);

#rewrite dtrace file
print "\nrewriting dtrace file ...\n";
my @clustered_files = glob("*\\.cluster");
$command = "perl $ENV{INV}/scripts/write_dtrace.pl $dtrace_file " . join(' ', @clustered_files);
print "$command\n";
system($command);

#rewrite .decls file to add cluster info
my $decls_new = $decls_file;
#remove everything from the pathname except the filename itself.
if ($decls_new =~ /\//) {
    $decls_new =~ s/.*\///;
}

$decls_new =~ s/\.decls/_new\.decls/;
# print "$decls_new will be the new decls file\n";

$command = "perl $ENV{INV}/scripts/decls-add-cluster.pl  $decls_file $decls_new";
print "\nrewriting .decls files ($decls_file -> $decls_new) to include cluster info...\n";
print "$command\n";
system($command);
# print "decls-add-cluster finished\n";

#write spinfo file
my $spinfo_file = "temp.spinfo";
print "\nwriting spinfo file $spinfo_file ...\n";
open (SPINFO, ">$spinfo_file") || die "couldn't write cluster spinfo file temp.spinfo\n";
my $spinfostring  = "PPT_NAME OBJECT\ncluster == 1 \ncluster == 2 \ncluster == 3 \ncluster == 4 \ncluster == 5";
print SPINFO $spinfostring;
close SPINFO;

#run daikon with cluster spinfo file and cluster dtrace file.
$dtrace_file =~ /(.*)\.dtrace/;
my $new_dtrace = "$1_new.dtrace";
my $invfile = "$algorithm-$ncluster.inv";
$command = "java -Xmx512m daikon.Daikon -o $invfile --no_text_output --suppress_redundant --suppress_post $spinfo_file $decls_new $new_dtrace";
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
  unlink_glob("*\\.EXIT*");
  unlink_glob("*\\.OBJECT*");
  unlink_glob("*\\.CLASS\\.*");
  unlink_glob("*_new.dtrace*");
  unlink_glob("*_new.decls");
  unlink("temp.spinfo");
}

