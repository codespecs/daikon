#!/usr/bin/env perl

# performs a clustering of a Daikon data trace to produce a cluster info file out
# of the useful clusters. Input is a dtrace and a decls file. Output is "cluster.spinfo"

use English;
use strict;

$WARNING = 0;                    # -w flag

sub usage() {
    print STDERR
	"Usage: extract_vars.pl [OPTIONS] DTRACE_FILES DECL_FILES",
	"\n",
	"Options:\n",
	" -a, --algorithm ALG\n",    
	"       ALG specifies an implemtation of a clustering algorithm.\n",
	"       Current options are 'km' (for kmeans), 'hierarchical'\n",
	"       and 'xm' (for xmeans). Default is xmeans.\n",
	" -k,   The number of clusters to use (for algorithms which require\n",
	"       this input.) The default is 4\n",
	" --keep\n",
	"       Don't delete the temporary files created by the clustering\n",
	"       process\n",
	;
} #usage

my $ncluster = 4; # the number of clusters
my $algorithm = "xm";
my $keep_tempfiles = 0;
my @trace_files; # the dtrace files to be clustered
my @dcl_files ; # the decls files

while (scalar(@ARGV) > 0) {
    if ($ARGV[0] eq '-k') {
	$ncluster = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ($ARGV[0] eq '-a' || $ARGV[0] eq '--algorithm') {
	$algorithm = $ARGV[1];
	shift @ARGV;
	shift @ARGV;
    } elsif ($ARGV[0] =~ /\.decls/){
	push @dcl_files, $ARGV[0];
	shift @ARGV;
    } elsif ( $ARGV[0] =~/\.dtrace/ ) {
	push @trace_files, $ARGV[0];
	shift @ARGV;
    } elsif ( $ARGV[0] eq '--keep') {
	$keep_tempfiles = 1;
	shift @ARGV;
    } else {
	&dieusage();
    }
}

print "runcluster.pl @ARGV\n";

my $dtrace_files;
if (defined(@trace_files)) {
    $dtrace_files = join(' ', @trace_files);
} else {
    &dieusage();
}

my $decls_files;
if (defined(@dcl_files)) {
    $decls_files = join(' ', @dcl_files);
} else {
    &dieusage();
}

#remove files from a previous run that might have aborted...
&remove_temporary_files();

#extract the variables from the dtrace file
print "\nextracting variables from dtrace file ...\n";
my $command = "perl $ENV{INV}/scripts/extract_vars.pl --algorithm $algorithm $decls_files $dtrace_files";
print "$command\n";
system($command);

#perform the clustering ...
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
	$command = $command. " $exec kmeans -k 1 -method blacklist -max_leaf_size 40 -min_box_width 0.03 -cutoff_factor 0.5 -max_iter 200 -num_splits 6 -max_ctrs 15 -in $filename -printclusters out.clust; ";
	$command = $command. " $exec membership in out.clust > $outfile ; rm out.clust ;";
    } else {
	&dieusage ("unknown algorithm $algorithm\n");
    }
}

print "\nperforming clustering ....\n";
print "$command\n";
system($command);

#rewrite the dtrace file to include the cluster information ...
print "\nrewriting dtrace file ...\n";
my @clustered_files = glob("*\\.cluster");
$command = "perl $ENV{INV}/scripts/write_dtrace.pl --algorithm $algorithm $dtrace_files " . join(' ', @clustered_files);
print "$command\n";
system($command);

#rewrite the decls file to include the cluster information ...
$command = "perl $ENV{INV}/scripts/decls-add-cluster.pl $decls_files";
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

#run daikon with cluster spinfo file and new dtrace and decls files.
my @new_dtraces = ();
foreach my $dtrace_file (@trace_files) {
    $dtrace_file =~ /(.*)\.dtrace/;
    push @new_dtraces , "$1_daikon_temp.dtrace";
}

my $invfile = "$algorithm-$ncluster.inv";
$command = "java -Xmx1024m daikon.Daikon -o $invfile --config $ENV{INV}/java/daikon/config/config.txt --no_text_output $spinfo_file $decls_new ". join(' ', @new_dtraces);
print "$command\n";
system($command);

$invfile =~ /(.*)\.inv/;

####################
##print out the invariants
#my $textout = $1;
#$command = "java daikon.PrintInvariants --suppress_redundant --java_output $invfile > $textout";
#print "\n$command\n";
#system($command);

##clean up results
#$command = "$ENV{INV}/scripts/extract_implications.pl -o cluster-$algorithm-$ncluster.spinfo $textout";
#print "\n$command\n";
#system($command);
#unlink($textout);
####################

$command = "java daikon.ExtractClusterInvariant $invfile > cluster-$algorithm-$ncluster.spinfo";
system($command);

#remove all temporary files
if (! $keep_tempfiles ) {
    &remove_temporary_files();
}

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
  unlink_glob("*daikon_temp*");
} #remove_temporary_files

sub dieusage ($) {
if ($_[0] !~ /^\s*$/) {
	print STDERR "$_[0]\n";
    }
    &usage();
    die;
} #dieusage
